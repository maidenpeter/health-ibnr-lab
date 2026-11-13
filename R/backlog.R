# Payment slowdown scenarios, the four methods against the true ultimate, and the diagnostics.
# A slowdown only moves payment months. Report months and amounts never change, so the true
# ultimate is the same in every scenario and any gap between methods is method error.

backlog_draws <- function(n, seed) {
  set.seed(seed)
  list(u_delay = runif(n), u_catch = runif(n))
}

# Claims scheduled to pay in the last n_months up to and including valuation are delayed when
# their draw is below pct. Using one fixed draw per claim nests the scenarios: every claim delayed
# at 30% is also delayed at 40%.
apply_backlog <- function(claims, n_months, pct, draws, catch_up, val_idx) {
  out <- data.table::copy(claims)
  sched <- out$pay_idx
  hit <- sched > val_idx - n_months & sched <= val_idx & draws$u_delay < pct
  out$pay_idx[hit] <- val_idx + 1L + as.integer(floor(draws$u_catch[hit] * catch_up))
  out$delayed <- hit
  out
}

true_ultimate <- function(claims, n_acc) {
  out <- numeric(n_acc)
  s <- rowsum(claims$amount_paid_kes, claims$acc_idx)
  out[as.integer(rownames(s))] <- s[, 1]
  out
}

METHOD_LABELS <- c(
  cl_all = "Paid chain ladder, all diagonals",
  cl_excl = "Paid chain ladder, slowed diagonals excluded",
  received_cl = "Received claims chain ladder",
  bf_paid = "BF on paid, ELR prior"
)

# Ultimates by accident month for the four methods, at a given valuation month.
four_methods <- function(claims, cfg, premium, n_excl, val_idx, n_acc = cfg$n_accident_months) {
  P <- cumulate(paid_triangle(claims, n_acc, n_acc, val_idx))
  Rc <- cumulate(received_triangle(claims, cfg, n_acc, n_acc, val_idx))
  f_all <- cl_factors(P)
  f_excl <- cl_factors(P, exclusion_weights(P, n_excl, val_idx))
  f_rec <- cl_factors(Rc)
  paid <- latest_diagonal(P)
  ult <- cbind(
    cl_all = chain_ladder(P, f_all)$ultimate,
    cl_excl = chain_ladder(P, f_excl)$ultimate,
    received_cl = chain_ladder(Rc, f_rec)$ultimate,
    bf_paid = bornhuetter_ferguson(P, f_all, premium, cfg$methods$bf_elr)$ultimate
  )
  ult[is.na(ult)] <- 0
  paid[is.na(paid)] <- 0
  list(P = P, Rc = Rc, paid = paid, ult = ult, f_all = f_all, f_excl = f_excl, f_rec = f_rec)
}

# Actual incremental paid against expected. Expected paid at development month k is the received
# amount to date times the paid-to-received ratio seen at month k on older payments only (factors
# fitted at least ae_fit_gap_months before valuation). Received claims do not slow down, so the
# expectation does not fall with the payments it is checking.
paid_to_received_ratios <- function(P, Rc, gap, val_idx) {
  old <- outer(seq_len(nrow(P)), seq_len(ncol(P)), "+") - 1L <= val_idx - gap
  vapply(seq_len(ncol(P)), function(k) {
    ok <- old[, k] & !is.na(P[, k]) & !is.na(Rc[, k]) & Rc[, k] > 0
    if (any(ok)) sum(P[ok, k]) / sum(Rc[ok, k]) else NA_real_
  }, numeric(1))
}

ae_diagnostic <- function(P, Rc, cfg, val_idx) {
  r <- paid_to_received_ratios(P, Rc, cfg$diagnostics$ae_fit_gap_months, val_idx)
  inc <- decumulate(P)
  n_dev <- cfg$max_payment_lag_months + 1L
  first_cal <- val_idx - cfg$diagnostics$ae_show_months + 1L
  rows <- list()
  for (i in seq_len(nrow(P))) for (k in 1:n_dev) {
    cal <- i + k - 1L
    if (cal < first_cal || cal > val_idx) next
    expd <- r[k] * Rc[i, k] - if (k > 1) r[k - 1] * Rc[i, k - 1] else 0
    rows[[length(rows) + 1]] <- c(i, k, cal, inc[i, k], expd)
  }
  d <- as.data.frame(do.call(rbind, rows))
  names(d) <- c("acc_idx", "dev", "cal_idx", "actual", "expected")
  d$ae <- ifelse(d$expected > 0, d$actual / d$expected, NA)
  by_cal <- stats::aggregate(cbind(actual, expected) ~ cal_idx, d, sum)
  by_cal$ae <- by_cal$actual / by_cal$expected
  list(cells = d, by_cal = by_cal)
}

pipeline_trend <- function(claims, cfg, val_idx) {
  avg_n <- cfg$diagnostics$pipeline_avg_months
  months <- (avg_n + 12L):val_idx
  pipe <- pipeline_by_month(claims, cfg, months)
  paid_cal <- paid_by_calendar(claims, (months[1] - avg_n + 1L):val_idx)
  avg_paid <- vapply(seq_along(months), function(j) mean(paid_cal[j:(j + avg_n - 1L)]), numeric(1))
  data.frame(cal_idx = months, pipeline = pipe, avg_paid = avg_paid, months_of_paid = pipe / avg_paid)
}

# Hold out the latest diagonal, value at the month before, and project the held-out month.
# Each method's ultimate times the share of it that its paid pattern expects in the next month.
backtest <- function(claims, cfg, premium, n_excl, val_idx) {
  prev <- val_idx - 1L
  fm <- four_methods(claims, cfg, premium, max(n_excl - 1L, 0L), prev)
  pct_all <- 1 / cdf_from_factors(fm$f_all)
  pct_excl <- 1 / cdf_from_factors(fm$f_excl)
  dev <- latest_dev(fm$P)
  origins <- which(dev >= 1 & seq_len(nrow(fm$P)) <= prev)
  nxt <- function(pct, u) sum(u[origins] * (pct[pmin(dev[origins] + 1L, length(pct))] - pct[dev[origins]]))
  actual <- sum(claims$amount_paid_kes[claims$pay_idx == val_idx & claims$acc_idx <= prev])
  expd <- c(
    cl_all = nxt(pct_all, fm$ult[, "cl_all"]),
    cl_excl = nxt(pct_excl, fm$ult[, "cl_excl"]),
    received_cl = nxt(pct_all, fm$ult[, "received_cl"]),
    bf_paid = nxt(pct_all, fm$ult[, "bf_paid"])
  )
  data.frame(method = names(expd), label = METHOD_LABELS[names(expd)], expected = expd,
             actual = actual, ae = actual / expd, row.names = NULL)
}

# base: list with cdf (base case paid CDF by accident month) and cl_ultimate (base case CL total).
run_scenario <- function(claims, cfg, premium, n_months, pct, draws, val_idx, base) {
  n_acc <- cfg$n_accident_months
  sc <- apply_backlog(claims, n_months, pct, draws, cfg$backlog$catch_up_months, val_idx)
  fm <- four_methods(sc, cfg, premium, n_months, val_idx)
  truth <- true_ultimate(sc, n_acc)
  true_os <- sum(truth) - sum(fm$paid)
  use_bf <- seq_len(n_acc) > n_acc - cfg$methods$bf_latest_months
  selected <- ifelse(use_bf, fm$ult[, "bf_paid"], fm$ult[, "cl_all"])
  ult_tot <- c(colSums(fm$ult), selected = sum(selected))
  labels <- c(METHOD_LABELS, selected = sprintf("Selected basis (BF for the latest %d months, chain ladder before)",
                                                as.integer(cfg$methods$bf_latest_months)))
  totals <- data.frame(
    method = names(ult_tot), label = labels[names(ult_tot)],
    ultimate = ult_tot, true_ultimate = sum(truth),
    outstanding = ult_tot - sum(fm$paid), true_outstanding = true_os, row.names = NULL
  )
  totals$error <- totals$ultimate - totals$true_ultimate
  totals$error_pct <- totals$error / true_os

  # Where the paid chain ladder shortfall comes from.
  d <- numeric(n_acc)
  s <- rowsum(sc$amount_paid_kes[sc$delayed], sc$acc_idx[sc$delayed])
  d[as.integer(rownames(s))] <- s[, 1]
  cl_err <- totals$error[totals$method == "cl_all"]
  parts <- c(
    base_error = base$cl_ultimate - sum(truth),
    missing_payments = -sum(d),
    multiplied = -sum(d * (base$cdf - 1)),
    factors = 0
  )
  parts["factors"] <- cl_err - sum(parts)

  list(
    n_months = n_months, pct = pct,
    delayed_kes = sum(d), delayed_claims = sum(sc$delayed), delayed_by_origin = d,
    paid_to_date = sum(fm$paid), true_ultimate_total = sum(truth), truth_by_origin = truth,
    totals = totals, ult_by_origin = cbind(fm$ult, selected = selected), paid_by_origin = fm$paid,
    cl_error_parts = parts,
    ae = ae_diagnostic(fm$P, fm$Rc, cfg, val_idx),
    pipeline = pipeline_trend(sc, cfg, val_idx),
    backtest = backtest(sc, cfg, premium, n_months, val_idx)
  )
}

scenario_key <- function(n_months, pct) sprintf("n%d_x%02d", as.integer(n_months), as.integer(round(pct * 100)))
