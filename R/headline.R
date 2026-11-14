# Every number the README and the Book page headline quote, taken from results.rds.
# The test suite rebuilds this from results.rds and checks it against data/headline.json.

fmt_kes <- function(x, digits = 0) paste0("KES ", formatC(round(x, digits), format = "f", digits = digits, big.mark = ","))
fmt_pct <- function(x, digits = 1) paste0(formatC(100 * x, format = "f", digits = digits), "%")
fmt_kes_m <- function(x, digits = 1) paste0("KES ", formatC(x / 1e6, format = "f", digits = digits, big.mark = ","), "m")

backlog_sentence <- function(n_months, pct, error_kes, error_pct) {
  verb <- if (error_kes < 0) "under-reserve" else "over-reserve"
  sprintf("A %d-month, %s payment slowdown makes chain ladder %s by %s (%s).",
          as.integer(n_months), fmt_pct(pct, 0), verb, fmt_kes(abs(error_kes)), fmt_pct(abs(error_pct)))
}

make_headline <- function(res) {
  b <- res$base
  d <- res$grid[[res$default_key]]
  tot <- d$totals
  m <- function(k) tot[tot$method == k, ]
  cl <- m("cl_all")
  base_s <- res$grid[[scenario_key(d$n_months, 0)]]
  base_bt <- base_s$backtest
  base_cl <- base_s$totals[base_s$totals$method == "cl_all", ]
  q <- stats::quantile(b$boot$totals, c(0.5, 0.75, 0.995), type = 7)
  lic <- b$lic_default
  meth <- lapply(split(tot, tot$method), function(r) list(
    label = r$label, outstanding_kes = round(r$outstanding), error_kes = round(r$error), error_pct = r$error_pct))
  list(
    valuation_date = format(res$cfg$valuation_date),
    n_claims = b$n_claims,
    n_accident_months = res$cfg$n_accident_months,
    paid_to_date_kes = round(b$paid_to_date),
    best_estimate_outstanding_kes = round(b$selected_total),
    cl_outstanding_kes = round(b$cl_total),
    bf_outstanding_kes = round(b$bf_total),
    pipeline_kes = round(b$pipeline_total),
    beyond_pipeline_kes = round(b$selected_total - b$pipeline_total),
    true_outstanding_kes = round(b$true_outstanding),
    base_cl_error_kes = round(base_cl$error),
    base_cl_error_pct = base_cl$error_pct,
    mack_se_kes = round(b$mack$total_se),
    mack_cv = b$mack$total_se / b$mack$total_reserve,
    boot_mean_kes = round(mean(b$boot$totals)),
    boot_sd_kes = round(stats::sd(b$boot$totals)),
    boot_p50_kes = round(q[[1]]), boot_p75_kes = round(q[[2]]), boot_p995_kes = round(q[[3]]),
    boot_replicates = length(b$boot$totals),
    months_of_claims = b$selected_total / b$avg_monthly_paid,
    newest_month_cdf = utils::tail(b$origin$cdf, 1),
    true_loss_ratio = b$true_loss_ratio,
    bf_elr = res$cfg$methods$bf_elr,
    backlog = list(
      n_months = d$n_months, pct = d$pct,
      delayed_claims = d$delayed_claims, delayed_kes = round(d$delayed_kes),
      cl_error_kes = round(cl$error), cl_error_pct = cl$error_pct,
      sentence = backlog_sentence(d$n_months, d$pct, cl$error, cl$error_pct),
      methods = meth,
      shortfall_parts_kes = as.list(round(d$cl_error_parts)),
      backtest_ae = list(
        base = as.list(stats::setNames(base_bt$ae, base_bt$method)),
        backlog = as.list(stats::setNames(d$backtest$ae, d$backtest$method))
      ),
      pipeline_months_at_valuation = list(
        base = utils::tail(base_s$pipeline$months_of_paid, 1),
        backlog = utils::tail(d$pipeline$months_of_paid, 1)
      ),
      ae_latest_month = list(
        base = utils::tail(base_s$ae$by_cal$ae, 1),
        backlog = utils::tail(d$ae$by_cal$ae, 1)
      ),
      grid = local({
        g <- res$grid_summary
        corner <- g[g$n_months == max(g$n_months) & g$pct == max(g$pct), ]
        list(
          n_scenarios = length(res$grid),
          n_months_range = range(g$n_months), pct_range = range(g$pct),
          corner_error_pct = as.list(stats::setNames(corner$error_pct, corner$method)),
          received_cl_error_pct_range = range(g$error_pct[g$method == "received_cl"])
        )
      })
    ),
    ifrs17 = list(
      confidence = lic$confidence,
      best_estimate_kes = round(lic$best_estimate), present_value_kes = round(lic$present_value),
      discount_effect_kes = round(lic$discount_effect), ra_kes = round(lic$ra_discounted),
      lic_kes = round(lic$lic_discounted), lic_undiscounted_kes = round(lic$lic_undiscounted)
    )
  )
}
