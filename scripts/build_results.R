# Builds every result the app shows. Run from the repo root: source("scripts/build_results.R")
# Outputs: data/claims.csv, data/results.rds, data/headline.json, data/test_results.csv

suppressPackageStartupMessages({
  library(data.table)
  library(ChainLadder)
})
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)
dir.create("data", showWarnings = FALSE)

cfg <- read_config()
n_acc <- cfg$n_accident_months
val <- month_index(cfg$valuation_date, cfg$first_accident_month)
stopifnot(val == n_acc)
months <- index_to_month(seq_len(n_acc + cfg$max_payment_lag_months), cfg$first_accident_month)
message("Generating claims")

claims <- generate_claims(cfg)
fwrite(claims[, .(claim_id, accident_month, report_month, payment_month, benefit_type, scheme_type,
                  amount_billed_kes, amount_paid_kes)], "data/claims.csv")
expo <- exposure_table(cfg)
premium <- expo[, .(earned_premium_kes = sum(earned_premium_kes)), by = acc_idx][order(acc_idx)]$earned_premium_kes

# Base case ------------------------------------------------------------------
message("Base case")
benefits <- c("all", names(cfg$frequency$benefit_mix))
tri <- list()
for (b in benefits) {
  sub <- if (b == "all") claims else claims[benefit_type == b]
  tri[[b]] <- list(paid = paid_triangle(sub, n_acc, n_acc, val),
                   received = received_triangle(sub, cfg, n_acc, n_acc, val))
}
factor_tables <- list()
link_tables <- list()
for (b in benefits) for (kind in c("paid", "received")) {
  cum <- cumulate(tri[[b]][[kind]])
  key <- paste(kind, b, sep = "_")
  vol <- cl_factors(cum)
  factor_tables[[key]] <- data.frame(
    dev = seq_len(n_acc - 1),
    simple = cl_factors(cum, average = "simple"),
    volume = vol,
    volume_excl_latest = cl_factors(cum, exclusion_weights(cum, 1, val)),
    cdf_volume = cdf_from_factors(vol)[seq_len(n_acc - 1)]
  )
  link_tables[[key]] <- sweep(link_ratios(cum), 2, vol, "/")
}

P <- cumulate(tri$all$paid)
Rc <- cumulate(tri$all$received)
f_all <- cl_factors(P)
cl <- chain_ladder(P, f_all)
bf <- bornhuetter_ferguson(P, f_all, premium, cfg$methods$bf_elr)
truth <- true_ultimate(claims, n_acc)
pipe_origin <- pipeline_by_origin(claims, cfg, n_acc, val)
use_bf <- seq_len(n_acc) > n_acc - cfg$methods$bf_latest_months
selected_ult <- ifelse(use_bf, bf$ultimate, cl$ultimate)
selected_res <- selected_ult - cl$latest

message("Mack and bootstrap")
mack <- mack_fit(P)
stopifnot(isTRUE(all.equal(unname(mack$f[seq_along(f_all)]), f_all)))
boot <- bootstrap_fit(P, cfg$bootstrap$R, cfg$bootstrap$process_distr, cfg$bootstrap$seed)
if (boot$n_nan > 0) stop("Bootstrap returned non-finite totals; use the fallback in CLAUDE.md")

paid_cal <- paid_by_calendar(claims, seq_len(val))
avg_monthly_paid <- mean(tail(paid_cal, cfg$methods$months_of_claims_window))

cf <- future_cashflows(P, f_all, selected_res, cfg$max_payment_lag_months)
cf_by_month <- colSums(cf)
lic_default <- lic_view(cf_by_month, boot$totals, cfg$ifrs17$ra_confidence_default, cfg)

origin <- data.frame(
  acc_idx = seq_len(n_acc), accident_month = months[seq_len(n_acc)],
  premium = premium, paid = cl$latest, dev = cl$dev, cdf = cl$cdf,
  cl_ultimate = cl$ultimate, cl_reserve = cl$reserve,
  bf_ultimate = bf$ultimate, bf_reserve = bf$reserve,
  selected_method = ifelse(use_bf, "BF", "CL"), selected_ultimate = selected_ult, selected_reserve = selected_res,
  pipeline = pipe_origin, mack_se = mack$by_origin$mack_se,
  boot_mean_reserve = boot$by_origin_mean, true_ultimate = truth
)

base <- list(
  n_claims = nrow(claims), triangles = tri, factor_tables = factor_tables, link_tables = link_tables, f_all = f_all,
  members_at_valuation = sum(expo[acc_idx == val]$members),
  members_at_start = sum(expo[acc_idx == 1]$members),
  paid_by_benefit = vapply(benefits[-1], function(b) sum(tri[[b]]$paid, na.rm = TRUE), numeric(1)),
  origin = origin, mack = mack, boot = boot,
  paid_to_date = sum(cl$latest), cl_total = sum(cl$reserve), bf_total = sum(bf$reserve),
  selected_total = sum(selected_res), pipeline_total = sum(pipe_origin),
  true_outstanding = sum(truth) - sum(cl$latest), true_loss_ratio = sum(truth) / sum(premium),
  paid_by_calendar = data.frame(cal_idx = seq_len(val), month = months[seq_len(val)], paid = paid_cal),
  avg_monthly_paid = avg_monthly_paid,
  cashflows_by_month = data.frame(future_month = seq_along(cf_by_month), month = months[val + seq_along(cf_by_month)],
                                  cashflow = cf_by_month,
                                  discount_factor = discount_factors(length(cf_by_month), cfg)),
  lic_default = lic_default,
  max_observed_lag = max(claims$pay_idx - claims$acc_idx),
  benefit_summary = claims[, .(claims = .N, paid = sum(amount_paid_kes), large = sum(large_claim)), by = benefit_type],
  scheme_summary = claims[, .(claims = .N, paid = sum(amount_paid_kes)), by = scheme_type],
  premium_by_scheme = expo[, .(earned_premium_kes = sum(earned_premium_kes)), by = scheme_type]
)

# Backlog grid ---------------------------------------------------------------
message("Backlog grid")
draws <- backlog_draws(nrow(claims), cfg$backlog$seed)
base_ref <- list(cdf = cl$cdf, cl_ultimate = sum(cl$ultimate))
grid <- list()
for (n in unlist(cfg$backlog$n_months_grid)) for (x in unlist(cfg$backlog$pct_grid)) {
  grid[[scenario_key(n, x)]] <- run_scenario(claims, cfg, premium, n, x, draws, val, base_ref)
}
grid_summary <- rbindlist(lapply(grid, function(s) cbind(n_months = s$n_months, pct = s$pct, s$totals)))

res <- list(
  built_at = format(Sys.time(), "%Y-%m-%d %H:%M"),
  r_version = R.version.string,
  chainladder_version = as.character(packageVersion("ChainLadder")),
  cfg = cfg, config_table = config_table(), val_idx = val, months = months,
  base = base, grid = grid, grid_summary = as.data.frame(grid_summary),
  default_key = scenario_key(cfg$backlog$default_n_months, cfg$backlog$default_pct)
)
res$headline <- make_headline(res)
saveRDS(res, "data/results.rds", compress = "xz")
jsonlite::write_json(res$headline, "data/headline.json", auto_unbox = TRUE, pretty = TRUE, digits = NA)

# Tests ----------------------------------------------------------------------
message("Running tests")
tr <- as.data.frame(testthat::test_dir("tests/testthat", reporter = "silent", stop_on_failure = FALSE))
tests <- data.frame(
  file = tr$file, test = tr$test,
  result = ifelse(tr$failed > 0 | tr$error, "failed", ifelse(tr$skipped, "skipped", "passed")),
  expectations = tr$nb
)
fwrite(tests, "data/test_results.csv")
message(sprintf("Tests: %d passed, %d failed", sum(tests$result == "passed"), sum(tests$result == "failed")))

# README ---------------------------------------------------------------------
# Every number in the README comes from headline.json and test_results.csv.
H <- res$headline
m <- H$backlog$methods
sp <- H$backlog$shortfall_parts_kes
fill <- c(
  n_claims = formatC(H$n_claims, big.mark = ","), n_acc = H$n_accident_months,
  valuation_date = format(as.Date(H$valuation_date), "%d %B %Y"),
  be = fmt_kes(H$best_estimate_outstanding_kes), be_m = fmt_kes_m(H$best_estimate_outstanding_kes),
  shortfall_m = fmt_kes_m(-H$backlog$cl_error_kes), mack_cv = fmt_pct(H$mack_cv), mack_se = fmt_kes(H$mack_se_kes),
  sentence = H$backlog$sentence, received_err = fmt_pct(abs(m$received_cl$error_pct)),
  bf_elr = fmt_pct(H$bf_elr, 0), true_lr = fmt_pct(H$true_loss_ratio),
  cdf1 = formatC(H$newest_month_cdf, format = "f", digits = 0),
  boot_r = formatC(H$boot_replicates, big.mark = ","), p75 = fmt_kes(H$boot_p75_kes), p995 = fmt_kes(H$boot_p995_kes),
  pv = fmt_kes(H$ifrs17$present_value_kes), ra = fmt_kes(H$ifrs17$ra_kes), lic = fmt_kes(H$ifrs17$lic_kes),
  delayed = fmt_kes(H$backlog$delayed_kes),
  err_cl_all = fmt_pct(m$cl_all$error_pct), err_cl_excl = fmt_pct(m$cl_excl$error_pct),
  err_received_cl = fmt_pct(m$received_cl$error_pct), err_bf_paid = fmt_pct(m$bf_paid$error_pct),
  shortfall = fmt_kes(-H$backlog$cl_error_kes), missing = fmt_kes(-sp$missing_payments),
  multiplied = fmt_kes(-sp$multiplied), factors = fmt_kes(-sp$factors),
  bt_cl_backlog = fmt_pct(H$backlog$backtest_ae$backlog$cl_all), bt_cl_base = fmt_pct(H$backlog$backtest_ae$base$cl_all),
  bt_rec_backlog = fmt_pct(H$backlog$backtest_ae$backlog$received_cl),
  pipe_backlog = formatC(H$backlog$pipeline_months_at_valuation$backlog, format = "f", digits = 1),
  pipe_base = formatC(H$backlog$pipeline_months_at_valuation$base, format = "f", digits = 1),
  tests_passed = sum(tests$result == "passed"), tests_total = nrow(tests)
)
readme <- readLines("scripts/readme_template.md", encoding = "UTF-8")
for (k in names(fill)) readme <- gsub(paste0("{{", k, "}}"), fill[[k]], readme, fixed = TRUE)
if (any(grepl("{{", readme, fixed = TRUE))) stop("README placeholder left unfilled: ", grep("{{", readme, value = TRUE, fixed = TRUE)[1])
writeLines(readme, "README.md", useBytes = TRUE)
message(res$headline$backlog$sentence)
