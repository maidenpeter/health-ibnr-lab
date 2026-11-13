# Illustrative IFRS 17 view of the liability for incurred claims. Discussion, not an implementation.

# Spot rate at term t (years): linear between the config terms, flat beyond either end.
spot_rate <- function(t, cfg) {
  stats::approx(unlist(cfg$ifrs17$curve_terms_years), unlist(cfg$ifrs17$curve_spot_rates),
                xout = t, rule = 2)$y
}

# Payments are assumed to fall mid-month.
discount_factors <- function(n_months, cfg) {
  t <- (seq_len(n_months) - 0.5) / 12
  (1 + spot_rate(t, cfg))^(-t)
}

# Risk adjustment: percentile of the bootstrap reserve distribution minus its mean.
risk_adjustment <- function(boot_totals, confidence) {
  as.numeric(stats::quantile(boot_totals, confidence, type = 7)) - mean(boot_totals)
}

lic_view <- function(cf_by_month, boot_totals, confidence, cfg) {
  df <- discount_factors(length(cf_by_month), cfg)
  be <- sum(cf_by_month)
  pv <- sum(cf_by_month * df)
  ra <- risk_adjustment(boot_totals, confidence)
  ratio <- pv / be
  list(
    best_estimate = be, present_value = pv, discount_effect = be - pv,
    ra_undiscounted = ra, ra_discounted = ra * ratio,
    lic_discounted = pv + ra * ratio, lic_undiscounted = be + ra,
    confidence = confidence, pv_ratio = ratio
  )
}
