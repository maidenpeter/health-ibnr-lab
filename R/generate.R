# Seeded synthetic health claims and earned premium. Every parameter comes from config.yml.

sample_lag <- function(n, probs) {
  sample.int(length(probs), n, replace = TRUE, prob = probs) - 1L
}

exposure_table <- function(cfg) {
  n <- cfg$n_accident_months
  schemes <- names(cfg$members$start)
  ex <- data.table::CJ(acc_idx = seq_len(n), scheme_type = schemes, sorted = FALSE)
  ex[, members := unlist(cfg$members$start[scheme_type]) * (1 + cfg$members$monthly_growth)^(acc_idx - 1)]
  ex[, pmpm := unlist(cfg$premium$pmpm_kes[scheme_type]) * (1 + cfg$premium$annual_rate_increase)^((acc_idx - 1) / 12)]
  ex[, earned_premium_kes := members * pmpm]
  ex[, accident_month := index_to_month(acc_idx, cfg$first_accident_month)]
  ex[]
}

generate_claims <- function(cfg) {
  set.seed(cfg$seed)
  n <- cfg$n_accident_months
  benefits <- names(cfg$frequency$benefit_mix)
  ex <- exposure_table(cfg)
  g <- ex[, .(benefit_type = benefits), by = .(acc_idx, scheme_type, members, accident_month)]
  mon <- tolower(month.abb[as.POSIXlt(g$accident_month)$mon + 1L])
  g[, lambda := members * cfg$frequency$claims_per_member_month *
      unlist(cfg$frequency$scheme_relativity[scheme_type]) *
      unlist(cfg$frequency$benefit_mix[benefit_type]) *
      unlist(cfg$seasonality[mon])]
  g[, n_claims := rpois(.N, lambda)]
  cl <- g[rep(seq_len(.N), n_claims), .(acc_idx, accident_month, scheme_type, benefit_type)]
  data.table::setorder(cl, acc_idx, benefit_type, scheme_type)

  cl[, report_lag := 0L]
  cl[, pay_lag := 0L]
  for (b in benefits) {
    k <- which(cl$benefit_type == b)
    cl$report_lag[k] <- sample_lag(length(k), unlist(cfg$report_lag_probs[[b]]))
    cl$pay_lag[k] <- sample_lag(length(k), unlist(cfg$payment_lag_probs[[b]]))
  }
  cl[, total_lag := pmin(report_lag + pay_lag, cfg$max_payment_lag_months)]

  sv <- cfg$severity
  trend <- (1 + sv$medical_inflation)^((cl$acc_idx - 1) / 12)
  med <- unlist(sv$median_kes[cl$benefit_type])
  sdl <- unlist(sv$sdlog[cl$benefit_type])
  billed <- rlnorm(nrow(cl), log(med), sdl)
  large <- cl$benefit_type == "inpatient" & runif(nrow(cl)) < sv$large_claim_prob_inpatient
  billed[large] <- rlnorm(sum(large), log(sv$large_claim_median_kes), sv$large_claim_sdlog)
  cl[, amount_billed_kes := round(billed * trend)]
  cl[, large_claim := large]

  ratio <- unlist(cfg$settlement_ratio[cl$benefit_type]) + rnorm(nrow(cl), 0, cfg$settlement_noise_sd)
  ratio <- pmin(pmax(ratio, 0.5), 1)
  cl[, amount_paid_kes := round(amount_billed_kes * ratio)]

  cl[, report_idx := acc_idx + report_lag]
  cl[, pay_idx := acc_idx + total_lag]
  cl[, report_month := index_to_month(report_idx, cfg$first_accident_month)]
  cl[, payment_month := index_to_month(pay_idx, cfg$first_accident_month)]
  cl[, claim_id := sprintf("C%06d", seq_len(.N))]
  cl[, .(claim_id, accident_month, report_month, payment_month, benefit_type, scheme_type,
         amount_billed_kes, amount_paid_kes, large_claim, acc_idx, report_idx, pay_idx)]
}
