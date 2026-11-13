# Chain ladder, Bornhuetter-Ferguson, Mack and bootstrap wrappers.
# The chain ladder here is written out by hand so that factor choices (simple, volume weighted,
# excluded diagonals) are explicit. A test checks it against ChainLadder::MackChainLadder.

# Weight matrix that drops every link ratio ending in the latest n_excl calendar months.
# Link ratio (i, k) runs from development k to k + 1 and ends in calendar month i + k.
exclusion_weights <- function(cum, n_excl, val_idx) {
  w <- matrix(1, nrow(cum), ncol(cum), dimnames = dimnames(cum))
  if (n_excl > 0) w[outer(seq_len(nrow(cum)), seq_len(ncol(cum)), "+") >= val_idx - n_excl + 1L] <- 0
  w
}

# Age to age factors. average = "volume" (volume weighted) or "simple" (mean of link ratios).
cl_factors <- function(cum, weights = NULL, average = "volume") {
  n <- ncol(cum)
  if (is.null(weights)) weights <- matrix(1, nrow(cum), n)
  f <- numeric(n - 1)
  for (k in seq_len(n - 1)) {
    ok <- !is.na(cum[, k]) & !is.na(cum[, k + 1]) & weights[, k] > 0 & cum[, k] > 0
    if (!any(ok)) { f[k] <- 1; next }
    f[k] <- if (average == "simple") mean(cum[ok, k + 1] / cum[ok, k]) else sum(cum[ok, k + 1]) / sum(cum[ok, k])
  }
  f
}

# Cumulative development factor to ultimate from each development month.
cdf_from_factors <- function(f) c(rev(cumprod(rev(f))), 1)

# Link ratios C(k+1) / C(k) as a matrix, NA where not observed.
link_ratios <- function(cum) {
  n <- ncol(cum)
  lr <- cum[, -1, drop = FALSE] / cum[, -n, drop = FALSE]
  lr[!is.finite(lr)] <- NA
  dimnames(lr) <- list(origin = rownames(cum), dev = seq_len(n - 1))
  lr
}

chain_ladder <- function(cum, f) {
  latest <- latest_diagonal(cum)
  latest[is.na(latest)] <- 0
  dev <- latest_dev(cum)
  cdf <- cdf_from_factors(f)[pmax(dev, 1L)]
  data.frame(latest = latest, dev = dev, cdf = cdf, ultimate = latest * cdf, reserve = latest * (cdf - 1))
}

# BF: paid to date plus the prior ultimate times the share still to be paid.
bornhuetter_ferguson <- function(cum, f, premium, elr) {
  latest <- latest_diagonal(cum)
  latest[is.na(latest)] <- 0
  cdf <- cdf_from_factors(f)[pmax(latest_dev(cum), 1L)]
  reserve <- elr * premium * (1 - 1 / cdf)
  data.frame(latest = latest, cdf = cdf, prior_ultimate = elr * premium,
             ultimate = latest + reserve, reserve = reserve)
}

# Mack standard errors on the paid triangle, volume weighted, all diagonals.
mack_fit <- function(cum) {
  m <- suppressWarnings(ChainLadder::MackChainLadder(cum, est.sigma = "Mack"))
  s <- summary(m)
  list(
    by_origin = data.frame(ultimate = s$ByOrigin$Ultimate, reserve = s$ByOrigin$IBNR,
                           mack_se = s$ByOrigin$Mack.S.E, cv = s$ByOrigin$`CV(IBNR)`),
    total_reserve = s$Totals["IBNR:", 1],
    total_se = s$Totals["Mack S.E.:", 1],
    f = m$f
  )
}

bootstrap_fit <- function(cum, R, process_distr, seed) {
  b <- ChainLadder::BootChainLadder(cum, R = R, process.distr = process_distr, seed = seed)
  list(
    totals = as.numeric(b$IBNR.Totals),
    by_origin_mean = apply(b$IBNR.ByOrigin, 1, mean),
    n_nan = sum(!is.finite(b$IBNR.Totals))
  )
}

# Expected future paid by accident month and future calendar month, from the paid pattern.
# Each accident month's reserve is spread in proportion to the pattern's remaining increments.
future_cashflows <- function(cum, f, reserve, n_future) {
  pct <- 1 / cdf_from_factors(f)
  dev <- latest_dev(cum)
  out <- matrix(0, nrow(cum), n_future)
  for (i in seq_len(nrow(cum))) {
    if (reserve[i] <= 0) next
    future_dev <- (dev[i] + 1):(dev[i] + n_future)
    inc <- diff(c(pct[dev[i]], pct[pmin(future_dev, length(pct))]))
    if (sum(inc) <= 0) next
    out[i, ] <- reserve[i] * inc / sum(inc)
  }
  out
}
