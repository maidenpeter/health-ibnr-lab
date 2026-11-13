test_that("chain ladder on a fully developed synthetic triangle reproduces the known ultimate", {
  ult <- c(100, 120, 90, 150, 130, 110) * 1e6
  pattern <- c(0.40, 0.75, 0.90, 0.97, 1, 1)
  full <- outer(ult, pattern)
  cum <- full
  cum[outer(1:6, 1:6, "+") - 1L > 6] <- NA
  f <- cl_factors(cum)
  expect_equal(chain_ladder(cum, f)$ultimate, ult)
})

test_that("hand-written chain ladder matches ChainLadder::MackChainLadder", {
  P <- cumulate(res$base$triangles$all$paid)
  m <- suppressWarnings(ChainLadder::MackChainLadder(P, est.sigma = "Mack"))
  expect_equal(chain_ladder(P, cl_factors(P))$ultimate, unname(summary(m)$ByOrigin$Ultimate))
})

test_that("excluding diagonals matches MackChainLadder weights", {
  # Cut at the lag cap: with the full width, excluding diagonals empties the last columns and
  # MackChainLadder cannot fit them. Beyond the cap every factor is 1 either way.
  P <- cumulate(res$base$triangles$all$paid)[, seq_len(cfg$max_payment_lag_months + 1L)]
  w <- exclusion_weights(P, 3, val)
  m <- suppressWarnings(ChainLadder::MackChainLadder(P, weights = w, est.sigma = "Mack"))
  expect_equal(cl_factors(P, w), unname(m$f[seq_len(ncol(P) - 1)]))
  expect_false(isTRUE(all.equal(cl_factors(P, w), cl_factors(P))))
})

test_that("BF with ELR equal to the true loss ratio lands within 10% of the true outstanding", {
  o <- res$base$origin
  P <- cumulate(res$base$triangles$all$paid)
  bf <- bornhuetter_ferguson(P, res$base$f_all, o$premium, res$base$true_loss_ratio)
  true_os <- sum(o$true_ultimate) - sum(o$paid)
  expect_lt(abs(sum(bf$reserve) / true_os - 1), 0.10)
})

test_that("bootstrap mean is within 2% of the chain ladder reserve", {
  expect_lt(abs(mean(res$base$boot$totals) / res$base$cl_total - 1), 0.02)
  expect_equal(res$base$boot$n_nan, 0)
})
