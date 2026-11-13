test_that("backlog scenarios leave the true ultimate unchanged", {
  tu <- vapply(res$grid, function(s) s$true_ultimate_total, numeric(1))
  expect_true(all(tu == sum(res$base$origin$true_ultimate)))
})

test_that("a 0% slowdown reproduces the base case chain ladder", {
  s <- res$grid[[scenario_key(3, 0)]]$totals
  expect_equal(s$outstanding[s$method == "cl_all"], res$base$cl_total)
})

test_that("delayed claims are only those scheduled in the window, and pay after valuation", {
  draws <- backlog_draws(nrow(claims), cfg$backlog$seed)
  sc <- apply_backlog(claims, 3, 0.3, draws, cfg$backlog$catch_up_months, val)
  expect_true(all(claims$pay_idx[sc$delayed] %in% (val - 2):val))
  expect_true(all(sc$pay_idx[sc$delayed] %in% (val + 1):(val + cfg$backlog$catch_up_months)))
  expect_equal(sc$report_idx, claims$report_idx)
  expect_equal(sc$amount_paid_kes, claims$amount_paid_kes)
})

test_that("paid to date falls by exactly the delayed amount", {
  for (s in res$grid) expect_equal(s$paid_to_date + s$delayed_kes, res$base$paid_to_date)
})
