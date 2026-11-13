test_that("paid triangle reconciles to claim payments up to valuation", {
  P <- paid_triangle(claims, n_acc, n_acc, val)
  expect_equal(sum(P, na.rm = TRUE), sum(claims$amount_paid_kes[claims$pay_idx <= val]))
})

test_that("received triangle reconciles to claims reported up to valuation", {
  R <- received_triangle(claims, cfg, n_acc, n_acc, val)
  expect_equal(sum(R, na.rm = TRUE), sum(expected_received(claims, cfg)[claims$report_idx <= val]))
})

test_that("no payment after valuation leaks into any triangle", {
  P <- paid_triangle(claims, n_acc, n_acc, val)
  future <- outer(seq_len(n_acc), seq_len(n_acc), "+") - 1L > val
  expect_true(all(is.na(P[future])))
  late <- claims[pay_idx > val & report_idx > val]
  expect_gt(nrow(late), 0)
  expect_equal(sum(paid_triangle(late, n_acc, n_acc, val), na.rm = TRUE), 0)
  expect_equal(sum(received_triangle(late, cfg, n_acc, n_acc, val), na.rm = TRUE), 0)
  expect_equal(sum(res$base$triangles$all$paid, na.rm = TRUE), sum(claims$amount_paid_kes[claims$pay_idx <= val]))
})

test_that("payment lag never exceeds the cap", {
  expect_lte(max(claims$pay_idx - claims$acc_idx), cfg$max_payment_lag_months)
  expect_true(all(claims$pay_idx >= claims$report_idx))
})
