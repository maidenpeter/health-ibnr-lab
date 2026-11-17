sample_files <- list(
  claims = read_upload_csv(file.path(root, "data", "sample_claims.csv"), cfg$upload$max_rows),
  premium = read_upload_csv(file.path(root, "data", "sample_premium.csv"), cfg$upload$max_rows))

test_that("the sample file passes every check", {
  p <- prepare_upload(sample_files$claims, sample_files$premium, cfg, run_log())
  expect_true(p$ok)
  expect_equal(sum(p$checks$status == "Failed"), 0)
  expect_equal(p$n_acc, n_acc)
})

test_that("an upload of the sample reproduces the base case paid chain ladder", {
  p <- prepare_upload(sample_files$claims, sample_files$premium, cfg, run_log())
  r <- run_upload(p, cfg, run_log(), boot_R = 50)
  expect_equal(sum(r$cl$latest), res$base$paid_to_date)
  expect_equal(sum(r$cl$reserve), res$base$cl_total, tolerance = 1e-9)
})

test_that("payments after the chosen valuation month stay out of the uploaded triangle", {
  x <- sample_files$claims
  vk <- parse_month("2026-06")
  p <- prepare_upload(x, NULL, cfg, run_log(), valuation_key = vk)
  r <- run_upload(p, cfg, run_log(), boot_R = 50)
  acc <- parse_month(x$accident_month); pay <- parse_month(x$payment_month); amt <- parse_amount(x$amount_paid_kes)
  expect_equal(sum(r$cl$latest), sum(amt[!is.na(pay) & pay <= vk & acc <= vk]))
  expect_true(any(grepl("paid after Jun 2026", p$checks$detail)))
})

test_that("a file without a required column fails and names the column", {
  bad <- as.data.frame(sample_files$claims)
  bad$payment_month <- NULL
  p <- prepare_upload(bad, NULL, cfg, run_log())
  expect_false(p$ok)
  expect_match(p$checks$detail[p$checks$status == "Failed"], "payment_month")
})

test_that("unreadable months and amounts fail with their line numbers", {
  bad <- as.data.frame(sample_files$claims)
  bad$accident_month[3] <- "13/2024"
  bad$amount_paid_kes[10] <- "abc"
  p <- prepare_upload(bad, NULL, cfg, run_log())
  expect_false(p$ok)
  failed <- paste(p$checks$detail[p$checks$status == "Failed"], collapse = " ")
  expect_match(failed, "line 4", fixed = TRUE)
  expect_match(failed, "line 11", fixed = TRUE)
})

test_that("columns outside the template are dropped on arrival", {
  x <- as.data.frame(sample_files$claims)
  x$member_name <- "someone"
  lg <- run_log()
  p <- prepare_upload(x, NULL, cfg, lg)
  expect_true(p$ok)
  expect_false("member_name" %in% names(p$rows))
  expect_match(paste(lg$entries$detail, collapse = " "), "Dropped on arrival and not used: member_name", fixed = TRUE)
})

test_that("months and amounts are read in the accepted formats only", {
  k <- 2024L * 12L
  expect_equal(parse_month(c("2024-01", "2024-01-15", "2024/1", "2024-01-15 00:00:00", "15/01/2024", "2024-13", "")),
               c(k, k, k, k, NA, NA, NA))
  expect_equal(parse_amount(c("1,234", "KES 1,234.50", "Ksh 10", "abc")), c(1234, 1234.5, 10, NA))
})

test_that("the run log records the checks, every method and the disclaimer", {
  lg <- run_log()
  p <- prepare_upload(sample_files$claims, sample_files$premium, cfg, lg)
  run_upload(p, cfg, lg, boot_R = 50)
  txt <- paste(log_text(lg, CONTACT_EMAIL), collapse = "\n")
  for (pat in c("not been reviewed against real industry data", "Required columns", "Paid triangle", "Chain ladder",
                "Mack", "ODP bootstrap", "Received claims chain ladder", "Bornhuetter-Ferguson", CONTACT_EMAIL))
    expect_match(txt, pat, fixed = TRUE)
})
