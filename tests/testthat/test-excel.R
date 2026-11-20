# Downloads lead with Excel, and an Excel upload goes through exactly what a CSV does.

sample_files <- list(
  claims = read_upload_csv(file.path(root, "data", "sample_claims.csv"), cfg$upload$max_rows),
  premium = read_upload_csv(file.path(root, "data", "sample_premium.csv"), cfg$upload$max_rows))

test_that("an Excel copy of the sample reads and runs like the CSV", {
  f <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(list(Claims = sample_sheet(file.path(root, "data", "sample_claims.csv"))), f)
  x <- read_upload(f, "sample_claims.xlsx", cfg$upload$max_rows)
  expect_equal(nrow(x), nrow(sample_files$claims))
  p <- prepare_upload(x, sample_files$premium, cfg, run_log())
  expect_true(p$ok)
  r <- run_upload(p, cfg, run_log(), boot_R = 50)
  expect_equal(sum(r$cl$latest), res$base$paid_to_date)
})

test_that("months typed as Excel dates are read as their month", {
  f <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(data.frame(accident_month = as.Date(c("2024-01-15", "2024-02-01")), payment_month = c("2024-03", NA),
                                 Report_Month = as.POSIXct(c("2024-05-02 10:30:00", "2024-06-30 23:59:00"), tz = "UTC"),
                                 amount_paid_kes = c(1250.5, 99)), f)
  x <- read_upload(f, "dates.xlsx", 100)
  expect_equal(x$accident_month, c("2024-01-15", "2024-02-01"))
  expect_equal(x$Report_Month, c("2024-05-02", "2024-06-30"))
  expect_equal(parse_month(x$accident_month), parse_month(c("2024-01", "2024-02")))
  expect_true(is.na(x$payment_month[2]))
  expect_equal(parse_amount(x$amount_paid_kes), c(1250.5, 99))
})

test_that("the templates list every field, and the log is a web page", {
  t <- template_sheets(UPLOAD_CLAIM_FIELDS)
  expect_equal(names(t$Template), UPLOAD_CLAIM_FIELDS$field)
  expect_equal(nrow(t$Template), 0)
  f <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(t, f)
  expect_equal(readxl::excel_sheets(f), c("Template", "Fields"))
  h <- log_html(c("a < b & c", "second line"))
  expect_match(h, "^<!doctype html>")
  expect_match(h, "a &lt; b &amp; c", fixed = TRUE)
})
