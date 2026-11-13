test_that("every config value has a comment", {
  ct <- config_table(file.path(root, "config.yml"))
  expect_gt(nrow(ct), 50)
  expect_true(all(nzchar(trimws(ct$comment))), info = paste(ct$parameter[!nzchar(trimws(ct$comment))], collapse = ", "))
})

test_that("no em dashes or en dashes in the repo text files", {
  files <- list.files(root, pattern = "[.](R|md|yml|css|json|csv|txt)$", recursive = TRUE, full.names = TRUE)
  files <- files[!grepl("renv/|data/claims.csv", files)]
  dashes <- paste(intToUtf8(c(8212, 8211), multiple = TRUE), collapse = "|")
  bad <- files[vapply(files, function(f) any(grepl(dashes, readLines(f, warn = FALSE, encoding = "UTF-8"))), logical(1))]
  expect_equal(basename(bad), character(0))
})

test_that("the app carries the synthetic label", {
  src <- paste(c(readLines(file.path(root, "app.R")), readLines(file.path(root, "R", "ui_helpers.R"))), collapse = "\n")
  expect_match(src, "Synthetic portfolio. Illustrative only.", fixed = TRUE)
})
