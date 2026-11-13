test_that("headline.json matches a fresh recomputation from results.rds", {
  saved <- jsonlite::read_json(file.path(root, "data", "headline.json"), simplifyVector = TRUE)
  fresh <- jsonlite::fromJSON(jsonlite::toJSON(make_headline(res), auto_unbox = TRUE, digits = NA))
  expect_equal(saved, fresh)
})
