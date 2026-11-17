test_that("the visitor is the rightmost public address in X-Forwarded-For", {
  expect_equal(client_address(list(HTTP_X_FORWARDED_FOR = "102.211.5.6, 172.16.0.1, 172.16.3.4", REMOTE_ADDR = "127.0.0.1")),
               "v4:102.211.5.6")
  # A made-up entry the visitor sends first is ignored.
  expect_equal(client_address(list(HTTP_X_FORWARDED_FOR = "8.8.8.8, 102.211.5.6, 172.16.0.1")), "v4:102.211.5.6")
  expect_true(is.na(client_address(list(HTTP_X_FORWARDED_FOR = "10.0.0.1, 192.168.1.1", REMOTE_ADDR = "127.0.0.1"))))
})

test_that("an IPv6 address shares one free run with the rest of its /64 network", {
  a <- address_key("2c0f:fe38:2101:1a2b:1111:2222:3333:4444")
  expect_equal(a, address_key("2c0f:fe38:2101:1a2b::9"))
  expect_equal(a, "v6:2c0f:fe38:2101:1a2b::/64")
  expect_true(is.na(address_key("fe80::1")))
  expect_true(is.na(address_key("::1")))
  expect_equal(address_key("::ffff:102.211.5.6"), "v4:102.211.5.6")
})

test_that("the local store gives one run per address and keeps only a hash", {
  d <- tempfile()
  st <- trial_store(env = c("", "", "a salt"), dir = d)
  h <- trial_hash("v4:102.211.5.6", st$salt)
  expect_equal(st$kind, "local")
  expect_false(st$used(h)$used)
  expect_true(st$claim(h, "2026-11-17"))
  expect_false(st$claim(h, "2026-11-17"))
  expect_true(st$used(h)$used)
  stored <- paste(c(list.files(d), unlist(lapply(list.files(d, full.names = TRUE), readLines))), collapse = " ")
  expect_false(grepl("102.211", stored, fixed = TRUE))
})

test_that("the GitHub store remembers a run across restarts, one file per hash", {
  repo_files <- list()
  fake_github <- function(method, url, token, body = NULL) {
    key <- sub(".*/contents/", "", url)
    if (method == "GET") {
      if (is.null(repo_files[[key]])) return(list(status = 404, body = "{}"))
      return(list(status = 200, body = jsonlite::toJSON(list(content = repo_files[[key]]), auto_unbox = TRUE)))
    }
    if (!is.null(repo_files[[key]])) return(list(status = 422, body = "{}"))
    repo_files[[key]] <<- jsonlite::fromJSON(body)$content
    list(status = 201, body = "{}")
  }
  first <- trial_store(env = c("owner/repo", "token", "a salt"), dir = tempfile(), http = fake_github)
  restarted <- trial_store(env = c("owner/repo", "token", "a salt"), dir = tempfile(), http = fake_github)
  h <- trial_hash("v4:102.211.5.6", "a salt")
  expect_equal(first$kind, "github")
  expect_true(first$claim(h, "2026-11-17"))
  u <- restarted$used(h)
  expect_true(u$used)
  expect_equal(u$date, "2026-11-17")
  expect_false(restarted$claim(h, "2026-11-18"))
  expect_equal(names(repo_files), paste0("trials/", h, ".json"))
})

test_that("when GitHub cannot be reached the store falls back to the local folder", {
  down <- function(...) stop("no network")
  st <- trial_store(env = c("owner/repo", "token", "a salt"), dir = tempfile(), http = down)
  h <- trial_hash("v4:102.211.5.6", "a salt")
  suppressMessages({
    expect_false(st$used(h)$used)
    expect_true(st$claim(h, "2026-11-17"))
  })
  expect_true(st$used(h)$used)
})
