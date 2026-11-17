# One free run on uploaded data per network address.
# The address itself is never stored. The app keeps a salted SHA-256 of it and the date of the run.
# Permanent store: with TRIAL_GITHUB_REPO, TRIAL_GITHUB_TOKEN and TRIAL_SALT set (scripts/setup_trial_store.R
# writes them to .Renviron, which is git-ignored and deployed with the app), each used address becomes one small
# file in a private GitHub repo, so the limit survives restarts and redeploys.
# Fallback: without those settings, or when GitHub cannot be reached, a folder in the R session's temp
# directory is used. It is forgotten when the app restarts.

# Address checks ------------------------------------------------------------------

ipv4_parts <- function(a) {
  if (!grepl("^[0-9]{1,3}([.][0-9]{1,3}){3}$", a)) return(NULL)
  p <- as.integer(strsplit(a, ".", fixed = TRUE)[[1]])
  if (any(p > 255)) NULL else p
}

is_public_ipv4 <- function(p) {
  !(p[1] %in% c(0, 10, 127) || (p[1] == 172 && p[2] >= 16 && p[2] <= 31) || (p[1] == 192 && p[2] == 168) ||
      (p[1] == 169 && p[2] == 254) || (p[1] == 100 && p[2] >= 64 && p[2] <= 127) || p[1] >= 224)
}

# Eight 16-bit groups of an IPv6 address, or NULL if it is not one.
ipv6_groups <- function(a) {
  a <- tolower(a)
  if (!grepl("^[0-9a-f:]+$", a) || !grepl(":", a)) return(NULL)
  halves <- strsplit(a, "::", fixed = TRUE)[[1]]
  if (length(halves) > 2 || length(gregexpr("::", a, fixed = TRUE)[[1]]) > 1) return(NULL)
  split <- function(s) if (nzchar(s)) strsplit(s, ":", fixed = TRUE)[[1]] else character(0)
  left <- split(halves[1]); right <- if (length(halves) == 2) split(halves[2]) else character(0)
  if (grepl("::", a, fixed = TRUE)) {
    fill <- 8 - length(left) - length(right)
    if (fill < 1) return(NULL)
    g <- c(left, rep("0", fill), right)
  } else g <- left
  if (length(g) != 8 || any(nchar(g) > 4) || any(!nzchar(g))) return(NULL)
  strtoi(g, 16L)
}

is_public_ipv6 <- function(g) {
  !(all(g == 0) || (all(g[1:7] == 0) && g[8] == 1) || bitwAnd(g[1], 0xffc0) == 0xfe80 || bitwAnd(g[1], 0xfe00) == 0xfc00 ||
      bitwAnd(g[1], 0xff00) == 0xff00)
}

# The key the trial is tied to: the IPv4 address, or the /64 network of an IPv6 address (devices rotate the rest).
# NA for anything that is not a public address.
address_key <- function(a) {
  if (is.null(a) || is.na(a) || !nzchar(a)) return(NA_character_)
  a <- trimws(a)
  a <- sub("^\\[([^]]+)\\](:[0-9]+)?$", "\\1", a)                  # [v6]:port
  if (grepl("^[0-9.]+:[0-9]+$", a)) a <- sub(":[0-9]+$", "", a)    # v4:port
  a <- sub("%.*$", "", a)                                           # v6 zone id
  if (grepl("^::ffff:[0-9.]+$", tolower(a))) a <- sub("^::ffff:", "", tolower(a))
  p <- ipv4_parts(a)
  if (!is.null(p)) return(if (is_public_ipv4(p)) paste0("v4:", paste(p, collapse = ".")) else NA_character_)
  g <- ipv6_groups(a)
  if (!is.null(g)) return(if (is_public_ipv6(g)) paste0("v6:", paste(sprintf("%x", g[1:4]), collapse = ":"), "::/64") else NA_character_)
  NA_character_
}

# shinyapps.io puts the visitor first in X-Forwarded-For and appends its own private proxies. A visitor can
# prepend made-up entries but cannot change what the edge proxy appends, so the rightmost public entry is used.
client_address <- function(req) {
  xff <- req$HTTP_X_FORWARDED_FOR
  cands <- if (!is.null(xff)) rev(trimws(strsplit(xff, ",", fixed = TRUE)[[1]])) else character(0)
  cands <- c(cands, req$HTTP_X_REAL_IP, req$REMOTE_ADDR)
  for (a in cands) { k <- address_key(a); if (!is.na(k)) return(k) }
  NA_character_
}

trial_hash <- function(key, salt) digest::digest(paste0(salt, "|", key), algo = "sha256", serialize = FALSE)

# Store ------------------------------------------------------------------------------

github_http <- function(method, url, token, body = NULL) {
  h <- curl::new_handle()
  curl::handle_setheaders(h, Authorization = paste("Bearer", token), Accept = "application/vnd.github+json",
                          `X-GitHub-Api-Version` = "2022-11-28", `User-Agent` = "health-ibnr-lab",
                          `Content-Type` = "application/json")
  curl::handle_setopt(h, customrequest = method, timeout = 10, connecttimeout = 5)
  if (!is.null(body)) curl::handle_setopt(h, postfields = body)
  r <- curl::curl_fetch_memory(url, handle = h)
  list(status = r$status_code, body = rawToChar(r$content))
}

# env: settings (defaults to the process environment). http: replaceable for tests.
trial_store <- function(env = Sys.getenv(c("TRIAL_GITHUB_REPO", "TRIAL_GITHUB_TOKEN", "TRIAL_SALT")),
                        dir = file.path(tempdir(), "trials"), http = github_http) {
  repo <- unname(env[1]); token <- unname(env[2]); salt <- unname(env[3])
  github <- nzchar(repo) && nzchar(token) && nzchar(salt)
  if (!nzchar(salt)) salt <- digest::digest(paste(Sys.time(), Sys.getpid(), stats::runif(1)), algo = "sha256", serialize = FALSE)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  url <- function(h) sprintf("https://api.github.com/repos/%s/contents/trials/%s.json", repo, h)
  local_file <- function(h) file.path(dir, paste0(h, ".json"))
  local_used <- function(h) {
    f <- local_file(h)
    if (file.exists(f)) list(used = TRUE, date = jsonlite::read_json(f)$first_run, via = "local") else list(used = FALSE, date = NA, via = "local")
  }
  local_claim <- function(h, date) {
    f <- local_file(h)
    if (file.exists(f)) return(FALSE)
    jsonlite::write_json(list(first_run = date), f, auto_unbox = TRUE)
    TRUE
  }
  warn <- function(...) message("trial store: ", ...)

  used <- function(h) {
    loc <- local_used(h)
    if (!github || loc$used) return(loc)
    r <- tryCatch(http("GET", url(h), token), error = function(e) list(status = -1, body = conditionMessage(e)))
    if (r$status == 200) {
      b <- jsonlite::fromJSON(r$body)
      date <- tryCatch(jsonlite::fromJSON(rawToChar(jsonlite::base64_dec(gsub("\\s", "", b$content))))$first_run,
                       error = function(e) NA)
      local_claim(h, date)
      return(list(used = TRUE, date = date, via = "github"))
    }
    if (r$status == 404) return(list(used = FALSE, date = NA, via = "github"))
    warn("GitHub lookup failed with status ", r$status, "; using the local store")
    loc
  }
  # TRUE if this call took the trial, FALSE if it had already gone.
  claim <- function(h, date = format(now_eat(), "%Y-%m-%d")) {
    if (!local_claim(h, date)) return(FALSE)
    if (!github) return(TRUE)
    body <- jsonlite::toJSON(list(message = "Trial used", content = jsonlite::base64_enc(
      jsonlite::toJSON(list(first_run = date), auto_unbox = TRUE))), auto_unbox = TRUE)
    r <- tryCatch(http("PUT", url(h), token, body), error = function(e) list(status = -1, body = conditionMessage(e)))
    if (r$status %in% c(200, 201)) return(TRUE)
    if (r$status == 422) return(FALSE)  # the file exists: used already, from another session
    warn("GitHub write failed with status ", r$status, "; the trial is kept in the local store only")
    TRUE
  }
  list(kind = if (github) "github" else "local", salt = salt, used = used, claim = claim)
}
