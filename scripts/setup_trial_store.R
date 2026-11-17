# One-time setup for the permanent free run store (see R/trial_store.R).
# Before running it:
#   1. Create an empty private GitHub repo, for example maidenpeter/health-ibnr-lab-trials.
#   2. Create a fine-grained token at https://github.com/settings/personal-access-tokens/new with access to that
#      repo only and Repository permissions, Contents: Read and write.
# Then, in a terminal at the repo root (not in a chat):
#   Rscript scripts/setup_trial_store.R
# It asks for the repo and the token, checks the token can write to the repo, and writes .Renviron with the repo,
# the token and a random salt. .Renviron is git-ignored and scripts/deploy.R sends it with the app.
# Running it again keeps the salt, so addresses already stored stay recognised.

source("R/trial_store.R")
stdin_con <- file("stdin", "r")
ask <- function(prompt, default = "") {
  cat(prompt, if (nzchar(default)) sprintf(" [%s]", default), ": ", sep = "")
  x <- trimws(readLines(stdin_con, n = 1))
  if (!length(x) || !nzchar(x)) default else x
}

repo <- ask("Private repo for the store (owner/name)", "maidenpeter/health-ibnr-lab-trials")
token <- ask("Paste the GitHub token and press Enter")
if (!nzchar(token)) stop("No token given.")

api <- function(method, path, body = NULL) github_http(method, sprintf("https://api.github.com/repos/%s%s", repo, path), token, body)
r <- api("GET", "")
if (r$status != 200) stop(sprintf("The token cannot see %s (status %d). Check the repo name and the token's repository access.", repo, r$status))
if (!isTRUE(jsonlite::fromJSON(r$body)$private)) stop(sprintf("%s is public. The store must be a private repo.", repo))

# Write test: create a file, then delete it.
probe <- sprintf("/contents/trials/setup-check-%s.json", format(Sys.time(), "%Y%m%d%H%M%S"))
w <- api("PUT", probe, jsonlite::toJSON(list(message = "Setup check", content = jsonlite::base64_enc("{}")), auto_unbox = TRUE))
if (!w$status %in% c(200, 201)) stop(sprintf("The token cannot write to %s (status %d). Give it Contents: Read and write.", repo, w$status))
sha <- jsonlite::fromJSON(w$body)$content$sha
api("DELETE", probe, jsonlite::toJSON(list(message = "Setup check done", sha = sha), auto_unbox = TRUE))

lines <- if (file.exists(".Renviron")) readLines(".Renviron", warn = FALSE) else character(0)
get_val <- function(k) { m <- grep(paste0("^", k, "="), lines, value = TRUE); if (length(m)) sub(paste0("^", k, "="), "", m[1]) else "" }
salt <- get_val("TRIAL_SALT")
if (!nzchar(salt)) salt <- paste(as.character(openssl::rand_bytes(32)), collapse = "")
lines <- lines[!grepl("^TRIAL_(GITHUB_REPO|GITHUB_TOKEN|SALT)=", lines)]
lines <- c(lines, paste0("TRIAL_GITHUB_REPO=", repo), paste0("TRIAL_GITHUB_TOKEN=", token), paste0("TRIAL_SALT=", salt))
writeLines(lines, ".Renviron")
cat(sprintf("\nDone. The token can read and write %s, and .Renviron is written.\nRedeploy the app to switch the free run limit to the permanent store.\n", repo))
