# Deploys the app to shinyapps.io. Run from the repo root after scripts/build_results.R.
# Link the account once first (token and secret from the shinyapps.io dashboard, Account, Tokens):
#   rsconnect::setAccountInfo(name = "<account>", token = "<token>", secret = "<secret>")
# Then: Sys.setenv(SHINYAPPS_ACCOUNT = "<account>"); source("scripts/deploy.R")
# The bundle is the app, R/, config.yml, the result files, the sample files and www/. Tests, scripts and
# claims.csv stay out. .Renviron (git-ignored) goes with the app when it exists: it holds the settings of the
# permanent trial store, written by scripts/setup_trial_store.R.

account <- Sys.getenv("SHINYAPPS_ACCOUNT")
if (!nzchar(account)) stop("Set SHINYAPPS_ACCOUNT to the shinyapps.io account name first.")
files <- c("app.R", "config.yml", list.files("R", full.names = TRUE), list.files("www", full.names = TRUE),
           "data/results.rds", "data/headline.json", "data/test_results.csv",
           "data/sample_claims.csv", "data/sample_premium.csv")
if (file.exists(".Renviron")) {
  files <- c(files, ".Renviron")
  message("Sending .Renviron: the free run limit uses the permanent store.")
} else {
  message("No .Renviron: the free run limit is kept only while the app is awake. See scripts/setup_trial_store.R.")
}
rsconnect::deployApp(appDir = ".", appFiles = files, appName = "health-ibnr-lab", appTitle = "Health IBNR Lab",
                     account = account, server = "shinyapps.io", forceUpdate = TRUE)
