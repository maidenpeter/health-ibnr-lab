# Deploys the app to shinyapps.io. Run from the repo root after scripts/build_results.R.
# Link the account once first (token and secret from the shinyapps.io dashboard, Account, Tokens):
#   rsconnect::setAccountInfo(name = "<account>", token = "<token>", secret = "<secret>")
# Then: Sys.setenv(SHINYAPPS_ACCOUNT = "<account>"); source("scripts/deploy.R")
# The bundle is the app, R/, config.yml, the result files and www/. Tests, scripts and claims.csv stay out.

account <- Sys.getenv("SHINYAPPS_ACCOUNT")
if (!nzchar(account)) stop("Set SHINYAPPS_ACCOUNT to the shinyapps.io account name first.")
files <- c("app.R", "config.yml", list.files("R", full.names = TRUE), list.files("www", full.names = TRUE),
           "data/results.rds", "data/headline.json", "data/test_results.csv")
rsconnect::deployApp(appDir = ".", appFiles = files, appName = "health-ibnr-lab", appTitle = "Health IBNR Lab",
                     account = account, server = "shinyapps.io", forceUpdate = TRUE)
