# Loads the project code and the built results once for every test file.
root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
suppressPackageStartupMessages(library(data.table))
for (f in list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE)) source(f)
cfg <- read_config(file.path(root, "config.yml"))
val <- month_index(cfg$valuation_date, cfg$first_accident_month)
n_acc <- cfg$n_accident_months
claims <- generate_claims(cfg)
res <- readRDS(file.path(root, "data", "results.rds"))
