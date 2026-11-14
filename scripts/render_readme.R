# Fills README.md from scripts/readme_template.md and draws the README figure.
# Every number comes from data/headline.json and data/test_results.csv; the figure is drawn from data/results.rds.
# Run from the repo root: source("scripts/render_readme.R"). scripts/build_results.R calls it at the end.

suppressPackageStartupMessages(library(ggplot2))
source("R/headline.R")
source("R/ui_helpers.R")
res <- readRDS("data/results.rds")
H <- jsonlite::read_json("data/headline.json", simplifyVector = TRUE)
tests <- utils::read.csv("data/test_results.csv")
dir.create("assets/readme", recursive = TRUE, showWarnings = FALSE)

# Figure: every slowdown scenario in the grid, for each method ---------------
g <- res$grid_summary
ord <- c("cl_all", "cl_excl", "bf_paid", "received_cl")
g <- g[g$method %in% ord, ]
g$panel <- factor(g$label, levels = g$label[match(ord, g$method)])
lim <- max(abs(g$error_pct))
g$txt <- fmt_pct(g$error_pct)
default_cell <- g[g$n_months == H$backlog$n_months & abs(g$pct - H$backlog$pct) < 1e-9, ]
fig <- ggplot(g, aes(factor(pct), factor(n_months))) +
  geom_tile(aes(fill = error_pct), colour = COL$bg, linewidth = 1.2) +
  geom_tile(data = default_cell, fill = NA, colour = COL$ink, linewidth = 1) +
  geom_text(aes(label = txt), size = 3.1, colour = COL$ink) +
  facet_wrap(~panel, ncol = 2) +
  scale_fill_gradient2(low = COL$amber, mid = COL$neutral, high = COL$navy, midpoint = 0, limits = c(-lim, lim),
                       labels = function(x) fmt_pct(x, 0), name = "Error against the true outstanding") +
  scale_x_discrete(labels = function(x) fmt_pct(as.numeric(x), 0)) +
  labs(x = "Share of due payments held back", y = "Months slowed before valuation") +
  theme_lab(13) +
  theme(panel.grid.major = element_blank(), axis.line.x = element_blank(),
        strip.text = element_text(colour = COL$ink, face = "bold", hjust = 0, size = 12, margin = margin(b = 6)),
        panel.spacing = unit(1.4, "lines"), legend.position = "bottom", legend.justification = "center",
        legend.title = element_text(colour = COL$ink2, size = 11)) +
  guides(fill = guide_colourbar(title.position = "top", barwidth = 16, barheight = 0.7))
ggsave("assets/readme/backlog_grid.png", fig, width = 10, height = 8.6, dpi = 150, bg = COL$bg)

# README ---------------------------------------------------------------------
m <- H$backlog$methods
sp <- H$backlog$shortfall_parts_kes
gr <- H$backlog$grid
month_long <- function(i) format(res$months[i], "%B %Y")
gap <- function(x) paste(fmt_pct(abs(x)), if (x < 0) "short" else "over")
fill <- c(
  r_ver = sub("^R version ([0-9.]+).*$", "\\1", res$r_version), cl_ver = res$chainladder_version,
  n_claims = formatC(H$n_claims, big.mark = ","), n_acc = H$n_accident_months,
  first_month = month_long(1), last_month = month_long(H$n_accident_months),
  valuation_date = format(as.Date(H$valuation_date), "%d %B %Y"),
  be = fmt_kes(H$best_estimate_outstanding_kes), be_m = fmt_kes_m(H$best_estimate_outstanding_kes),
  pipeline = fmt_kes(H$pipeline_kes),
  mack_cv = fmt_pct(H$mack_cv), mack_se = fmt_kes(H$mack_se_kes),
  sentence = H$backlog$sentence, received_err = fmt_pct(abs(m$received_cl$error_pct)),
  bf_elr = fmt_pct(H$bf_elr, 0), true_lr = fmt_pct(H$true_loss_ratio), bf_latest = res$cfg$methods$bf_latest_months,
  cdf1 = formatC(H$newest_month_cdf, format = "f", digits = 0),
  boot_r = formatC(H$boot_replicates, big.mark = ","), boot_distr = res$cfg$bootstrap$process_distr,
  boot_vs_cl = fmt_pct(abs(H$boot_mean_kes / H$cl_outstanding_kes - 1)),
  p75 = fmt_kes(H$boot_p75_kes), p995 = fmt_kes(H$boot_p995_kes), p995_m = fmt_kes_m(H$boot_p995_kes),
  pv = fmt_kes(H$ifrs17$present_value_kes), ra = fmt_kes(H$ifrs17$ra_kes), lic = fmt_kes(H$ifrs17$lic_kes),
  discount = fmt_kes(H$ifrs17$discount_effect_kes), ra_conf = fmt_pct(H$ifrs17$confidence),
  bl_n = H$backlog$n_months, bl_x = fmt_pct(H$backlog$pct, 0), delayed = fmt_kes(H$backlog$delayed_kes),
  err_cl_all = fmt_pct(m$cl_all$error_pct), err_cl_excl = fmt_pct(m$cl_excl$error_pct),
  err_received_cl = fmt_pct(m$received_cl$error_pct), err_bf_paid = fmt_pct(m$bf_paid$error_pct),
  shortfall = fmt_kes(-H$backlog$cl_error_kes), missing = fmt_kes(-sp$missing_payments),
  multiplied = fmt_kes(-sp$multiplied), factors = fmt_kes(-sp$factors),
  bt_cl_backlog = fmt_pct(H$backlog$backtest_ae$backlog$cl_all), bt_cl_base = fmt_pct(H$backlog$backtest_ae$base$cl_all),
  bt_rec_backlog = fmt_pct(H$backlog$backtest_ae$backlog$received_cl),
  pipe_backlog = formatC(H$backlog$pipeline_months_at_valuation$backlog, format = "f", digits = 1),
  pipe_base = formatC(H$backlog$pipeline_months_at_valuation$base, format = "f", digits = 1),
  base_cl_err = gap(H$base_cl_error_pct),
  n_scen = gr$n_scenarios, n_min = gr$n_months_range[1], n_max = gr$n_months_range[2],
  x_min = fmt_pct(gr$pct_range[1], 0), x_max = fmt_pct(gr$pct_range[2], 0),
  corner_cl_all = gap(gr$corner_error_pct$cl_all), corner_cl_excl = gap(gr$corner_error_pct$cl_excl),
  corner_bf_paid = gap(gr$corner_error_pct$bf_paid),
  rec_min = fmt_pct(gr$received_cl_error_pct_range[1]), rec_max = fmt_pct(gr$received_cl_error_pct_range[2]),
  tests_passed = sum(tests$result == "passed"), tests_total = nrow(tests)
)
readme <- readLines("scripts/readme_template.md", encoding = "UTF-8")
for (k in names(fill)) readme <- gsub(paste0("{{", k, "}}"), fill[[k]], readme, fixed = TRUE)
if (any(grepl("{{", readme, fixed = TRUE))) stop("README placeholder left unfilled: ", grep("{{", readme, value = TRUE, fixed = TRUE)[1])
writeLines(readme, "README.md", useBytes = TRUE)
message("README.md and assets/readme/backlog_grid.png written")
