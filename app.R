# Health IBNR Lab. Reserving a medical book, and what breaks when claims slow down.
# Every number on screen comes from data/results.rds and data/headline.json, built by
# scripts/build_results.R. The server only reads them; sliders pick from stored results.
# Shiny loads the files in R/ before this one.

library(shiny)
library(bslib)
library(ggplot2)
library(DT)

res <- readRDS("data/results.rds")
H <- jsonlite::read_json("data/headline.json")
tests <- utils::read.csv("data/test_results.csv", stringsAsFactors = FALSE)
cfg <- res$cfg
B <- res$base
O <- B$origin
val <- res$val_idx
n_acc <- cfg$n_accident_months
months <- res$months
acc_lab <- month_label(months[seq_len(n_acc)])
bf_n <- cfg$methods$bf_latest_months
latest_k <- which(seq_len(n_acc) > n_acc - bf_n)
val_label <- format(cfg$valuation_date, "%d %B %Y")
pct_grid <- unlist(cfg$backlog$pct_grid)

# Your data page: upload size, and the store that keeps the free run to one per network address.
# .Renviron (git-ignored, written by scripts/setup_trial_store.R) holds the store's settings when there are any.
options(shiny.maxRequestSize = cfg$upload$max_file_mb * 1024^2)
if (file.exists(".Renviron")) readRenviron(".Renviron")
TRIALS <- trial_store()
message("Trial store: ", TRIALS$kind)

# Book -----------------------------------------------------------------------

book_ui <- function() {
  latest_share <- sum(O$selected_reserve[latest_k]) / B$selected_total
  tagList(
    page_head(
      paste("Valuation", val_label), "What we owe on a medical book",
      sprintf(paste(
        "A synthetic health book: %s claims from about %s members on corporate, SME and individual cover,",
        "accident months %s to %s. Providers send invoices, the insurer vets them and pays. At valuation some claims",
        "have been paid, some invoices are in but not yet paid, and some have not arrived. This lab estimates what is",
        "still owed, how sure we can be, and what happens to the estimate when payments slow down."),
        formatC(B$n_claims, big.mark = ","), formatC(round(B$members_at_valuation, -2), format = "d", big.mark = ","),
        acc_lab[1], acc_lab[n_acc])
    ),
    div(class = "kpi-row",
      kpi("Best estimate outstanding claims", fmt_kes_m(H$best_estimate_outstanding_kes),
          sprintf("%s. Pipeline plus IBNR. Chain ladder, with BF for the latest %d accident months.",
                  fmt_kes(H$best_estimate_outstanding_kes), bf_n)),
      kpi("Mack coefficient of variation", fmt_pct(H$mack_cv),
          sprintf("Mack standard error %s on the chain ladder reserve.", fmt_kes_m(H$mack_se_kes))),
      kpi("Outstanding as months of claims", formatC(H$months_of_claims, format = "f", digits = 1),
          sprintf("Against average paid of %s a month over the last %d months.",
                  fmt_kes_m(B$avg_monthly_paid), as.integer(cfg$methods$months_of_claims_window))),
      kpi("The twist", H$backlog$sentence,
          tagList("Received claims chain ladder stays within ",
                  fmt_pct(abs(H$backlog$methods$received_cl$error_pct)), " of the truth. ",
                  actionLink("go_backlog", "See the Backlog page")), class = "teaser")
    ),
    div(class = "cta-strip",
      tags$span(tags$b("Have your own claims?"), " The same methods run on your data, with a log of every step you can download."),
      actionLink("go_yourdata", "Try the Your data page")),
    section("Paid to date and still to pay, by accident month",
      plotOutput("book_plot", height = "340px"),
      takeaway(sprintf(paste(
        "The latest %d accident months hold %s of the outstanding. Older months are close to fully paid:",
        "health claims here are settled within %d months of the accident month."),
        bf_n, fmt_pct(latest_share), B$max_observed_lag))
    ),
    section("The book",
      layout_columns(col_widths = breakpoints(sm = 12, lg = c(7, 5)),
        DTOutput("book_benefit", fill = FALSE), DTOutput("book_scheme", fill = FALSE)),
      takeaway(sprintf(paste(
        "Outpatient is the bulk of claim counts; inpatient is the bulk of the money. Earned premium over the",
        "36 months is %s, and the BF prior assumes %s of it becomes paid claims."),
        fmt_kes_m(sum(O$premium)), fmt_pct(cfg$methods$bf_elr, 0)))
    )
  )
}

book_server <- function(input, output, session) {
  observeEvent(input$go_backlog, nav_select("nav", "Backlog"))
  observeEvent(input$go_yourdata, nav_select("nav", "Your data"))
  output$book_plot <- renderPlot({
    d <- rbind(
      data.frame(month = months[seq_len(n_acc)], part = "Paid to date", kes = O$paid),
      data.frame(month = months[seq_len(n_acc)], part = "Still to pay (best estimate)", kes = O$selected_reserve)
    )
    d$part <- factor(d$part, c("Still to pay (best estimate)", "Paid to date"))
    nar <- is_narrow(session, "book_plot")
    ggplot(d, aes(month, kes, fill = part)) +
      geom_col(width = 24, colour = COL$bg, linewidth = 0.3) +
      scale_fill_manual(values = c("Paid to date" = COL$navy, "Still to pay (best estimate)" = COL$amber)) +
      scale_y_continuous(labels = kes_axis, expand = expansion(c(0, 0.04))) +
      scale_x_date(date_breaks = if (nar) "12 months" else "3 months", date_labels = "%b %y") +
      guides(fill = guide_legend(nrow = if (nar) 2 else 1)) +
      labs(x = "Accident month", y = "KES") + theme_lab()
  }, res = 96)
  output$book_benefit <- renderDT({
    bs <- B$benefit_summary
    bs <- bs[match(names(B$paid_by_benefit), bs$benefit_type), ]
    lab_dt(data.frame(
      `Benefit` = tools::toTitleCase(bs$benefit_type),
      `Claims` = formatC(bs$claims, big.mark = ","),
      `Share of claims` = fmt_pct(bs$claims / sum(bs$claims)),
      `Paid to valuation` = fmt_kes(B$paid_by_benefit),
      `Large claims` = bs$large, check.names = FALSE), dom = "t")
  })
  output$book_scheme <- renderDT({
    ps <- B$premium_by_scheme
    lab_dt(data.frame(
      `Scheme type` = c(corporate = "Corporate", sme = "SME", individual = "Individual")[ps$scheme_type],
      `Earned premium, 36 months` = fmt_kes(ps$earned_premium_kes),
      `Share` = fmt_pct(ps$earned_premium_kes / sum(ps$earned_premium_kes)), check.names = FALSE), dom = "t")
  })
}

# Triangles ------------------------------------------------------------------

tri_ui <- function() {
  tagList(
    page_head("Triangles", "Paid and received development",
      paste("Health claims carry no case reserves, so there is no incurred triangle. The received triangle stands in:",
            "invoices at the month they arrive, valued at the expected settlement ratio for their benefit type. It is",
            "observable at valuation and does not move when payments slow down.")),
    div(class = "controls", layout_columns(col_widths = breakpoints(sm = 12, md = c(4, 4, 4)),
      radioButtons("tri_kind", "Triangle", c("Paid" = "paid", "Received" = "received"), inline = TRUE),
      radioButtons("tri_view", "View", c("Cumulative" = "cum", "Incremental" = "inc"), inline = TRUE),
      selectInput("tri_benefit", "Benefit type", c("All benefits" = "all", "Outpatient" = "outpatient",
                  "Inpatient" = "inpatient", "Maternity" = "maternity", "Chronic" = "chronic"))
    )),
    section("Triangle, KES thousands, development months 1 to 24", DTOutput("tri_table", fill = FALSE), uiOutput("tri_take")),
    section("Development factors",
      layout_columns(col_widths = breakpoints(sm = 12, xl = c(5, 7)),
        DTOutput("tri_factors", fill = FALSE),
        tagList(plotOutput("tri_heat", height = "460px"), uiOutput("tri_heat_take"))))
  )
}

tri_server <- function(input, output, session) {
  key <- reactive(paste(input$tri_kind, input$tri_benefit, sep = "_"))
  output$tri_table <- renderDT({
    m <- B$triangles[[input$tri_benefit]][[input$tri_kind]]
    if (input$tri_view == "cum") m <- cumulate(m)
    m <- round(m[, 1:24] / 1000)
    df <- data.frame(`Accident month` = acc_lab, m, check.names = FALSE)
    names(df)[-1] <- 1:24
    DT::formatRound(DT::datatable(df, rownames = FALSE, class = "compact stripe", fillContainer = FALSE,
      options = list(dom = "t", paging = FALSE, scrollX = TRUE, scrollY = "430px", ordering = FALSE)),
      columns = 2:25, digits = 0)
  })
  output$tri_take <- renderUI({
    ft <- B$factor_tables[[key()]]
    first <- 1 / ft$cdf_volume[1]
    takeaway(if (input$tri_kind == "paid") sprintf(paste(
      "On average only %s of an accident month's paid claims are paid in the month itself, so the chain ladder",
      "multiplies the newest month by %s. Small changes in what gets paid early move the reserve a lot."),
      fmt_pct(first), formatC(ft$cdf_volume[1], format = "f", digits = 1))
    else sprintf(paste("%s of an accident month's claims are received in the month itself and %s by the end of",
      "the next month. Received claims develop faster than paid, so the received chain ladder leans less on the",
      "newest diagonal."), fmt_pct(first), fmt_pct(1 / ft$cdf_volume[2])))
  })
  output$tri_factors <- renderDT({
    ft <- B$factor_tables[[key()]][1:23, ]
    df <- data.frame(`Months` = paste(ft$dev, "to", ft$dev + 1), `Simple average` = ft$simple,
                     `Volume weighted` = ft$volume, `Volume, latest diagonal excluded` = ft$volume_excl_latest,
                     `To ultimate (volume)` = ft$cdf_volume, check.names = FALSE)
    DT::formatRound(DT::datatable(df, rownames = FALSE, class = "compact stripe", fillContainer = FALSE,
      options = list(dom = "t", paging = FALSE, scrollX = TRUE, scrollY = "400px", ordering = FALSE)),
      columns = 2:5, digits = 4)
  })
  output$tri_heat <- renderPlot({
    nd <- as.integer(cfg$max_payment_lag_months) + 1L
    lr <- B$link_tables[[key()]][, seq_len(nd)]
    d <- data.frame(acc = rep(seq_len(n_acc), nd), dev = rep(seq_len(nd), each = n_acc), rel = as.vector(lr))
    d <- d[!is.na(d$rel), ]
    nar <- is_narrow(session, "tri_heat")
    ggplot(d, aes(dev, factor(acc, levels = n_acc:1), fill = rel)) +
      geom_tile(colour = COL$bg, linewidth = 0.4) +
      scale_fill_gradient2(low = COL$navy, mid = COL$neutral, high = COL$amber, midpoint = 1,
                           limits = c(0.9, 1.1), breaks = c(0.9, 1, 1.1), labels = if (nar) c("-10%", "equal", "+10%") else c("10% below", "equal", "10% above"),
                           oob = scales::squish, name = "Link ratio against its column factor") +
      scale_y_discrete(labels = function(l) acc_lab[as.integer(l)], breaks = as.character(seq(1, n_acc, 3))) +
      scale_x_continuous(breaks = if (nar) seq(1, nd, 2) else seq_len(nd), expand = c(0, 0)) +
      labs(x = if (nar) "Development month" else "Development month (link from this month to the next)", y = NULL) +
      guides(fill = guide_colourbar(title.position = "top", barwidth = unit(if (nar) 10 else 14, "lines"), barheight = unit(0.6, "lines"))) +
      theme_lab() + theme(legend.title = element_text(size = 10, colour = COL$ink2), panel.grid = element_blank())
  }, res = 96)
  output$tri_heat_take <- renderUI({
    lr <- B$link_tables[[key()]][, 1:12]
    takeaway(sprintf(paste("Each cell is a link ratio divided by its column's volume weighted factor. %s of the",
      "link ratios in the first 12 development months sit within 5%% of their column factor. Links after month %d",
      "are not shown: every one of them is 1, because nothing is paid that late."),
      fmt_pct(mean(abs(lr - 1) <= 0.05, na.rm = TRUE)), as.integer(cfg$max_payment_lag_months) + 1L))
  })
}

# Reserves -------------------------------------------------------------------

res_ui <- function() {
  tagList(
    page_head("Reserves", "Chain ladder and Bornhuetter-Ferguson",
      sprintf(paste("Chain ladder takes the paid to date and develops it with volume weighted factors. BF takes the",
        "same pattern but replaces the paid to date in the young months with a prior: ELR times earned premium.",
        "The selected basis uses BF for the latest %d accident months, where chain ladder leans hardest on very",
        "little data, and chain ladder before that."), bf_n)),
    div(class = "controls", sliderInput("elr", "BF expected loss ratio (prior)", min = round(100 * cfg$methods$elr_slider_min),
        max = round(100 * cfg$methods$elr_slider_max), value = round(100 * cfg$methods$bf_elr), step = 1, post = "%", width = "100%")),
    uiOutput("res_kpis"),
    section("Ultimates for the latest 12 accident months", plotOutput("res_plot", height = "360px"), uiOutput("res_take")),
    section("Outstanding claims (pipeline plus IBNR), by accident month", uiOutput("res_split"), DTOutput("res_table", fill = FALSE))
  )
}

res_server <- function(input, output, session) {
  bf <- reactive({
    ult <- O$paid + (input$elr / 100) * O$premium * (1 - 1 / O$cdf)
    sel <- ifelse(seq_len(n_acc) %in% latest_k, ult, O$cl_ultimate)
    list(ult = ult, reserve = ult - O$paid, sel_ult = sel, sel_res = sel - O$paid)
  })
  output$res_kpis <- renderUI({
    b <- bf()
    div(class = "kpi-row",
      kpi("Chain ladder outstanding", fmt_kes_m(B$cl_total), fmt_kes(B$cl_total)),
      kpi("BF outstanding", fmt_kes_m(sum(b$reserve)), sprintf("At ELR %s.", fmt_pct((input$elr / 100), 0))),
      kpi("Selected outstanding", fmt_kes_m(sum(b$sel_res)), sprintf("BF for the latest %d months.", bf_n)),
      kpi("Mack standard error", fmt_kes_m(B$mack$total_se), sprintf("CV %s on the chain ladder reserve.", fmt_pct(H$mack_cv))))
  })
  output$res_plot <- renderPlot({
    k <- (n_acc - 11):n_acc
    b <- bf()
    pts <- rbind(
      data.frame(month = months[k], series = "Chain ladder", kes = O$cl_ultimate[k]),
      data.frame(month = months[k], series = "BF", kes = b$ult[k]),
      data.frame(month = months[k], series = "True ultimate (known only because the data is synthetic)", kes = O$true_ultimate[k]))
    lv <- c("Chain ladder", "BF", "True ultimate (known only because the data is synthetic)")
    nar <- is_narrow(session, "res_plot")
    leg_lab <- if (nar) wrap_lab(30) else ggplot2::waiver()
    pts$series <- factor(pts$series, lv)
    ggplot() +
      geom_col(data = data.frame(month = months[k], kes = O$paid[k]), aes(month, kes), fill = "#dcd9d0", width = 22) +
      geom_point(data = pts, aes(month, kes, colour = series, shape = series), size = 3.2, stroke = 1.2,
                 position = position_dodge(width = 14)) +
      scale_colour_manual(values = setNames(c(COL$amber, COL$navy, COL$ink), lv), labels = leg_lab) +
      scale_shape_manual(values = setNames(c(16, 17, 1), lv), labels = leg_lab) +
      scale_y_continuous(labels = kes_axis, limits = c(0, NA), expand = expansion(c(0, 0.05))) +
      scale_x_date(date_breaks = if (nar) "3 months" else "1 month", date_labels = "%b %y") +
      labs(x = "Accident month (grey bar is paid to date)", y = "KES") +
      theme_lab() + guides(colour = guide_legend(ncol = if (nar) 1 else 3))
  }, res = 96)
  output$res_take <- renderUI({
    b <- bf()
    takeaway(sprintf(paste("For the latest %d accident months, BF at ELR %s gives %s outstanding against chain",
      "ladder's %s. The true figure, which no reserving team would have, is %s."),
      bf_n, fmt_pct((input$elr / 100), 0), fmt_kes(sum(b$reserve[latest_k])), fmt_kes(sum(O$cl_reserve[latest_k])),
      fmt_kes(sum(O$true_ultimate[latest_k] - O$paid[latest_k]))))
  })
  output$res_split <- renderUI({
    b <- bf()
    below <- which(b$sel_res < O$pipeline)
    flag <- if (length(below)) sprintf(paste(" For %s the estimate is below the invoices already received. Chain ladder",
      "is not a floor; an analyst would hold at least the pipeline there, which adds %s."),
      paste(acc_lab[below], collapse = ", "), fmt_kes(sum(O$pipeline[below] - b$sel_res[below]))) else ""
    takeaway(sprintf(paste("Of the selected %s outstanding, %s is pipeline: invoices received but not yet paid.",
      "The other %s is claims not yet received, the IBNR in the narrow sense.%s"),
      fmt_kes(sum(b$sel_res)), fmt_kes(B$pipeline_total), fmt_kes(sum(b$sel_res) - B$pipeline_total), flag))
  })
  output$res_table <- renderDT({
    b <- bf()
    k <- n_acc:1
    df <- data.frame(
      `Accident month` = acc_lab[k], `Paid to date` = O$paid[k], `CL ultimate` = O$cl_ultimate[k],
      `BF ultimate` = b$ult[k], `Selected` = ifelse(k %in% latest_k, "BF", "CL"),
      `Selected ultimate` = b$sel_ult[k], `Outstanding` = b$sel_res[k], `Pipeline` = O$pipeline[k],
      `Not yet received` = b$sel_res[k] - O$pipeline[k], `Mack SE` = O$mack_se[k], check.names = FALSE)
    DT::formatRound(lab_dt(df, page_length = 12), columns = c(2:4, 6:10), digits = 0)
  })
}

# Uncertainty ----------------------------------------------------------------

unc_ui <- function() {
  q <- c(H$boot_p50_kes, H$boot_p75_kes, H$boot_p995_kes)
  tagList(
    page_head("Uncertainty", "How sure we can be",
      sprintf(paste("Two views of the spread around the chain ladder reserve. Mack gives a standard error from the",
        "variability of the link ratios. The over-dispersed Poisson bootstrap (England and Verrall) resamples the",
        "residuals %s times and adds process error, giving a full distribution of the reserve."),
        formatC(H$boot_replicates, big.mark = ","))),
    div(class = "kpi-row",
      kpi("Bootstrap mean", fmt_kes_m(H$boot_mean_kes), sprintf("Chain ladder reserve %s.", fmt_kes_m(H$cl_outstanding_kes))),
      kpi("50th percentile", fmt_kes_m(q[1]), fmt_kes(q[1])),
      kpi("75th percentile", fmt_kes_m(q[2]), fmt_kes(q[2])),
      kpi("99.5th percentile", fmt_kes_m(q[3]), fmt_kes(q[3]))),
    section("Bootstrap distribution of the outstanding claims", plotOutput("unc_plot", height = "360px"),
      takeaway(sprintf(paste("Half the simulated reserves fall below %s. One in two hundred is above %s, %s more",
        "than the mean. The bootstrap standard deviation is %s, against a Mack standard error of %s."),
        fmt_kes(q[1]), fmt_kes(q[3]), fmt_pct(q[3] / H$boot_mean_kes - 1), fmt_kes(H$boot_sd_kes), fmt_kes(H$mack_se_kes)))),
    note_box("How the bootstrap was run",
      tags$ul(
        tags$li(sprintf("ChainLadder::BootChainLadder on the full %d by %d paid triangle, %s replicates, %s process distribution, seed %s.",
          n_acc, n_acc, formatC(H$boot_replicates, big.mark = ","), cfg$bootstrap$process_distr, cfg$bootstrap$seed)),
        tags$li(sprintf(paste("Development columns after month %d hold no payments. The bootstrap ran on them without",
          "errors and returned %d non-finite totals, so neither fallback in the build plan was needed."),
          as.integer(cfg$max_payment_lag_months) + 1L, B$boot$n_nan)),
        tags$li(sprintf("The bootstrap mean is %s away from the chain ladder reserve.",
          fmt_pct(abs(H$boot_mean_kes / H$cl_outstanding_kes - 1)))),
        tags$li("This is the spread of the chain ladder reserve on this data. It does not cover a payment slowdown: see the Backlog page.")))
  )
}

unc_server <- function(input, output, session) {
  output$unc_plot <- renderPlot({
    q <- c(H$boot_p50_kes, H$boot_p75_kes, H$boot_p995_kes)
    # 50th sits left of its line and 75th right of it, so the two never collide; 99.5th sits left so it stays inside.
    lines <- data.frame(x = q, lab = c("50th", "75th", "99.5th"), hj = c(1.12, -0.12, 1.12))
    ggplot(data.frame(x = B$boot$totals), aes(x)) +
      geom_histogram(bins = 70, fill = "#a3b2d5", colour = COL$bg, linewidth = 0.2) +
      geom_vline(data = lines, aes(xintercept = x), colour = COL$amber, linewidth = 0.8) +
      geom_text(data = lines, aes(x = x, y = Inf, label = lab, hjust = hj), vjust = 1.5, colour = COL$ink, size = 3.6) +
      scale_x_continuous(labels = kes_axis) +
      scale_y_continuous(expand = expansion(c(0, 0.08))) +
      labs(x = "Total outstanding claims, KES", y = "Simulations") + theme_lab()
  }, res = 96)
}

# Backlog --------------------------------------------------------------------

bl_ui <- function() {
  tagList(
    page_head("The Backlog Problem", "What breaks when claims slow down",
      paste("A payment backlog, a claims system migration or a dispute with a provider slows payments down. The claims",
        "still happened and the invoices still arrive. Paid chain ladder cannot tell slow payment from fewer claims,",
        "so it under-reserves. Here a share of the payments due in the last few months before valuation is held back",
        "and paid after it. Nothing else changes, so the true ultimate is the same in every scenario and every gap",
        "below is method error.")),
    uiOutput("bl_headline"),
    div(class = "controls", layout_columns(col_widths = breakpoints(sm = 12, md = c(6, 6)),
      sliderInput("bl_n", "Slowdown length (calendar months up to valuation)", min = 1, max = 6, value = cfg$backlog$default_n_months, step = 1, width = "100%"),
      sliderInput("bl_x", "Share of due payments held back", min = 0, max = 60, value = 100 * cfg$backlog$default_pct, step = 10, post = "%", width = "100%"))),
    uiOutput("bl_kpis"),
    section("Four methods against the truth", plotOutput("bl_methods", height = "300px"), uiOutput("bl_methods_take"), DTOutput("bl_table", fill = FALSE)),
    section("Why excluding the slowed months barely helps", plotOutput("bl_parts", height = "250px"), uiOutput("bl_parts_take")),
    section("See it before the reserve moves",
      layout_columns(col_widths = breakpoints(sm = 12, lg = c(6, 6)),
        tagList(h3("Actual against expected paid"), plotOutput("bl_ae", height = "440px"), uiOutput("bl_ae_take")),
        tagList(h3("Pipeline as months of paid claims"), plotOutput("bl_pipe", height = "440px"), uiOutput("bl_pipe_take")))),
    section("Back-test: hold out the latest month", uiOutput("bl_bt_take"), DTOutput("bl_bt", fill = FALSE)),
    note_box("What we expected, and what the numbers say", uiOutput("bl_expect"))
  )
}

bl_server <- function(input, output, session) {
  sc <- reactive(res$grid[[scenario_key(input$bl_n, input$bl_x / 100)]])
  base <- reactive(res$grid[[scenario_key(input$bl_n, 0)]])
  tot <- function(s, m) s$totals[s$totals$method == m, ]

  output$bl_headline <- renderUI({
    s <- sc(); cl <- tot(s, "cl_all")
    div(class = "headline", backlog_sentence(s$n_months, s$pct, cl$error, cl$error_pct))
  })
  output$bl_kpis <- renderUI({
    s <- sc()
    div(class = "kpi-row",
      kpi("Payments held back", fmt_kes_m(s$delayed_kes), sprintf("%s claims, paid within %d months after valuation.",
          formatC(s$delayed_claims, big.mark = ","), as.integer(cfg$backlog$catch_up_months))),
      kpi("True outstanding", fmt_kes_m(tot(s, "cl_all")$true_outstanding), "Known only because the data is synthetic."),
      kpi("Paid chain ladder says", fmt_kes_m(tot(s, "cl_all")$outstanding), sprintf("Error %s.", fmt_pct(tot(s, "cl_all")$error_pct))),
      kpi("Received chain ladder says", fmt_kes_m(tot(s, "received_cl")$outstanding), sprintf("Error %s.", fmt_pct(tot(s, "received_cl")$error_pct))))
  })
  output$bl_methods <- renderPlot({
    s <- sc()
    d <- s$totals[s$totals$method != "selected", ]
    d$label <- factor(d$label, rev(METHOD_LABELS))
    nar <- is_narrow(session, "bl_methods")
    d$txt <- if (nar) fmt_pct(d$error_pct) else sprintf("%s  (%s)", fmt_kes_m(d$outstanding), fmt_pct(d$error_pct))
    truth <- d$true_outstanding[1]
    ggplot(d, aes(outstanding, label, fill = method)) +
      geom_col(width = 0.62) +
      geom_vline(xintercept = truth, colour = COL$ink, linewidth = 0.9) +
      annotate("text", x = truth, y = 4.45, label = paste("True", fmt_kes_m(truth)), hjust = if (nar) 1.05 else -0.05, size = 3.4, colour = COL$ink) +
      geom_text(aes(label = txt), hjust = -0.08, size = 3.4, colour = COL$ink) +
      scale_fill_manual(values = METHOD_COLS, guide = "none") +
      scale_y_discrete(labels = wrap_lab(if (nar) 16 else 30)) +
      scale_x_continuous(labels = kes_axis, limits = c(0, max(d$outstanding, truth) * if (nar) 1.3 else 1.35),
                         n.breaks = if (nar) 3 else 6, expand = c(0, 0)) +
      labs(x = "Outstanding claims, KES", y = NULL) + theme_lab() +
      theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(colour = COL$ink, size = if (nar) 9 else 11))
  }, res = 96)
  output$bl_methods_take <- renderUI({
    s <- sc(); t <- s$totals
    best <- t[t$method != "selected", ]; best <- best[which.min(abs(best$error)), ]
    takeaway(sprintf(paste("%s is closest, %s off. Paid chain ladder is %s short; excluding the slowed diagonals",
      "takes it to %s short. BF on paid is %s short. The selected basis (BF for the latest %d months) is %s short."),
      best$label, fmt_pct(abs(best$error_pct)), fmt_pct(-tot(s, "cl_all")$error_pct), fmt_pct(-tot(s, "cl_excl")$error_pct),
      fmt_pct(-tot(s, "bf_paid")$error_pct), bf_n, fmt_pct(-tot(s, "selected")$error_pct)))
  })
  output$bl_table <- renderDT({
    t <- sc()$totals
    df <- data.frame(Method = t$label, `Outstanding` = t$outstanding,
                     `Error` = t$error, `Error, % of true` = fmt_pct(t$error_pct), check.names = FALSE)
    DT::formatRound(lab_dt(df, dom = "t"), columns = 2:3, digits = 0)
  })
  output$bl_parts <- renderPlot({
    p <- sc()$cl_error_parts
    labs_ <- c(base_error = "Base case error (no slowdown)", missing_payments = "Payments missing from the latest diagonal",
               multiplied = "Chain ladder multiplying that gap", factors = "Depressed development factors")
    d <- data.frame(part = factor(labs_[names(p)], rev(labs_)), kes = as.numeric(p))
    d$txt <- fmt_kes_m(d$kes)
    nar <- is_narrow(session, "bl_parts")
    ggplot(d, aes(kes, part)) +
      geom_col(width = 0.6, fill = ifelse(names(p) == "factors", COL$plum, COL$amber)) +
      geom_vline(xintercept = 0, colour = COL$axis) +
      geom_text(aes(label = txt), hjust = 1.08, size = 3.4, colour = COL$ink) +
      scale_y_discrete(labels = wrap_lab(if (nar) 16 else 30)) +
      scale_x_continuous(labels = kes_axis, limits = c(min(d$kes, 0) * 1.45, max(d$kes, 0) + 1e6), n.breaks = if (nar) 3 else 6) +
      labs(x = "Contribution to the error, KES", y = NULL) + theme_lab() +
      theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(colour = COL$ink, size = if (nar) 9 else 11))
  }, res = 96)
  output$bl_parts_take <- renderUI({
    s <- sc(); p <- s$cl_error_parts
    if (s$pct == 0) return(takeaway("With no slowdown, the only error is the base case estimation error."))
    takeaway(sprintf(paste("Of the %s shortfall, %s is payments that slipped past valuation and %s is chain ladder",
      "multiplying that gap by its development factors. The factors themselves account for %s. Excluding the slowed",
      "diagonals fixes the factors, so it can only win back that last piece. The latest diagonal is still short."),
      fmt_kes(-sum(p)), fmt_kes(-p[["missing_payments"]]), fmt_kes(-p[["multiplied"]]), fmt_kes(-p[["factors"]])))
  })
  output$bl_ae <- renderPlot({
    d <- sc()$ae$cells
    max_dev <- cfg$diagnostics$ae_max_dev
    d <- d[d$dev <= max_dev, ]
    ggplot(d, aes(dev, factor(acc_idx, levels = n_acc:1), fill = ae)) +
      geom_tile(colour = COL$bg, linewidth = 0.6) +
      scale_fill_gradient2(low = COL$navy, mid = COL$neutral, high = COL$amber, midpoint = 1, limits = c(0.4, 1.6),
                           breaks = c(0.5, 1, 1.5), labels = c("50%", "100%", "150%"),
                           oob = scales::squish, na.value = "#ffffff", name = "Actual paid / expected") +
      scale_y_discrete(labels = function(l) acc_lab[as.integer(l)], breaks = as.character(seq(val, 1, -2))) +
      scale_x_continuous(breaks = seq_len(max_dev), expand = c(0, 0)) +
      labs(x = "Development month", y = "Accident month") + theme_lab() +
      guides(fill = guide_colourbar(title.position = "top", barwidth = unit(14, "lines"), barheight = unit(0.6, "lines"))) +
      theme(panel.grid = element_blank(), legend.title = element_text(size = 10, colour = COL$ink2))
  }, res = 96)
  output$bl_ae_take <- renderUI({
    s <- sc(); bc <- s$ae$by_cal; b0 <- base()$ae$by_cal
    k <- tail(seq_len(nrow(bc)), s$n_months)
    max_dev <- cfg$diagnostics$ae_max_dev
    takeaway(sprintf(paste("Expected paid comes from the received claims and the paid-to-received ratios seen before",
      "the last %d months. Across the slowed months, actual paid is %s of expected (%s with no slowdown). In the",
      "months before, it runs between %s and %s. The slowdown shows as a cold diagonal band. Development months 1",
      "to %d are shown; they carry %s of all payments."),
      as.integer(cfg$diagnostics$ae_fit_gap_months), fmt_pct(sum(bc$actual[k]) / sum(bc$expected[k]), 0),
      fmt_pct(sum(b0$actual[k]) / sum(b0$expected[k]), 0),
      fmt_pct(min(bc$ae[-k]), 0), fmt_pct(max(bc$ae[-k]), 0),
      as.integer(max_dev), fmt_pct(1 / B$factor_tables$paid_all$cdf_volume[max_dev])))
  })
  output$bl_pipe <- renderPlot({
    s <- sc()
    d <- rbind(data.frame(base()$pipeline, case = "No slowdown"), data.frame(s$pipeline, case = "With slowdown"))
    d$month <- months[d$cal_idx]
    ggplot(d, aes(month, months_of_paid, colour = case)) +
      annotate("rect", xmin = months[val - s$n_months + 1] - 15, xmax = months[val] + 15, ymin = -Inf, ymax = Inf, fill = "#f3e6cf", alpha = 0.7) +
      geom_line(linewidth = 1) + geom_point(size = 2) +
      scale_colour_manual(values = c("No slowdown" = COL$muted, "With slowdown" = COL$amber)) +
      scale_x_date(date_breaks = if (is_narrow(session, "bl_pipe")) "6 months" else "3 months", date_labels = "%b %y") +
      scale_y_continuous(limits = c(0, NA), expand = expansion(c(0, 0.08))) +
      labs(x = "Calendar month (shaded: slowed months)", y = "Pipeline, months of paid claims") + theme_lab()
  }, res = 96)
  output$bl_pipe_take <- renderUI({
    s <- sc(); p <- s$pipeline; p0 <- base()$pipeline
    takeaway(sprintf(paste("At valuation the pipeline of received but unpaid claims is %s months of paid claims,",
      "against %s with no slowdown. It needs nothing but the claims system, so it is the earliest warning."),
      formatC(tail(p$months_of_paid, 1), format = "f", digits = 1), formatC(tail(p0$months_of_paid, 1), format = "f", digits = 1)))
  })
  output$bl_bt <- renderDT({
    b <- base()$backtest; s <- sc()$backtest
    df <- data.frame(Method = b$label, `No slowdown: expected` = b$expected, `actual` = b$actual, `A/E` = fmt_pct(b$ae),
                     `With slowdown: expected` = s$expected, `actual ` = s$actual, `A/E ` = fmt_pct(s$ae), check.names = FALSE)
    DT::formatRound(lab_dt(df, dom = "t"), columns = c(2, 3, 5, 6), digits = 0)
  })
  output$bl_bt_take <- renderUI({
    b <- base()$backtest; s <- sc()$backtest
    ae <- function(x, m) x$ae[x$method == m]
    takeaway(sprintf(paste("Each method values the book at %s and projects the payments for %s. With no slowdown,",
      "paid chain ladder's A/E is %s. With the slowdown it is %s: the slowdown pulls down both the forecast and the",
      "actual, so paid chain ladder can pass its own back-test while it is %s short. The received claims chain",
      "ladder's A/E is %s, which does flag it."),
      month_label(months[val - 1]), month_label(months[val]), fmt_pct(ae(b, "cl_all")), fmt_pct(ae(s, "cl_all")),
      fmt_pct(-tot(sc(), "cl_all")$error_pct), fmt_pct(ae(s, "received_cl"))))
  })
  output$bl_expect <- renderUI({
    m <- H$backlog$methods
    tagList(tags$p(sprintf("Before building this page we expected, for a %d-month, %s slowdown: paid chain ladder worst, excluding the slowed diagonals better but still short, received claims chain ladder and BF closer.",
      as.integer(H$backlog$n_months), fmt_pct(H$backlog$pct, 0))),
      tags$ul(
        tags$li(sprintf("Paid chain ladder is the worst, %s short. As expected.", fmt_pct(-m$cl_all$error_pct))),
        tags$li(sprintf("Excluding the slowed diagonals barely helps: %s short. We expected more. The factors were only slightly wrong; the damage is in the latest diagonal.", fmt_pct(-m$cl_excl$error_pct))),
        tags$li(sprintf("Received claims chain ladder is within %s of the truth. As expected.", fmt_pct(abs(m$received_cl$error_pct)))),
        tags$li(sprintf("BF on paid is better than chain ladder but still %s short. Not as close as we expected: BF does not multiply the gap, but it cannot see it either.", fmt_pct(-m$bf_paid$error_pct)))))
  })
}

# IFRS 17 --------------------------------------------------------------------

ifrs_ui <- function() {
  tagList(
    page_head("IFRS 17", "From best estimate to the liability for incurred claims",
      paste("An illustrative view, not an implementation. The best estimate of outstanding claims is spread over the",
        "months it is expected to be paid, discounted on an illustrative curve, and a risk adjustment is added from the",
        "bootstrap distribution at a chosen confidence level.")),
    div(class = "controls", sliderInput("ra_conf", "Risk adjustment confidence level", min = 100 * cfg$ifrs17$ra_slider_min,
        max = 100 * cfg$ifrs17$ra_slider_max, value = 100 * cfg$ifrs17$ra_confidence_default, step = 0.5, post = "%", width = "100%")),
    uiOutput("ifrs_kpis"),
    layout_columns(col_widths = breakpoints(sm = 12, lg = c(6, 6)),
      section("From best estimate to LIC", plotOutput("ifrs_steps", height = "330px"), uiOutput("ifrs_steps_take")),
      section("Expected payments after valuation", plotOutput("ifrs_cf", height = "330px"), uiOutput("ifrs_cf_take"))),
    uiOutput("ifrs_notes"),
    note_box("Calculation engine inputs: discussion, not implementation",
      tags$p(paste("In production these numbers are produced by an IFRS 17 calculation engine rather than a script.",
        "Whatever the engine, the reserving team supplies broadly the same inputs:")),
      tags$ul(
        tags$li("Expected cash flow patterns for incurred claims, by portfolio, group of contracts and cohort."),
        tags$li("Discount curves, with the method used to build them (for example bottom-up, risk-free plus an illiquidity premium)."),
        tags$li("The risk adjustment method and its confidence level, and any diversification between portfolios."),
        tags$li("Actual cash flows for the period, so the engine can produce experience variances and roll the liability forward."),
        tags$li("Mappings from claims system codes to portfolios and cohorts, with reconciliations back to the ledger.")),
      tags$p(class = "muted", "This lab produces the first three for one portfolio and one cohort per accident month. It does not connect to any engine."))
  )
}

ifrs_server <- function(input, output, session) {
  lic <- reactive(lic_view(B$cashflows_by_month$cashflow, B$boot$totals, input$ra_conf / 100, cfg))
  output$ifrs_kpis <- renderUI({
    l <- lic()
    div(class = "kpi-row",
      kpi("Undiscounted best estimate", fmt_kes_m(l$best_estimate), fmt_kes(l$best_estimate)),
      kpi("Present value", fmt_kes_m(l$present_value), sprintf("Discounting removes %s.", fmt_kes(l$discount_effect))),
      kpi(sprintf("Risk adjustment at %s", fmt_pct(l$confidence)), fmt_kes_m(l$ra_discounted), fmt_kes(l$ra_discounted)),
      kpi("Liability for incurred claims", fmt_kes_m(l$lic_discounted), sprintf("%s. Undiscounted: %s.", fmt_kes(l$lic_discounted), fmt_kes_m(l$lic_undiscounted))))
  })
  output$ifrs_steps <- renderPlot({
    l <- lic()
    nar <- is_narrow(session, "ifrs_steps")
    # Horizontal so the step names and the amounts both have room at any width.
    steps <- data.frame(step = c("Best estimate", "Discounting", "Present value", "Risk adjustment", "LIC"),
      start = c(0, l$present_value, 0, l$present_value, 0),
      end = c(l$best_estimate, l$best_estimate, l$present_value, l$lic_discounted, l$lic_discounted),
      kind = c("total", "change", "total", "change", "total"))
    steps$i <- rev(seq_len(nrow(steps)))
    steps$lab <- paste0(c("", "-", "", "+", ""), fmt_kes_m(abs(steps$end - steps$start)))
    ggplot(steps) +
      geom_rect(aes(ymin = i - 0.32, ymax = i + 0.32, xmin = start, xmax = end, fill = kind)) +
      geom_text(aes(y = i, x = pmax(start, end), label = lab), hjust = -0.08, size = if (nar) 3.1 else 3.5, colour = COL$ink) +
      scale_fill_manual(values = c(total = COL$navy, change = COL$amber), guide = "none") +
      scale_y_continuous(breaks = steps$i, labels = steps$step) +
      scale_x_continuous(labels = kes_axis, limits = c(0, max(steps$end) * if (nar) 1.45 else 1.3), expand = c(0, 0),
                         n.breaks = if (nar) 3 else 5) +
      labs(x = "KES", y = NULL) + theme_lab() +
      theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(colour = COL$ink, size = if (nar) 9 else 11))
  }, res = 96)
  output$ifrs_steps_take <- renderUI({
    l <- lic()
    takeaway(sprintf(paste("Discounting takes %s (%s) off the best estimate. The risk adjustment at %s adds %s, so it",
      "matters about %s times as much as discounting here."), fmt_kes(l$discount_effect),
      fmt_pct(l$discount_effect / l$best_estimate), fmt_pct(l$confidence), fmt_kes(l$ra_discounted),
      formatC(l$ra_discounted / l$discount_effect, format = "f", digits = 1)))
  })
  output$ifrs_cf <- renderPlot({
    d <- B$cashflows_by_month
    ggplot(d, aes(month, cashflow)) + geom_col(fill = COL$navy, width = 22) +
      scale_y_continuous(labels = kes_axis, expand = expansion(c(0, 0.05))) +
      scale_x_date(date_breaks = if (is_narrow(session, "ifrs_cf")) "4 months" else "2 months", date_labels = "%b %y") +
      labs(x = "Month of expected payment", y = "KES") + theme_lab()
  }, res = 96)
  output$ifrs_cf_take <- renderUI({
    d <- B$cashflows_by_month
    t_avg <- sum(d$cashflow * (d$future_month - 0.5)) / sum(d$cashflow)
    takeaway(sprintf(paste("%s of the best estimate is expected to be paid in the first three months after",
      "valuation, and all of it within twelve. The average time to payment is %s months."),
      fmt_pct(sum(d$cashflow[1:3]) / sum(d$cashflow)), formatC(t_avg, format = "f", digits = 1)))
  })
  output$ifrs_notes <- renderUI({
    l <- lic()
    terms <- unlist(cfg$ifrs17$curve_terms_years)
    curve <- paste(sprintf("%s at %s %s", fmt_pct(unlist(cfg$ifrs17$curve_spot_rates)), terms,
                           ifelse(terms == 1, "year", "years")), collapse = ", ")
    note_box("Choices and disclosures",
      tags$ul(
        tags$li(sprintf(paste("Discounting. Under the premium allocation approach, IFRS 17 paragraph 59(b) lets an entity",
          "choose not to discount claims expected to be paid within one year of being incurred. Every claim in this",
          "book is paid within %d months of its accident month, so the option is available. The page shows the",
          "discounted LIC as the headline and the undiscounted one beside it, so the size of the choice (%s) is visible."),
          B$max_observed_lag, fmt_kes(l$lic_undiscounted - l$lic_discounted))),
        tags$li(sprintf(paste("Confidence level. IFRS 17 paragraph 119 asks for the confidence level the risk adjustment",
          "corresponds to. Here the risk adjustment is the %s percentile of the bootstrap distribution of outstanding",
          "claims minus its mean, scaled to present value by the best estimate's discount ratio (%s)."),
          fmt_pct(l$confidence), formatC(l$pv_ratio, format = "f", digits = 4))),
        tags$li(sprintf("Discount curve: %s. Illustrative, not a published curve. Payments are assumed mid-month.", curve)),
        tags$li(paste("Simplifications: one portfolio, no diversification, no reinsurance, no claims handling expenses",
          "in the cash flows, and the bootstrap reflects chain ladder uncertainty on the paid triangle only."))))
  })
}

# Methods --------------------------------------------------------------------

meth_ui <- function() {
  n_pass <- sum(tests$result == "passed")
  tagList(
    page_head("Methods", "Assumptions, choices and checks",
      "Everything on the other pages follows from the assumptions and choices below. The data is synthetic and every assumption is set in config.yml."),
    section("Method choices and why",
      div(class = "prose", tags$ul(
        tags$li(tags$b("Triangles. "), sprintf("%d monthly accident months by monthly development, valued at %s. A paid triangle, and a received triangle (invoices valued at the expected settlement ratio) because health claims have no case reserves and so no incurred triangle.", n_acc, val_label)),
        tags$li(tags$b("Chain ladder. "), "Volume weighted factors on all diagonals, no tail factor (payments stop at the lag cap). Written out by hand so the factor choices are explicit; a test checks it against ChainLadder::MackChainLadder."),
        tags$li(tags$b("Bornhuetter-Ferguson. "), sprintf("Prior ultimate = ELR times earned premium, with the paid chain ladder pattern. Default ELR %s, an assumption and not fitted to this book; the README gives published industry context. The book's true loss ratio, which the generator produces and no reserving team would know, is %s.", fmt_pct(cfg$methods$bf_elr, 0), fmt_pct(H$true_loss_ratio))),
        tags$li(tags$b("Selected basis. "), sprintf("BF for the latest %d accident months, chain ladder before. Chain ladder multiplies the newest month by about %s, which makes it fragile there.", bf_n, formatC(O$cdf[n_acc], format = "f", digits = 0))),
        tags$li(tags$b("Mack. "), "ChainLadder::MackChainLadder with Mack's approximation for the last sigma. Columns beyond the lag cap have no variation and carry no uncertainty."),
        tags$li(tags$b("Bootstrap. "), sprintf("ChainLadder::BootChainLadder, %s replicates, %s process distribution, seed %s, on the full %d by %d paid triangle. It returned %d non-finite totals, so no fallback was needed. A 13-column cut at the lag cap was tried first and BootChainLadder rejected it; the full triangle is used.", formatC(H$boot_replicates, big.mark = ","), cfg$bootstrap$process_distr, cfg$bootstrap$seed, n_acc, n_acc, B$boot$n_nan)),
        tags$li(tags$b("Backlog. "), sprintf("For each claim due to be paid in the last N calendar months up to valuation, a fixed random draw decides whether it is held back at share X. Held-back claims pay at random within %d months after valuation. One draw per claim nests the scenarios: every claim held back at 30%% is also held back at 40%%. Report months and amounts never change. The grid runs N from 1 to 6 and X from 0%% to 60%%; the sliders snap to it.", as.integer(cfg$backlog$catch_up_months))),
        tags$li(tags$b("Excluding slowed diagonals. "), "Weights of zero on every link ratio that ends in a slowed calendar month, the same as the weights argument of MackChainLadder (a test checks the two agree)."),
        tags$li(tags$b("A/E diagnostic. "), sprintf("Expected paid in each cell is the received amount to date times the paid-to-received ratio at that development month, measured on payments at least %d months before valuation. The build plan proposed expected = previous cumulative paid times (factor minus 1). We tried it first. It was too noisy at early development, where the factor is large, and its expectation falls with the slowed payments it is checking. Received claims do not slow down, so they make a stable anchor.", as.integer(cfg$diagnostics$ae_fit_gap_months))),
        tags$li(tags$b("Back-test. "), "Value the book one month before valuation, project the held-out month as each method's ultimate times the share its paid pattern expects in that month, and compare with what was paid."),
        tags$li(tags$b("IFRS 17. "), "See the IFRS 17 page for the discounting choice, the risk adjustment and the simplifications."),
        tags$li(tags$b("Your data. "), sprintf(paste("The same paid chain ladder, Mack and bootstrap code runs on an uploaded extract, with %s",
          "bootstrap replicates to keep a run quick. Received claims are valued at settlement ratios measured on the upload's own",
          "settled claims, by benefit type where a type has at least %d of them. The BF prior starts at the chain ladder loss",
          "ratio of the accident months more than %d months old, and the user can move it. The backlog check compares actual",
          "paid with what received claims and older payment patterns predict, the same way as on the Backlog page. Run on the",
          "sample file, it reproduces the Reserves page's chain ladder exactly (a test checks this)."),
          formatC(cfg$upload$bootstrap_R, big.mark = ","), as.integer(cfg$upload$min_claims_for_ratio), as.integer(cfg$upload$mature_after_months)))))),
    section("Limitations",
      div(class = "prose", tags$ul(
        tags$li("Synthetic data. The generator is simple: independent claims, fixed lag patterns, one settlement ratio per benefit type. Real books have changing mixes, tariff renegotiations, fraud reviews and seasonality in processing as well as in claims."),
        tags$li("No case reserves, so no incurred triangle, and no claims handling expenses in any reserve."),
        tags$li("No reinsurance, no diversification, one portfolio."),
        tags$li("The true ultimate is known only because the data is synthetic. It is shown to measure method error and would not exist in practice."),
        tags$li("The backlog is the only stress tested. A change in settlement ratios, a provider fraud review or a benefit change would hit the received claims chain ladder too."),
        tags$li("The Your data page has not been reviewed against real industry data. It applies one set of method choices with no tail factor and no judgement, so its results are a starting point for discussion, not a reserve.")))),
    section("Assumptions (config.yml)", DTOutput("meth_config", fill = FALSE)),
    section("Tests", takeaway(sprintf("%d of %d tests pass, run when the results were built.", n_pass, nrow(tests))), DTOutput("meth_tests", fill = FALSE)),
    div(class = "muted", sprintf("Results built %s with %s and ChainLadder %s.", res$built_at, res$r_version, res$chainladder_version))
  )
}

meth_server <- function(input, output, session) {
  output$meth_config <- renderDT(lab_dt(setNames(res$config_table, c("Parameter", "Value", "What it means")), page_length = 15, dom = "ftip"))
  output$meth_tests <- renderDT(lab_dt(setNames(tests[, c("file", "test", "result", "expectations")],
                                               c("File", "Test", "Result", "Expectations")), page_length = 25, dom = "t"))
}

# Your data ------------------------------------------------------------------
# Visitors run the same methods on their own claims extract. One free run per network address
# (R/trial_store.R); runs on the sample file are free. Files stay in memory for the session only.

SAMPLE_CLAIMS <- "data/sample_claims.csv"
SAMPLE_PREMIUM <- "data/sample_premium.csv"

field_table <- function(f) {
  tags$table(class = "field-table",
    tags$thead(tags$tr(tags$th("Column"), tags$th("Needed"), tags$th("Format"), tags$th("What it is"))),
    tags$tbody(lapply(seq_len(nrow(f)), function(i) tags$tr(
      tags$td(`data-label` = "Column", tags$code(f$field[i])), tags$td(`data-label` = "Needed", f$need[i]),
      tags$td(`data-label` = "Format", f$format[i]), tags$td(`data-label` = "What it is", f$meaning[i])))))
}

contact_link <- function() tags$a(href = paste0("mailto:", CONTACT_EMAIL, "?subject=Health%20IBNR%20Lab%20test%20data"),
                                  CONTACT_EMAIL, .noWS = "outside")

ud_ui <- function() {
  up <- cfg$upload
  tagList(
    page_head("Your data", "Run the lab on your own claims",
      paste("Upload a claims extract in the format below. The lab builds your paid and received triangles, runs chain",
        "ladder, Mack and an ODP bootstrap, adds Bornhuetter-Ferguson if you include earned premium, and checks the",
        "latest months for a payment slowdown. Every step goes into a log you can download."),
      label = "Your own data, or the synthetic sample file. Illustrative only."),
    div(class = "disclaimer",
      div(class = "disclaimer-title", "Not yet tested on real industry data"),
      tags$p(UPLOAD_DISCLAIMER),
      tags$p("I am open to testing it. If you can share de-identified data where the reserve that was actually booked is",
        "known, and ideally how the claims ran off afterwards, I will run it through this tool and send you the comparison:",
        "this tool's estimate, the booked reserve, and what emerged. Write to ", contact_link(), ".")),
    uiOutput("ud_trial"),
    section("1. Put your claims in this format",
      tags$p(class = "prose", "One row per claim invoice, saved as CSV. Column names must match the template; capitals",
        "and spaces do not matter. Months can be", tags$span(class = "nowrap", "YYYY-MM"), "or a full date,",
        tags$span(class = "nowrap", "YYYY-MM-DD,", .noWS = "after"), " and the day is ignored."),
      h3("Claims file"),
      field_table(UPLOAD_CLAIM_FIELDS),
      tags$ul(class = "prose rules",
        tags$li("Not yet paid: leave payment_month blank and put 0 in amount_paid_kes."),
        tags$li("Rejected: put 0 in amount_paid_kes and the month it was closed in payment_month, so rejections count in the settlement ratio."),
        tags$li("Paid in parts: one row per invoice, with the total paid and the month of the last payment."),
        tags$li(sprintf("Up to %s rows and %d MB. With more, send the latest %d accident months.",
          formatC(up$max_rows, big.mark = ","), as.integer(up$max_file_mb), as.integer(up$max_accident_months))),
        tags$li("Leave out names, member numbers, phone numbers and diagnoses. The template does not ask for them, and any column not in it is dropped as soon as the file is read.")),
      h3("Earned premium file (optional)"),
      tags$p(class = "prose", "Needed only for Bornhuetter-Ferguson. One row per accident month."),
      field_table(UPLOAD_PREMIUM_FIELDS),
      div(class = "dl-row",
        downloadButton("ud_tpl_claims", "Claims template", class = "btn-ghost"),
        downloadButton("ud_tpl_premium", "Premium template", class = "btn-ghost"),
        downloadButton("ud_sample_claims", "Sample claims file", class = "btn-ghost"),
        downloadButton("ud_sample_premium", "Sample premium file", class = "btn-ghost")),
      tags$p(class = "muted", "The sample is this lab's synthetic book as it stood at valuation, in the template format.",
        "Run on it, and the chain ladder matches the Reserves page.")),
    section("2. Upload and check",
      div(class = "controls",
        layout_columns(col_widths = breakpoints(sm = 12, md = c(6, 6)),
          fileInput("ud_claims", "Claims file (CSV)", accept = c(".csv", "text/csv"), buttonLabel = "Choose file",
                    placeholder = "No file chosen", width = "100%"),
          fileInput("ud_premium", "Earned premium file (CSV, optional)", accept = c(".csv", "text/csv"),
                    buttonLabel = "Choose file", placeholder = "No file chosen", width = "100%")),
        uiOutput("ud_val")),
      uiOutput("ud_checks"),
      div(class = "run-row",
        actionButton("ud_run", "Run on my file", class = "btn-run", disabled = TRUE),
        actionButton("ud_sample", "Run on the sample file", class = "btn-ghost"),
        uiOutput("ud_run_why", inline = TRUE)),
      div(class = "log-box",
        div(class = "log-head", tags$span(class = "note-title", "Log of what has been done"),
            downloadButton("ud_log_dl", "Download the log", class = "btn-ghost btn-sm")),
        tags$details(tags$summary("Show the log"), verbatimTextOutput("ud_log_text")))),
    uiOutput("ud_results"),
    note_box("What happens to your data",
      tags$ul(
        tags$li("Your files are read into memory for this visit only. They are not saved, and they are gone when you close the page."),
        tags$li("Columns that are not in the template are dropped as soon as the file is read, before anything else happens."),
        tags$li(paste("To keep the free run to one per network address, the app stores a scrambled fingerprint of the address",
          "(a salted SHA-256 hash, which cannot be turned back into the address) and the date of the run. Nothing else is kept.")),
        tags$li(tagList("An office or mobile network can share one address, so a colleague may have used the run already. Write to ",
          contact_link(), " for another.")),
        tags$li("Checking a file is free. Only a finished run on your own file uses the free run. Runs on the sample file are always free.")))
  )
}

ud_server <- function(input, output, session) {
  up <- cfg$upload
  lg <- run_log()
  log_tick <- reactiveVal(0)
  touch <- function() log_tick(isolate(log_tick()) + 1)
  addr <- client_address(session$request)
  hash <- if (is.na(addr)) NA_character_ else trial_hash(addr, TRIALS$salt)
  trial_tick <- reactiveVal(0)
  trial <- reactive({
    trial_tick()
    if (is.na(hash)) list(used = NA, date = NA) else TRIALS$used(hash)
  })
  files <- reactiveVal(list())
  prep <- reactiveVal(NULL)
  run <- reactiveVal(NULL)
  last_elr <- reactiveVal(NA)

  read_file <- function(f, what) {
    lg$add("File", sprintf("%s file received: %s, %s bytes, SHA-256 %s.", what, f$name,
                           formatC(f$size, format = "d", big.mark = ","), file_fingerprint(f$datapath)))
    tryCatch(read_upload_csv(f$datapath, up$max_rows),
             error = function(e) { lg$add("Failed", f$name, " could not be read as CSV: ", conditionMessage(e)); NULL })
  }
  do_check <- function(val_key = NULL) {
    fl <- files()
    if (is.null(fl$name)) return()
    run(NULL)
    lg$add("Check", "Checking ", fl$name, if (!is.null(fl$premium_name)) paste0(" with ", fl$premium_name) else "", ".")
    p <- if (is.null(fl$raw)) list(ok = FALSE, checks = data.frame(status = "Failed", check = "File",
                                   detail = "The file could not be read as CSV.", stringsAsFactors = FALSE))
         else prepare_upload(fl$raw, fl$premium_raw, cfg, lg, val_key)
    lg$add(if (p$ok) "Check" else "Failed", if (p$ok) "The file is ready to run." else "The file cannot be run until the failed checks are fixed.")
    prep(p)
    touch()
  }

  observeEvent(input$ud_claims, {
    fl <- files(); f <- input$ud_claims
    fl$raw <- read_file(f, "Claims"); fl$name <- f$name
    files(fl)
    do_check()
  })
  observeEvent(input$ud_premium, {
    fl <- files(); f <- input$ud_premium
    fl$premium_raw <- read_file(f, "Premium"); fl$premium_name <- f$name
    files(fl)
    vk <- prep()$val_key
    if (identical(vk, prep()$latest_pay_key)) vk <- NULL  # keep the default unless a month was chosen
    if (is.null(fl$name)) touch() else do_check(vk)
  })
  observeEvent(input$ud_val_month, {
    p <- prep()
    if (!is.null(p$val_key) && as.integer(input$ud_val_month) != p$val_key) {
      lg$add("Check", "Valuation month changed to ", month_key_label(as.integer(input$ud_val_month)), ".")
      do_check(as.integer(input$ud_val_month))
    }
  }, ignoreInit = TRUE)

  output$ud_val <- renderUI({
    p <- prep()
    if (is.null(p$latest_pay_key)) return(NULL)
    keys <- seq(p$latest_pay_key, p$latest_pay_key - 23L)
    selectInput("ud_val_month", "Valuation month", choices = stats::setNames(keys, month_key_label(keys)),
                selected = p$val_key, width = "260px")
  })

  output$ud_trial <- renderUI({
    t <- trial()
    if (is.na(hash)) div(class = "trial-pill used", "This visit's network address could not be read, so the free run on your own file",
                         "is not available here. The sample file still runs. Write to ", contact_link(), " for a run.")
    else if (isTRUE(t$used)) div(class = "trial-pill used", sprintf("This network address used its free run%s.",
      if (!is.na(t$date)) paste(" on", format(as.Date(t$date), "%d %B %Y")) else ""),
      " The sample file still runs. For another run on your own data, write to ", contact_link(), ".")
    else div(class = "trial-pill ok", "One free run on your own file is available from this network address.",
             "Checking a file does not use it; only a finished run does.")
  })

  can_run <- reactive({
    p <- prep()
    !is.null(p) && isTRUE(p$ok) && !is.na(hash) && !isTRUE(trial()$used)
  })
  observe({
    req(identical(input$nav, "Your data"))  # look the address up only when someone opens this page
    updateActionButton(session, "ud_run", disabled = !can_run())
  })
  # ChainLadder and its dependencies take several seconds to load the first time. Load them while the
  # visitor reads the page, after it has drawn, rather than when they press Run.
  observeEvent(input$nav, {
    if (identical(input$nav, "Your data") && !isNamespaceLoaded("ChainLadder"))
      later::later(function() suppressPackageStartupMessages(requireNamespace("ChainLadder", quietly = TRUE)), 1)
  })
  output$ud_run_why <- renderUI({
    p <- prep()
    tags$span(class = "muted", if (is.na(hash) || isTRUE(trial()$used)) "The free run from this address is used."
      else if (is.null(p)) "Upload a claims file and it is checked straight away."
      else if (!isTRUE(p$ok)) "Fix the failed checks and upload the file again."
      else "This uses your one free run.")
  })

  output$ud_checks <- renderUI({
    p <- prep()
    if (is.null(p)) return(NULL)
    ck <- p$checks
    n_fail <- sum(ck$status == "Failed"); n_warn <- sum(ck$status == "Warning")
    s <- function(n) if (n == 1) "" else "s"
    tagList(
      div(class = paste("check-summary", if (n_fail) "fail" else if (n_warn) "warn" else "pass"),
        if (n_fail) sprintf("%d check%s failed. Fix the file and upload it again.", n_fail, s(n_fail))
        else if (n_warn) sprintf("Ready to run, with %d warning%s to read first.", n_warn, s(n_warn))
        else "Ready to run. Every check passed."),
      tags$ul(class = "check-list", lapply(seq_len(nrow(ck)), function(i)
        tags$li(class = tolower(ck$status[i]), tags$span(class = "check-status", ck$status[i]),
                tags$span(tags$b(ck$check[i], .noWS = "after"), ". ", ck$detail[i])))))
  })

  start_run <- function(p, source_label) {
    r <- withProgress(message = "Running chain ladder, Mack and the bootstrap", value = 0.4,
      tryCatch(run_upload(p, cfg, lg), error = function(e) { lg$add("Failed", "The run stopped: ", conditionMessage(e)); NULL }))
    if (!is.null(r)) {
      # The slider moves in steps of 0.1%, so start BF at the default rounded to that step.
      if (!is.null(r$elr)) r$elr <- round(min(max(r$elr, up$elr_slider_min), up$elr_slider_max), 3)
      r$source <- source_label; last_elr(r$elr); run(r)
    }
    r
  }
  observeEvent(input$ud_run, {
    p <- prep()
    if (is.null(p) || !isTRUE(p$ok)) return()
    if (is.na(hash)) { lg$add("Trial", "The network address could not be read, so the free run is not available."); touch(); return() }
    t <- TRIALS$used(hash)
    if (isTRUE(t$used)) { lg$add("Trial", "This network address has already used its free run."); trial_tick(trial_tick() + 1); touch(); return() }
    lg$add("Run", "Free run started on ", files()$name, ".")
    r <- start_run(p, files()$name)
    if (!is.null(r)) {
      TRIALS$claim(hash)
      lg$add("Trial", "The free run for this network address is now used.")
      trial_tick(trial_tick() + 1)
    }
    touch()
  })
  observeEvent(input$ud_sample, {
    lg$add("Run", "Sample run started. It does not use the free run.")
    lg$add("File", sprintf("Sample claims file, SHA-256 %s.", file_fingerprint(SAMPLE_CLAIMS)))
    lg$add("File", sprintf("Sample premium file, SHA-256 %s.", file_fingerprint(SAMPLE_PREMIUM)))
    p <- prepare_upload(read_upload_csv(SAMPLE_CLAIMS, up$max_rows), read_upload_csv(SAMPLE_PREMIUM, up$max_rows), cfg, lg)
    start_run(p, "the synthetic sample file")
    touch()
  })

  elr <- reactive({
    r <- run()
    if (is.null(r$premium)) return(NULL)
    if (is.null(input$ud_elr)) r$elr else input$ud_elr / 100
  })
  elr_settled <- debounce(elr, 1200)
  observeEvent(elr_settled(), {
    r <- run(); e <- elr_settled()
    if (is.null(r) || is.null(e) || isTRUE(abs(e - last_elr()) < 5e-4)) return()
    last_elr(e)
    lg$add("Method", sprintf("Bornhuetter-Ferguson loss ratio set to %s: BF outstanding KES %s.", fmt_pct(e),
                             formatC(sum(upload_bf(r, e)$reserve), format = "f", digits = 0, big.mark = ",")))
    touch()
  })

  output$ud_log_text <- renderText({ log_tick(); paste(log_text(lg, CONTACT_EMAIL), collapse = "\n") })
  log_download <- function() downloadHandler(
    filename = function() paste0("health-ibnr-lab-log-", lg$ref, ".txt"),
    content = function(file) writeLines(log_text(lg, CONTACT_EMAIL), file, useBytes = TRUE))
  output$ud_log_dl <- log_download()
  output$ud_log_dl2 <- log_download()
  output$ud_tpl_claims <- downloadHandler(filename = "claims_template.csv",
    content = function(file) writeLines(paste(UPLOAD_CLAIM_FIELDS$field, collapse = ","), file))
  output$ud_tpl_premium <- downloadHandler(filename = "premium_template.csv",
    content = function(file) writeLines(paste(UPLOAD_PREMIUM_FIELDS$field, collapse = ","), file))
  output$ud_sample_claims <- downloadHandler(filename = "sample_claims.csv", content = function(file) file.copy(SAMPLE_CLAIMS, file))
  output$ud_sample_premium <- downloadHandler(filename = "sample_premium.csv", content = function(file) file.copy(SAMPLE_PREMIUM, file))

  results_table <- reactive({
    r <- run(); req(r)
    df <- upload_results_table(r, if (is.null(elr())) r$elr else elr())
    kes <- grepl("_kes$", names(df))
    df[kes] <- lapply(df[kes], round)
    df$cdf_to_ultimate <- round(df$cdf_to_ultimate, 4)
    df
  })
  output$ud_results_csv <- downloadHandler(filename = function() paste0("health-ibnr-lab-results-", lg$ref, ".csv"),
    content = function(file) utils::write.csv(results_table(), file, row.names = FALSE, na = ""))
  output$ud_tri_csv <- downloadHandler(filename = function() paste0("health-ibnr-lab-paid-triangle-", lg$ref, ".csv"),
    content = function(file) utils::write.csv(upload_triangle_table(run()), file, row.names = FALSE, na = ""))

  output$ud_results <- renderUI({
    r <- run()
    if (is.null(r)) return(NULL)
    tot_os <- sum(r$cl$reserve)
    elr_ui <- if (!is.null(r$premium)) {
      tagList(h3("Bornhuetter-Ferguson"),
        div(class = "controls", sliderInput("ud_elr", "BF expected loss ratio (prior)", min = round(100 * up$elr_slider_min),
          max = round(100 * up$elr_slider_max), value = 100 * r$elr, step = 0.1, post = "%", width = "100%")),
        uiOutput("ud_bf_take"))
    }
    section(paste("3. Results:", r$source),
      div(class = "kpi-row",
        kpi("Paid to date", fmt_kes_m(sum(r$cl$latest)), sprintf("%d accident months, valued at %s.", r$n_acc, month_key_label(r$val_key))),
        kpi("Chain ladder outstanding", fmt_kes_m(tot_os), sprintf("%s. %s months of paid claims.", fmt_kes(tot_os),
            formatC(tot_os / r$avg_paid, format = "f", digits = 1))),
        kpi("Mack coefficient of variation", if (is.null(r$mack)) "Not fitted" else fmt_pct(r$mack$total_se / r$mack$total_reserve),
            if (!is.null(r$mack)) sprintf("Standard error %s.", fmt_kes_m(r$mack$total_se)) else "See the log."),
        kpi("Bootstrap 75th percentile", if (is.null(r$boot)) "Not run" else fmt_kes_m(r$boot$q[2]),
            if (!is.null(r$boot)) sprintf("99.5th percentile %s. %s replicates.", fmt_kes_m(r$boot$q[3]), formatC(r$boot_R, big.mark = ","))
            else "See the log.")),
      plotOutput("ud_plot", height = "320px"), uiOutput("ud_plot_take"),
      elr_ui,
      if (!is.null(r$boot)) tagList(h3("Spread of the reserve"), plotOutput("ud_boot", height = "260px"), uiOutput("ud_boot_take")),
      if (!is.null(r$rcl)) tagList(h3("Backlog check"),
        layout_columns(col_widths = breakpoints(sm = 12, lg = c(6, 6)),
          tagList(plotOutput("ud_ae", height = "280px"), uiOutput("ud_ae_take")),
          tagList(plotOutput("ud_pipe", height = "280px"), uiOutput("ud_pipe_take"))),
        uiOutput("ud_recv_take"))
      else note_box("No backlog check", tags$p("Add report_month and amount_billed_kes to the file to get the received triangle, the pipeline and the backlog check.")),
      h3("By accident month"),
      DTOutput("ud_table", fill = FALSE),
      div(class = "dl-row",
        downloadButton("ud_results_csv", "Download results (CSV)", class = "btn-ghost"),
        downloadButton("ud_tri_csv", "Download the paid triangle (CSV)", class = "btn-ghost"),
        downloadButton("ud_log_dl2", "Download the log", class = "btn-ghost")),
      tags$p(class = "muted", UPLOAD_DISCLAIMER))
  })

  output$ud_plot <- renderPlot({
    r <- run(); req(r)
    d <- rbind(data.frame(month = r$months, part = "Paid to date", kes = r$cl$latest),
               data.frame(month = r$months, part = "Still to pay (chain ladder)", kes = r$cl$reserve))
    d$part <- factor(d$part, c("Still to pay (chain ladder)", "Paid to date"))
    nar <- is_narrow(session, "ud_plot")
    ggplot(d, aes(month, kes, fill = part)) +
      geom_col(width = 24, colour = COL$bg, linewidth = 0.3) +
      scale_fill_manual(values = c("Paid to date" = COL$navy, "Still to pay (chain ladder)" = COL$amber)) +
      scale_y_continuous(labels = kes_axis, expand = expansion(c(0, 0.04))) +
      scale_x_date(date_breaks = if (nar) "12 months" else if (r$n_acc > 36) "6 months" else "3 months", date_labels = "%b %y") +
      labs(x = "Accident month", y = "KES") + theme_lab()
  }, res = 96)
  output$ud_plot_take <- renderUI({
    r <- run(); req(r)
    k <- utils::tail(seq_len(r$n_acc), 3)
    takeaway(sprintf(paste("The latest 3 accident months hold %s of the chain ladder outstanding. The newest month is",
      "multiplied by %s, so a small change in what gets paid early moves the reserve a lot."),
      fmt_pct(sum(r$cl$reserve[k]) / sum(r$cl$reserve)), formatC(r$cl$cdf[r$n_acc], format = "f", digits = 1)))
  })
  output$ud_bf_take <- renderUI({
    r <- run(); e <- elr(); req(r, e)
    bf <- upload_bf(r, e)
    takeaway(sprintf(paste("At a prior loss ratio of %s, BF gives %s outstanding against chain ladder's %s. The slider",
      "starts at %s, the %s."), fmt_pct(e), fmt_kes(sum(bf$reserve)), fmt_kes(sum(r$cl$reserve)), fmt_pct(r$elr_default), r$elr_basis))
  })
  output$ud_boot <- renderPlot({
    r <- run(); req(r$boot)
    lines <- data.frame(x = r$boot$q, lab = c("50th", "75th", "99.5th"), hj = c(1.12, -0.12, 1.12))
    ggplot(data.frame(x = r$boot$totals[is.finite(r$boot$totals)]), aes(x)) +
      geom_histogram(bins = 60, fill = "#a3b2d5", colour = COL$bg, linewidth = 0.2) +
      geom_vline(data = lines, aes(xintercept = x), colour = COL$amber, linewidth = 0.8) +
      geom_text(data = lines, aes(x = x, y = Inf, label = lab, hjust = hj), vjust = 1.5, colour = COL$ink, size = 3.6) +
      scale_x_continuous(labels = kes_axis) + scale_y_continuous(expand = expansion(c(0, 0.08))) +
      labs(x = "Total outstanding claims, KES", y = "Simulations") + theme_lab()
  }, res = 96)
  output$ud_boot_take <- renderUI({
    r <- run(); req(r$boot)
    takeaway(sprintf(paste("The bootstrap mean is %s, %s from the chain ladder. A reserve at the 75th percentile would",
      "be %s, which is %s above the chain ladder."), fmt_kes(r$boot$mean), fmt_pct(abs(r$boot$mean / sum(r$cl$reserve) - 1)),
      fmt_kes(r$boot$q[2]), fmt_kes(r$boot$q[2] - sum(r$cl$reserve))))
  })
  cal_month <- function(r, cal_idx) month_key_to_date(r$val_key - r$n_acc + cal_idx)
  # Twelve monthly labels only fit on a wide chart.
  month_breaks <- function(id) {
    w <- session$clientData[[paste0("output_", id, "_width")]]
    if (is.null(w) || w < 500) "3 months" else if (w < 900) "2 months" else "1 month"
  }
  output$ud_ae <- renderPlot({
    r <- run(); req(r$ae)
    d <- r$ae
    d$month <- cal_month(r, d$cal_idx)
    d$latest <- seq_len(nrow(d)) > nrow(d) - 3
    ggplot(d, aes(month, ae, fill = latest)) +
      geom_col(width = 22) + geom_hline(yintercept = 1, colour = COL$ink, linewidth = 0.6) +
      scale_fill_manual(values = c(`TRUE` = COL$amber, `FALSE` = "#a3b2d5"), guide = "none") +
      scale_y_continuous(labels = function(x) paste0(round(100 * x), "%"), expand = expansion(c(0, 0.06))) +
      scale_x_date(date_breaks = month_breaks("ud_ae"), date_labels = "%b %y") +
      labs(x = "Calendar month (amber: latest 3)", y = "Actual paid / expected") + theme_lab()
  }, res = 96)
  output$ud_ae_take <- renderUI({
    r <- run(); req(r$ae)
    d <- r$ae; k <- utils::tail(seq_len(nrow(d)), 3)
    takeaway(sprintf(paste("In the latest 3 calendar months you paid %s of what your received claims and older payment",
      "patterns lead us to expect. In the %d months before, it ran between %s and %s. A clear drop in the latest",
      "months is what a payment slowdown looks like."), fmt_pct(sum(d$actual[k]) / sum(d$expected[k])), nrow(d) - 3L,
      fmt_pct(min(d$ae[-k])), fmt_pct(max(d$ae[-k]))))
  })
  output$ud_pipe <- renderPlot({
    r <- run(); req(r$trend)
    d <- r$trend
    d$month <- cal_month(r, d$cal_idx)
    ggplot(d, aes(month, months_of_paid)) + geom_line(colour = COL$amber, linewidth = 1) + geom_point(colour = COL$amber, size = 2) +
      scale_y_continuous(limits = c(0, NA), expand = expansion(c(0, 0.08))) +
      scale_x_date(date_breaks = month_breaks("ud_pipe"), date_labels = "%b %y") +
      labs(x = "Calendar month", y = "Pipeline, months of paid claims") + theme_lab()
  }, res = 96)
  output$ud_pipe_take <- renderUI({
    r <- run(); req(r$trend)
    d <- r$trend
    takeaway(sprintf(paste("Received but unpaid claims are %s months of paid claims at valuation, against %s twelve",
      "months earlier. A rising pipeline is the earliest warning of a backlog."),
      formatC(utils::tail(d$months_of_paid, 1), format = "f", digits = 1), formatC(d$months_of_paid[1], format = "f", digits = 1)))
  })
  output$ud_recv_take <- renderUI({
    r <- run(); req(r$rcl)
    rec_os <- sum(r$rcl$ultimate) - sum(r$cl$latest)
    takeaway(sprintf(paste("Received claims chain ladder gives %s outstanding, %s %s than paid chain ladder. Of the paid",
      "chain ladder's outstanding, %s is pipeline: invoices already in but not yet paid. If the received figure is",
      "well above the paid one, payments may have slowed; the Backlog page shows why paid chain ladder then falls short."),
      fmt_kes(rec_os), fmt_pct(abs(rec_os / sum(r$cl$reserve) - 1)), if (rec_os >= sum(r$cl$reserve)) "more" else "less",
      fmt_kes(sum(r$pipeline))))
  })
  output$ud_table <- renderDT({
    df <- results_table()
    show <- data.frame(`Accident month` = df$accident_month, `Paid to date` = df$paid_to_date_kes,
      `Development months` = df$development_months, `To ultimate` = df$cdf_to_ultimate,
      `CL ultimate` = df$cl_ultimate_kes, `CL outstanding` = df$cl_outstanding_kes, check.names = FALSE)
    if (!is.null(df$mack_se_kes)) show$`Mack SE` <- df$mack_se_kes
    if (!is.null(df$bf_outstanding_kes)) show$`BF outstanding` <- df$bf_outstanding_kes
    if (!is.null(df$pipeline_kes)) show$Pipeline <- df$pipeline_kes
    kes_cols <- which(!names(show) %in% c("Accident month", "Development months", "To ultimate"))
    DT::formatRound(DT::formatRound(lab_dt(show, dom = "t", paging = FALSE, scrollY = "420px"), columns = kes_cols, digits = 0),
                    columns = "To ultimate", digits = 3)
  })
}

# App ------------------------------------------------------------------------

ui <- page_navbar(
  id = "nav",
  title = tags$span(class = "brand", tags$img(src = "mark.svg", alt = ""),
    tags$span(class = "brand-text", tags$span(class = "brand-name", "Health IBNR Lab"),
              tags$span(class = "brand-tag", "Reserving a medical book, and what breaks when claims slow down"))),
  window_title = "Health IBNR Lab",
  theme = lab_theme(),
  fillable = FALSE,
  navbar_options = navbar_options(bg = COL$bar, theme = "dark", collapsible = TRUE),
  header = tags$head(tags$link(rel = "stylesheet", href = "styles.css"), tags$script(src = "app.js"),
                     tags$meta(name = "description", content = "Health IBNR reserving on a synthetic medical book, in R.")),
  nav_panel("Book", book_ui()),
  nav_panel("Triangles", tri_ui()),
  nav_panel("Reserves", res_ui()),
  nav_panel("Uncertainty", unc_ui()),
  nav_panel("Backlog", bl_ui()),
  nav_panel("IFRS 17", ifrs_ui()),
  nav_panel("Methods", meth_ui()),
  nav_panel("Your data", ud_ui()),
  footer = div(class = "footer container-fluid main",
    sprintf("%s Valuation %s. Built in R with shiny, bslib and ChainLadder. Marion Peter.", SYNTHETIC_LABEL, val_label))
)

server <- function(input, output, session) {
  book_server(input, output, session)
  tri_server(input, output, session)
  res_server(input, output, session)
  unc_server(input, output, session)
  bl_server(input, output, session)
  ifrs_server(input, output, session)
  meth_server(input, output, session)
  ud_server(input, output, session)
}

shinyApp(ui, server)
