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
        tags$li(tags$b("IFRS 17. "), "See the IFRS 17 page for the discounting choice, the risk adjustment and the simplifications.")))),
    section("Limitations",
      div(class = "prose", tags$ul(
        tags$li("Synthetic data. The generator is simple: independent claims, fixed lag patterns, one settlement ratio per benefit type. Real books have changing mixes, tariff renegotiations, fraud reviews and seasonality in processing as well as in claims."),
        tags$li("No case reserves, so no incurred triangle, and no claims handling expenses in any reserve."),
        tags$li("No reinsurance, no diversification, one portfolio."),
        tags$li("The true ultimate is known only because the data is synthetic. It is shown to measure method error and would not exist in practice."),
        tags$li("The backlog is the only stress tested. A change in settlement ratios, a provider fraud review or a benefit change would hit the received claims chain ladder too.")))),
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

# App ------------------------------------------------------------------------

ui <- page_navbar(
  id = "nav",
  title = tags$span(class = "navbar-brand", tags$img(src = "mark.svg", alt = ""),
    tags$span(tags$span(class = "brand-name", "Health IBNR Lab"), tags$br(),
              tags$span(class = "brand-tag", "Reserving a medical book, and what breaks when claims slow down"))),
  window_title = "Health IBNR Lab",
  theme = lab_theme(),
  fillable = FALSE,
  navbar_options = navbar_options(bg = COL$bar, theme = "dark", collapsible = TRUE),
  header = tags$head(tags$link(rel = "stylesheet", href = "styles.css"), tags$script(src = "tables.js"),
                     tags$meta(name = "description", content = "Health IBNR reserving on a synthetic medical book, in R.")),
  nav_panel("Book", book_ui()),
  nav_panel("Triangles", tri_ui()),
  nav_panel("Reserves", res_ui()),
  nav_panel("Uncertainty", unc_ui()),
  nav_panel("Backlog", bl_ui()),
  nav_panel("IFRS 17", ifrs_ui()),
  nav_panel("Methods", meth_ui()),
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
}

shinyApp(ui, server)
