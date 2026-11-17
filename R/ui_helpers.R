# Theme, formatting and small layout pieces shared by every page.
# Tokens follow the Instalment Persistency Lab so the two apps read as a family.

COL <- list(
  bar = "#0F1117", gold = "#E8960C", ink = "#1d2430", ink2 = "#52514e", muted = "#898781",
  grid = "#e6e4dd", axis = "#c9c6bc", surface = "#fdfcfa", bg = "#faf9f6",
  navy = "#3d5a9e", amber = "#c47a06", teal = "#00998a", plum = "#8e3a5d", neutral = "#efede7"
)

# Method colours, checked for colour-blind separation in this order.
METHOD_COLS <- c(cl_all = "#c47a06", cl_excl = "#8e3a5d", received_cl = "#00998a", bf_paid = "#3d5a9e",
                 selected = "#898781")

SYNTHETIC_LABEL <- "Synthetic portfolio. Illustrative only."

# Where people send test data and booked reserves to compare against.
CONTACT_EMAIL <- "marionpeter@nuruops.com"

lab_theme <- function() {
  bslib::bs_theme(
    version = 5, bg = COL$bg, fg = COL$ink, primary = "#34497d", secondary = "#52514e",
    base_font = bslib::font_collection("Inter", "system-ui", "-apple-system", "Segoe UI", "Roboto", "sans-serif"),
    heading_font = bslib::font_collection("EB Garamond", "Garamond", "Georgia", "serif"),
    "border-radius" = "0.35rem", "navbar-padding-y" = "0.6rem"
  )
}

page_head <- function(kicker, title, lede = NULL, label = SYNTHETIC_LABEL) {
  shiny::div(class = "page-head",
    shiny::div(class = "kicker", kicker),
    shiny::h1(title),
    shiny::div(class = "synthetic", label),
    if (!is.null(lede)) shiny::p(class = "lede", lede)
  )
}

kpi <- function(label, value, note = NULL, class = NULL) {
  shiny::div(class = paste("kpi", class),
    shiny::div(class = "label", label),
    shiny::div(class = "value", value),
    if (!is.null(note)) shiny::div(class = "note", note)
  )
}

takeaway <- function(...) shiny::div(class = "takeaway", ...)

section <- function(title, ...) shiny::div(class = "section", shiny::h2(title), ...)

note_box <- function(title, ...) shiny::div(class = "note-box", shiny::div(class = "note-title", title), ...)

month_label <- function(d) format(as.Date(d), "%b %Y")

wrap_lab <- function(width) function(x) vapply(strwrap(x, width, simplify = FALSE), paste, character(1), collapse = "\n")

# TRUE when a plot output is drawn narrower than a tablet, so charts can thin their axes.
is_narrow <- function(session, id) isTRUE(session$clientData[[paste0("output_", id, "_width")]] < 600)

kes_axis <- function(x) paste0(formatC(x / 1e6, format = "f", digits = 0, big.mark = ","), "m")

theme_lab <- function(base_size = 13) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      text = ggplot2::element_text(colour = COL$ink),
      plot.background = ggplot2::element_rect(fill = COL$bg, colour = NA),
      panel.background = ggplot2::element_rect(fill = COL$bg, colour = NA),
      panel.grid.major = ggplot2::element_line(colour = COL$grid, linewidth = 0.4),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(colour = COL$ink2, size = base_size - 2),
      axis.title = ggplot2::element_text(colour = COL$ink2, size = base_size - 2),
      axis.line.x = ggplot2::element_line(colour = COL$axis, linewidth = 0.4),
      legend.position = "top", legend.justification = "left",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(colour = COL$ink2, size = base_size - 2),
      plot.margin = ggplot2::margin(6, 12, 6, 6)
    )
}

# TRUE for a column of numbers, KES amounts or percentages, however they are formatted.
num_like <- function(x) is.numeric(x) || all(is.na(x) | grepl("^\\s*(KES )?[-+]?[0-9][0-9,]*([.][0-9]+)?%?\\s*$", x))

# DT table with the lab's defaults. Number columns are right aligned and never wrap; text and headers wrap,
# so a table fits its column where it can and scrolls inside its own box where it cannot.
lab_dt <- function(df, page_length = 12, dom = "tip", ...) {
  is_num <- vapply(df, num_like, logical(1))
  is_short <- !is_num & vapply(df, function(x) max(nchar(as.character(x)), 0, na.rm = TRUE) <= 12, logical(1))
  defs <- list()
  if (any(is_num)) defs <- c(defs, list(list(className = "dt-right num", targets = as.list(which(is_num) - 1))))
  if (any(is_short)) defs <- c(defs, list(list(className = "short", targets = as.list(which(is_short) - 1))))
  DT::datatable(df, rownames = FALSE, class = "compact stripe", fillContainer = FALSE,
                options = list(pageLength = page_length, dom = dom, scrollX = TRUE, ordering = FALSE,
                               columnDefs = defs, ...))
}
