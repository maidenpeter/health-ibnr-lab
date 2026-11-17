# Takes a full-page screenshot of every page of a running app. Needs Chrome.
# Usage: start the app (shiny::runApp(port = 7788)), then Rscript scripts/screenshots.R [url] [width] [suffix]
args <- commandArgs(trailingOnly = TRUE)
url <- if (length(args) >= 1) args[1] else "http://127.0.0.1:7788"
width <- if (length(args) >= 2) as.integer(args[2]) else 1360
suffix <- if (length(args) >= 3) args[3] else ""
pages <- c("Book", "Triangles", "Reserves", "Uncertainty", "Backlog", "IFRS 17", "Methods", "Your data")
dir.create("assets/screenshots", recursive = TRUE, showWarnings = FALSE)

options(chromote.timeout = 90)
b <- chromote::ChromoteSession$new(width = width, height = 900)
# A documentation address (TEST-NET-3), so a local run shows the Your data page as a visitor sees it.
b$Network$enable()
b$Network$setExtraHTTPHeaders(headers = list(`X-Forwarded-For` = "203.0.113.10"))
b$Page$navigate(url, wait_ = FALSE)
Sys.sleep(12)
for (i in seq_along(pages)) {
  p <- pages[i]
  b$Runtime$evaluate(sprintf("document.querySelector('a.nav-link[data-value=\"%s\"]').click()", p))
  Sys.sleep(7)
  # Your data: run the sample file so the page shows results, then open the log.
  if (p == "Your data") {
    b$Runtime$evaluate("document.getElementById('ud_sample').click()")
    Sys.sleep(25)
    b$Runtime$evaluate("document.querySelector('.log-box details').open = true")
    Sys.sleep(2)
  }
  errs <- b$Runtime$evaluate("Array.from(document.querySelectorAll('.tab-pane.active .shiny-output-error')).map(e => e.innerText).join(' | ')")$result$value
  if (nzchar(errs)) message(p, " ERRORS: ", errs)
  h <- b$Runtime$evaluate("document.documentElement.scrollHeight")$result$value
  b$Emulation$setDeviceMetricsOverride(width = width, height = h, deviceScaleFactor = 1, mobile = width < 700)
  Sys.sleep(1.5)
  f <- sprintf("assets/screenshots/%d-%s%s.png", i, tolower(gsub(" ", "", p)), suffix)
  b$screenshot(f, selector = "html", wait_ = TRUE)
  b$Emulation$setDeviceMetricsOverride(width = width, height = 900, deviceScaleFactor = 1, mobile = width < 700)
  message("saved ", f)
}
b$close()
