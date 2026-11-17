# Takes the README's pictures of a running app. Needs Chrome.
# Usage: start the app (shiny::runApp(port = 7788)), then Rscript scripts/readme_images.R [url]
# Writes, under assets/readme/:
#   pages/1-book.png ... pages/8-yourdata.png  what each page shows above the fold, all the same size
#   hero_backlog.png, phone_book.png            the Backlog page on a laptop and the Book page on a phone
#   hero.png                                    the two combined, laid out by scripts/readme_hero.html

args <- commandArgs(trailingOnly = TRUE)
url <- if (length(args) >= 1) args[1] else "http://127.0.0.1:7788"
pages <- c("Book", "Triangles", "Reserves", "Uncertainty", "Backlog", "IFRS 17", "Methods", "Your data")
dir.create("assets/readme/pages", recursive = TRUE, showWarnings = FALSE)
options(chromote.timeout = 90)

open_app <- function(width, height, scale = 1) {
  b <- chromote::ChromoteSession$new(width = width, height = height)
  b$Emulation$setDeviceMetricsOverride(width = width, height = height, deviceScaleFactor = scale, mobile = width < 700)
  # A documentation address (TEST-NET-3), so a local run shows the Your data page as a visitor sees it.
  b$Network$enable()
  b$Network$setExtraHTTPHeaders(headers = list(`X-Forwarded-For` = "203.0.113.10"))
  b$Page$navigate(url, wait_ = FALSE)
  for (i in 1:60) {
    Sys.sleep(1)
    ok <- tryCatch(b$Runtime$evaluate("!!document.querySelector('.kpi .value')")$result$value, error = function(e) FALSE)
    if (isTRUE(ok)) break
  }
  b
}
show_page <- function(b, page) {
  b$Runtime$evaluate(sprintf("document.querySelector('a.nav-link[data-value=\"%s\"]').click(); window.scrollTo(0, 0);", page))
  Sys.sleep(3)
  for (k in 1:40) {
    busy <- b$Runtime$evaluate("document.documentElement.classList.contains('shiny-busy') || document.querySelectorAll('.recalculating').length > 0")$result$value
    if (!isTRUE(busy)) break
    Sys.sleep(1)
  }
  Sys.sleep(2)
}

# Every page above the fold, on a laptop.
b <- open_app(1360, 860)
for (i in seq_along(pages)) {
  show_page(b, pages[i])
  f <- sprintf("assets/readme/pages/%d-%s.png", i, tolower(gsub(" ", "", pages[i])))
  b$screenshot(f, cliprect = c(0, 0, 1360, 860), wait_ = TRUE)
  message("saved ", f)
}
show_page(b, "Backlog")
b$screenshot("assets/readme/hero_backlog.png", cliprect = c(0, 0, 1360, 790), wait_ = TRUE)
b$close()

# The Book page on a phone.
b <- open_app(390, 844, scale = 2)
Sys.sleep(3)
b$screenshot("assets/readme/phone_book.png", cliprect = c(0, 0, 390, 844), wait_ = TRUE)
b$close()

# The hero: both pictures laid out by scripts/readme_hero.html.
b <- chromote::ChromoteSession$new(width = 1600, height = 860)
b$Page$navigate(paste0("file:///", normalizePath("scripts/readme_hero.html", winslash = "/")), wait_ = FALSE)
Sys.sleep(4)
b$screenshot("assets/readme/hero.png", cliprect = c(0, 0, 1600, 860), wait_ = TRUE)
b$close()
message("saved assets/readme/hero.png")
