# Reads config.yml, and turns its inline comments into a table for the Methods page.

read_config <- function(path = "config.yml") {
  cfg <- yaml::read_yaml(path)
  cfg$first_accident_month <- as.Date(cfg$first_accident_month)
  cfg$valuation_date <- as.Date(cfg$valuation_date)
  cfg
}

# One row per value: dotted parameter name, value as written, and its comment.
config_table <- function(path = "config.yml") {
  lines <- readLines(path, warn = FALSE)
  stack <- character(0)
  indents <- integer(0)
  rows <- list()
  for (ln in lines) {
    if (!nzchar(trimws(ln)) || grepl("^\\s*#", ln)) next
    m <- regmatches(ln, regexec("^(\\s*)([A-Za-z0-9_]+):\\s*([^#]*?)\\s*(#\\s*(.*))?$", ln))[[1]]
    if (length(m) == 0) next
    ind <- nchar(m[2]); key <- m[3]; val <- m[4]; com <- m[6]
    keep <- indents < ind
    stack <- stack[keep]; indents <- indents[keep]
    if (!nzchar(val)) {
      stack <- c(stack, key); indents <- c(indents, ind)
      next
    }
    rows[[length(rows) + 1]] <- data.frame(
      parameter = paste(c(stack, key), collapse = "."),
      value = val, comment = com, stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

# Month index: the first accident month is 1.
month_index <- function(d, first) {
  d <- as.POSIXlt(d); f <- as.POSIXlt(first)
  (d$year - f$year) * 12L + (d$mon - f$mon) + 1L
}

index_to_month <- function(i, first) {
  f <- as.POSIXlt(first)
  m0 <- f$year * 12L + f$mon + (i - 1L)
  as.Date(sprintf("%04d-%02d-01", m0 %/% 12L + 1900L, m0 %% 12L + 1L))
}
