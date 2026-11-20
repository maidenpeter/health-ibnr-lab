# Your data: read a claims extract, check it, build triangles and run the reserving methods on it.
# Every step goes into a run log the user can download. Files are read in memory and never saved.
# Months are handled as integer keys, year * 12 + month - 1, until they are turned back into dates.

UPLOAD_CLAIM_FIELDS <- data.frame(
  field = c("accident_month", "payment_month", "amount_paid_kes", "report_month", "amount_billed_kes", "benefit_type"),
  need = c("Required", "Required", "Required", "Recommended", "Recommended", "Optional"),
  format = c("YYYY-MM or YYYY-MM-DD", "YYYY-MM or YYYY-MM-DD; blank if not yet paid", "Number in KES",
             "YYYY-MM or YYYY-MM-DD", "Number in KES", "Text"),
  meaning = c(
    "Month the treatment happened: the date of service, or the admission date for inpatient.",
    "Month the claim was paid. For a rejected claim, the month it was closed.",
    "Amount paid. 0 for a rejected claim; 0 or blank if not yet paid.",
    "Month the invoice reached you. Needed for the received triangle and the backlog check.",
    "Amount on the invoice. Needed for the received triangle and the backlog check.",
    "For example outpatient, inpatient, maternity, chronic. Gives each benefit its own settlement ratio."),
  stringsAsFactors = FALSE)

UPLOAD_PREMIUM_FIELDS <- data.frame(
  field = c("accident_month", "earned_premium_kes"),
  need = c("Required", "Required"),
  format = c("YYYY-MM or YYYY-MM-DD", "Number in KES"),
  meaning = c("Month the premium was earned.", "Premium earned in that month, all schemes together."),
  stringsAsFactors = FALSE)

UPLOAD_DISCLAIMER <- paste(
  "This tool has not been reviewed against real industry data, and no actuary has signed off its results.",
  "It is a portfolio project. Its numbers are for discussion only and are not an actuarial opinion,",
  "a reserve to book, or advice.")

# Run log ---------------------------------------------------------------------

now_eat <- function() as.POSIXlt(Sys.time(), tz = "Africa/Nairobi")

run_log <- function() {
  e <- new.env()
  e$started <- now_eat()
  e$ref <- paste0("HIL-", format(e$started, "%Y%m%d-%H%M%S-"),
                  paste(sample(c(0:9, LETTERS[1:6]), 4, replace = TRUE), collapse = ""))
  e$entries <- data.frame(time = character(0), step = character(0), detail = character(0), stringsAsFactors = FALSE)
  e$add <- function(step, ...) {
    msg <- paste0(...)
    e$entries[nrow(e$entries) + 1, ] <- list(format(now_eat(), "%Y-%m-%d %H:%M:%S"), step, msg)
    invisible(msg)
  }
  e
}

log_text <- function(lg, contact) {
  wrap <- function(x, indent = 0) paste(strwrap(x, 100, exdent = indent), collapse = "\n")
  steps <- vapply(seq_len(nrow(lg$entries)), function(i) {
    e <- lg$entries[i, ]
    paste0(e$time, "  ", formatC(e$step, width = -10), " ",
           paste(strwrap(e$detail, 100 - 32, exdent = 0), collapse = paste0("\n", strrep(" ", 32))))
  }, character(1))
  c(
    "Health IBNR Lab: run log",
    "========================",
    paste("Reference:", lg$ref),
    paste("Log written:", format(now_eat(), "%Y-%m-%d %H:%M:%S"), "East Africa Time (UTC+3)"),
    "App: https://maidenpeter.shinyapps.io/health-ibnr-lab/",
    paste0("Software: ", R.version.string, ", ChainLadder ", as.character(utils::packageVersion("ChainLadder")),
           ", data.table ", as.character(utils::packageVersion("data.table"))),
    "",
    "Disclaimer",
    "----------",
    wrap(UPLOAD_DISCLAIMER),
    wrap(paste("To test it against a reserve that was actually booked, write to", contact)),
    "",
    "What was done",
    "-------------",
    if (length(steps)) steps else "Nothing yet.",
    ""
  )
}

# Parsing ---------------------------------------------------------------------

norm_names <- function(x) gsub("^_+|_+$", "", gsub("[^a-z0-9]+", "_", tolower(trimws(x))))

month_key_to_date <- function(k) as.Date(sprintf("%04d-%02d-01", k %/% 12L, k %% 12L + 1L))
month_key_label <- function(k) format(month_key_to_date(k), "%b %Y")

# Month keys from YYYY-MM, YYYY-MM-DD, YYYY/MM or YYYY/MM/DD, with an optional time after the date.
# Blank gives NA; so does anything else, which the checks report as unreadable.
parse_month <- function(x) {
  u <- unique(trimws(as.character(x)))
  pat <- "^([0-9]{4})[-/]([0-9]{1,2})(?:[-/]([0-9]{1,2}))?(?:[ T].*)?$"
  ok <- !is.na(u) & grepl(pat, u, perl = TRUE)
  key <- rep(NA_integer_, length(u))
  y <- as.integer(sub(pat, "\\1", u[ok], perl = TRUE))
  m <- as.integer(sub(pat, "\\2", u[ok], perl = TRUE))
  d <- sub(pat, "\\3", u[ok], perl = TRUE)
  dd <- suppressWarnings(as.integer(ifelse(nzchar(d), d, "1")))
  good <- m >= 1 & m <= 12 & y >= 1990 & y <= 2100 & dd >= 1 & dd <= 31
  key[which(ok)[good]] <- y[good] * 12L + m[good] - 1L
  key[match(trimws(as.character(x)), u)]
}

# Amounts with or without thousands separators and a KES or KSh prefix.
parse_amount <- function(x) {
  u <- unique(trimws(as.character(x)))
  v <- gsub("^(kes|kshs?)\\.?\\s*", "", u, ignore.case = TRUE)
  v <- gsub("[, ]", "", v)
  out <- suppressWarnings(as.numeric(v))
  out[match(trimws(as.character(x)), u)]
}

is_blank <- function(x) is.na(x) | !nzchar(trimws(as.character(x)))

# "1 rows are" reads badly; the checks write counts with sprintf, so fix the singular afterwards.
one_row_grammar <- function(x) {
  x <- gsub("\\b1 rows (\\([^)]*\\) )?are\\b", "1 row \\1is", x, perl = TRUE)
  x <- gsub("\\b1 rows (\\([^)]*\\) )?have\\b", "1 row \\1has", x, perl = TRUE)
  gsub("\\b1 rows\\b", "1 row", x, perl = TRUE)
}

rows_text <- function(i, n_show = 5) {
  i <- i + 1L  # the header is line 1 of the file
  paste0(if (length(i) == 1) "line " else "lines ", paste(utils::head(i, n_show), collapse = ", "),
         if (length(i) > n_show) sprintf(" and %s more", formatC(length(i) - n_show, big.mark = ",")) else "")
}

read_upload_csv <- function(path, max_rows) {
  data.table::fread(path, colClasses = "character", na.strings = c("", "NA", "N/A", "NULL"),
                    nrows = max_rows + 1, showProgress = FALSE, encoding = "UTF-8")
}

# Excel uploads: the first sheet, read as text in one pass. A date typed into a month column arrives as Excel's
# serial day number and is turned back into YYYY-MM-DD, the form parse_month reads (a date and time keeps its
# date), so an Excel file goes through exactly the same checks as a CSV.
XLSX_TYPE <- "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
is_excel_name <- function(name) endsWith(tolower(name), ".xlsx")
read_upload_excel <- function(path, max_rows) {
  x <- data.table::as.data.table(readxl::read_excel(path, sheet = 1, col_types = "text", n_max = max_rows + 1,
                                                    .name_repair = "minimal"))
  for (k in names(x)) {
    v <- x[[k]]
    v[!is.na(v) & trimws(v) %in% c("", "NA", "N/A", "NULL")] <- NA_character_
    if (grepl("month$", norm_names(k))) {
      serial <- !is.na(v) & grepl("^[0-9]{5}([.][0-9]+)?$", v)
      if (any(serial)) v[serial] <- format(as.Date(floor(as.numeric(v[serial])), origin = "1899-12-30"), "%Y-%m-%d")
    }
    data.table::set(x, j = k, value = v)
  }
  x
}
read_upload <- function(path, name, max_rows) {
  if (is_excel_name(name)) read_upload_excel(path, max_rows) else read_upload_csv(path, max_rows)
}

# Downloads lead with Excel (and the log with HTML); the CSV versions stay for anyone who wants them.
template_sheets <- function(fields) {
  blank <- as.data.frame(stats::setNames(replicate(nrow(fields), character(0), simplify = FALSE), fields$field),
                         stringsAsFactors = FALSE, check.names = FALSE)
  list(Template = blank, Fields = fields)
}
sample_sheet <- function(path) {
  x <- data.table::fread(path, colClasses = "character", na.strings = c("", "NA"), showProgress = FALSE)
  for (k in grep("_kes$", names(x), value = TRUE)) data.table::set(x, j = k, value = parse_amount(x[[k]]))
  as.data.frame(x)
}
log_html <- function(lines) {
  esc <- function(s) gsub(">", "&gt;", gsub("<", "&lt;", gsub("&", "&amp;", s, fixed = TRUE), fixed = TRUE), fixed = TRUE)
  paste0("<!doctype html>\n<html lang=\"en\"><head><meta charset=\"utf-8\"><title>Health IBNR Lab: run log</title>",
         "<style>body{font:14px/1.5 Consolas,Menlo,monospace;margin:2em;color:#1d2430}pre{white-space:pre-wrap}</style>",
         "</head><body><pre>", paste(esc(lines), collapse = "\n"), "</pre></body></html>")
}

file_fingerprint <- function(path) digest::digest(file = path, algo = "sha256")

# Checks ----------------------------------------------------------------------

# raw: the claims file as read (all columns text). premium_raw: the premium file or NULL.
# valuation_key: a month key, or NULL for the latest payment month in the file.
# Returns the cleaned rows, the checks, and ok = FALSE if any check failed.
prepare_upload <- function(raw, premium_raw, cfg, lg, valuation_key = NULL) {
  up <- cfg$upload
  checks <- data.frame(status = character(0), check = character(0), detail = character(0), stringsAsFactors = FALSE)
  add <- function(status, check, detail) {
    detail <- one_row_grammar(detail)
    checks[nrow(checks) + 1, ] <<- list(status, check, detail)
    lg$add(if (status == "Failed") "Failed" else if (status == "Warning") "Warning" else "Check", check, ": ", detail)
  }
  out <- list(ok = FALSE, checks = NULL)
  finish <- function() { out$checks <- checks; out$ok <- !any(checks$status == "Failed"); out }

  raw <- as.data.frame(raw, stringsAsFactors = FALSE)
  names(raw) <- norm_names(names(raw))
  known <- UPLOAD_CLAIM_FIELDS$field
  required <- known[UPLOAD_CLAIM_FIELDS$need == "Required"]
  missing <- setdiff(required, names(raw))
  if (length(missing)) {
    add("Failed", "Required columns", paste0("Missing: ", paste(missing, collapse = ", "),
        ". Column names must match the template; capitals and spaces are ignored."))
    return(finish())
  }
  add("Passed", "Required columns", paste0(paste(required, collapse = ", "), " found."))
  extra <- setdiff(names(raw), known)
  if (length(extra)) add("Passed", "Other columns", paste0("Dropped on arrival and not used: ", paste(extra, collapse = ", "), "."))
  raw <- raw[, intersect(known, names(raw)), drop = FALSE]

  n <- nrow(raw)
  if (n == 0) { add("Failed", "Rows", "The file has no data rows."); return(finish()) }
  if (n > up$max_rows) {
    add("Failed", "Rows", sprintf("More than %s rows. Send monthly totals by accident and payment month instead, or the latest %d accident months only.",
        formatC(up$max_rows, big.mark = ","), as.integer(up$max_accident_months)))
    return(finish())
  }
  add("Passed", "Rows", paste0(formatC(n, big.mark = ","), " rows read."))

  acc <- parse_month(raw$accident_month)
  bad <- which(is.na(acc))
  if (length(bad)) add("Failed", "accident_month", sprintf("%s rows are blank or not a month (%s). Use YYYY-MM or YYYY-MM-DD.",
                       formatC(length(bad), big.mark = ","), rows_text(bad)))
  pay <- parse_month(raw$payment_month)
  bad_pay <- which(is.na(pay) & !is_blank(raw$payment_month))
  if (length(bad_pay)) add("Failed", "payment_month", sprintf("%s rows are not a month (%s). Use YYYY-MM or YYYY-MM-DD, or leave blank if not yet paid.",
                           formatC(length(bad_pay), big.mark = ","), rows_text(bad_pay)))
  paid <- parse_amount(raw$amount_paid_kes)
  bad_amt <- which(is.na(paid) & !is_blank(raw$amount_paid_kes))
  if (length(bad_amt)) add("Failed", "amount_paid_kes", sprintf("%s rows are not a number (%s).",
                           formatC(length(bad_amt), big.mark = ","), rows_text(bad_amt)))
  if (any(checks$status == "Failed")) return(finish())
  add("Passed", "Months and amounts", "Every accident month, payment month and amount paid can be read.")

  paid[is.na(paid)] <- 0
  no_date <- which(is.na(pay) & paid != 0)
  if (length(no_date)) add("Warning", "Paid with no payment month", sprintf(
    "%s rows (KES %s) have an amount paid but no payment month (%s). They are left out of the paid triangle.",
    formatC(length(no_date), big.mark = ","), formatC(sum(paid[no_date]), format = "f", digits = 0, big.mark = ","), rows_text(no_date)))
  early <- which(!is.na(pay) & pay < acc)
  if (length(early)) add("Warning", "Paid before the accident month", sprintf(
    "%s rows (%s) are paid before their accident month. They are left out of the paid triangle.",
    formatC(length(early), big.mark = ","), rows_text(early)))
  neg <- which(paid < 0)
  if (length(neg)) add("Warning", "Negative amounts", sprintf(
    "%s rows (KES %s) are negative, for example recoveries or reversals. They are kept.",
    formatC(length(neg), big.mark = ","), formatC(sum(paid[neg]), format = "f", digits = 0, big.mark = ",")))
  pay[early] <- NA_integer_

  # Received view: needs both the report month and the billed amount.
  has_rep <- all(c("report_month", "amount_billed_kes") %in% names(raw))
  rpt <- billed <- NULL
  if (!has_rep) {
    add("Warning", "Received view", paste("report_month and amount_billed_kes are not both in the file, so there is no",
        "received triangle, no pipeline and no backlog check. Paid chain ladder, Mack and the bootstrap still run."))
  } else {
    rpt <- parse_month(raw$report_month)
    billed <- parse_amount(raw$amount_billed_kes)
    bad_r <- which(is.na(rpt) | is.na(billed) | rpt < acc)
    if (length(bad_r)) add("Warning", "Received view", sprintf(
      "%s rows have a blank or unreadable report month or billed amount, or arrive before their accident month (%s). They are left out of the received triangle.",
      formatC(length(bad_r), big.mark = ","), rows_text(bad_r)))
    else add("Passed", "Received view", "Every row has a report month and a billed amount.")
    rpt[bad_r] <- NA_integer_
    late_pay <- which(!is.na(rpt) & !is.na(pay) & pay < rpt)
    if (length(late_pay)) add("Warning", "Paid before the invoice arrived", sprintf(
      "%s rows are paid before their report month (%s), which can happen with pre-authorised claims. They are kept.",
      formatC(length(late_pay), big.mark = ","), rows_text(late_pay)))
  }
  benefit <- if ("benefit_type" %in% names(raw)) {
    b <- tolower(trimws(raw$benefit_type)); b[is_blank(b)] <- "not given"; b
  } else rep_len("all", n)

  # Valuation month and the accident months in the triangle.
  if (all(is.na(pay))) { add("Failed", "Payments", "No row has a payment month, so there is nothing to develop."); return(finish()) }
  latest_pay <- max(pay, na.rm = TRUE)
  val <- if (is.null(valuation_key)) latest_pay else as.integer(valuation_key)
  add("Passed", "Valuation month", sprintf("%s%s. Payments and invoices after it are treated as not yet happened.",
      month_key_label(val), if (is.null(valuation_key)) ", the latest payment month in the file" else ", chosen by you"))
  after <- which(acc > val)
  if (length(after)) add("Warning", "Accident months after valuation", sprintf("%s rows have an accident month after %s and are left out.",
                         formatC(length(after), big.mark = ","), month_key_label(val)))
  first <- max(min(acc), val - as.integer(up$max_accident_months) + 1L)
  older <- which(acc < first)
  if (length(older)) add("Warning", "History", sprintf("Only the latest %d accident months are used. %s rows from before %s are left out.",
                         as.integer(up$max_accident_months), formatC(length(older), big.mark = ","), month_key_label(first)))
  n_acc <- val - first + 1L
  if (n_acc < up$min_accident_months) {
    add("Failed", "History", sprintf("%d accident months up to valuation. At least %d are needed to build a triangle.",
        n_acc, as.integer(up$min_accident_months)))
    return(finish())
  }
  add("Passed", "History", sprintf("%d accident months, %s to %s.", n_acc, month_key_label(first), month_key_label(val)))
  keep <- acc >= first & acc <= val
  counts <- tabulate(acc[keep] - first + 1L, n_acc)
  if (any(counts == 0)) add("Warning", "Empty months", sprintf("No claims at all in %s. Check the extract covers every month.",
                            paste(month_key_label(first - 1L + which(counts == 0)), collapse = ", ")))
  n_after_val <- sum(keep & !is.na(pay) & pay > val)
  if (n_after_val) add("Passed", "Payments after valuation", sprintf("%s rows are paid after %s and count as not yet paid.",
                       formatC(n_after_val, big.mark = ","), month_key_label(val)))

  rows <- data.frame(acc_idx = acc[keep] - first + 1L, pay_idx = pay[keep] - first + 1L,
                     amount_paid_kes = paid[keep], benefit_type = benefit[keep], stringsAsFactors = FALSE)
  if (has_rep) {
    rows$report_idx <- rpt[keep] - first + 1L
    rows$amount_billed_kes <- billed[keep]
  }

  # Premium, for Bornhuetter-Ferguson.
  premium <- NULL
  if (is.null(premium_raw)) {
    add("Passed", "Premium", "No premium file, so Bornhuetter-Ferguson is not run.")
  } else {
    pr <- as.data.frame(premium_raw, stringsAsFactors = FALSE)
    names(pr) <- norm_names(names(pr))
    if (!all(UPLOAD_PREMIUM_FIELDS$field %in% names(pr))) {
      add("Warning", "Premium", paste0("The premium file needs accident_month and earned_premium_kes. ",
          "Bornhuetter-Ferguson is not run."))
    } else {
      pk <- parse_month(pr$accident_month); pv <- parse_amount(pr$earned_premium_kes)
      ok <- !is.na(pk) & !is.na(pv)
      prem <- tapply(pv[ok], pk[ok], sum)
      want <- first:val
      got <- prem[as.character(want)]
      if (any(is.na(got)) || any(got <= 0)) {
        add("Warning", "Premium", sprintf("No positive premium for %s. Bornhuetter-Ferguson is not run.",
            paste(month_key_label(want[is.na(got) | got <= 0]), collapse = ", ")))
      } else {
        premium <- as.numeric(got)
        add("Passed", "Premium", sprintf("Earned premium for all %d accident months, KES %s in total.",
            n_acc, formatC(sum(premium), format = "f", digits = 0, big.mark = ",")))
      }
    }
  }

  out$rows <- rows
  out$first_key <- first
  out$val_key <- val
  out$latest_pay_key <- latest_pay
  out$n_acc <- n_acc
  out$has_received <- has_rep
  out$premium <- premium
  out$months <- month_key_to_date(first + seq_len(n_acc) - 1L)
  finish()
}

# Methods on the uploaded data --------------------------------------------------

# Settlement ratio by benefit type from claims settled by valuation; one overall ratio for thin benefit types.
settlement_ratios <- function(rows, val_idx, min_claims) {
  s <- rows[!is.na(rows$report_idx) & !is.na(rows$pay_idx) & rows$pay_idx <= val_idx & rows$amount_billed_kes > 0, ]
  overall <- sum(s$amount_paid_kes) / sum(s$amount_billed_kes)
  types <- unique(rows$benefit_type)
  ratios <- stats::setNames(rep(overall, length(types)), types)
  n_by <- table(s$benefit_type)
  own <- names(n_by)[n_by >= min_claims]
  for (b in own) ratios[b] <- sum(s$amount_paid_kes[s$benefit_type == b]) / sum(s$amount_billed_kes[s$benefit_type == b])
  list(overall = overall, by_benefit = ratios, own = own)
}

# prep: from prepare_upload. elr: BF loss ratio, or NULL for the default from mature accident months.
run_upload <- function(prep, cfg, lg, elr = NULL, boot_R = cfg$upload$bootstrap_R) {
  up <- cfg$upload
  rows <- prep$rows
  n <- prep$n_acc
  val <- n
  far <- val + 1e6L  # unpaid or unreported: an index no triangle reaches
  pay_idx <- ifelse(is.na(rows$pay_idx), far, rows$pay_idx)
  t0 <- Sys.time()
  secs <- function() sprintf("%.1f s", as.numeric(difftime(Sys.time(), t0, units = "secs")))

  inc <- incremental_triangle(rows$acc_idx, pay_idx, rows$amount_paid_kes, n, n, val)
  P <- cumulate(inc)
  in_tri <- sum(rows$amount_paid_kes[pay_idx <= val])
  lg$add("Triangle", sprintf("Paid triangle: %d accident months by %d development months. Total KES %s; it matches the rows paid by valuation to within KES %s.",
         n, n, formatC(sum(inc, na.rm = TRUE), format = "f", digits = 0, big.mark = ","),
         formatC(abs(sum(inc, na.rm = TRUE) - in_tri), format = "f", digits = 0, big.mark = ",")))

  f <- cl_factors(P)
  cl <- chain_ladder(P, f)
  lg$add("Method", sprintf("Chain ladder: volume weighted factors on all diagonals, no tail factor. Outstanding KES %s on paid to date KES %s.",
         formatC(sum(cl$reserve), format = "f", digits = 0, big.mark = ","), formatC(sum(cl$latest), format = "f", digits = 0, big.mark = ",")))
  neg_inc <- sum(inc < 0, na.rm = TRUE)
  if (neg_inc) lg$add("Warning", sprintf("%d cells of the incremental paid triangle are negative. Mack and the bootstrap may struggle with them.", neg_inc))

  mack <- tryCatch(mack_fit(P), error = function(e) { lg$add("Warning", "Mack could not be fitted: ", conditionMessage(e)); NULL })
  if (!is.null(mack)) lg$add("Method", sprintf("Mack (ChainLadder::MackChainLadder, Mack's sigma for the last column): standard error KES %s, coefficient of variation %s.",
                             formatC(mack$total_se, format = "f", digits = 0, big.mark = ","), fmt_pct(mack$total_se / mack$total_reserve)))

  boot <- tryCatch(bootstrap_fit(P, boot_R, cfg$bootstrap$process_distr, cfg$bootstrap$seed),
                   error = function(e) { lg$add("Warning", "The bootstrap could not be run: ", conditionMessage(e)); NULL })
  if (!is.null(boot)) {
    if (boot$n_nan > 0) lg$add("Warning", sprintf("The bootstrap returned %d non-finite totals out of %d; they are dropped.", boot$n_nan, boot_R))
    tt <- boot$totals[is.finite(boot$totals)]
    boot$q <- stats::quantile(tt, c(0.5, 0.75, 0.995), names = FALSE)
    boot$mean <- mean(tt)
    lg$add("Method", sprintf("ODP bootstrap (ChainLadder::BootChainLadder, %s replicates, %s process distribution, seed %s): mean KES %s, 50th percentile KES %s, 75th KES %s, 99.5th KES %s.",
           formatC(boot_R, big.mark = ","), cfg$bootstrap$process_distr, cfg$bootstrap$seed,
           formatC(boot$mean, format = "f", digits = 0, big.mark = ","), formatC(boot$q[1], format = "f", digits = 0, big.mark = ","),
           formatC(boot$q[2], format = "f", digits = 0, big.mark = ","), formatC(boot$q[3], format = "f", digits = 0, big.mark = ",")))
  }

  paid_cal <- vapply(seq_len(val), function(t) sum(rows$amount_paid_kes[pay_idx == t]), numeric(1))
  window <- min(as.integer(cfg$methods$months_of_claims_window), val)
  avg_paid <- mean(utils::tail(paid_cal, window))

  out <- list(P = P, f = f, cl = cl, mack = mack, boot = boot, boot_R = boot_R, months = prep$months, n_acc = n,
              val_key = prep$val_key, paid_cal = paid_cal, avg_paid = avg_paid, window = window)

  # Received view, pipeline and the backlog check.
  sr <- if (prep$has_received) settlement_ratios(rows, val, up$min_claims_for_ratio)
  if (prep$has_received && !is.finite(sr$overall)) {
    lg$add("Warning", "No claim with a billed amount was settled by valuation, so there is no settlement ratio and no received view.")
  }
  if (prep$has_received && is.finite(sr$overall)) {
    received <- unname(rows$amount_billed_kes * sr$by_benefit[rows$benefit_type])
    rep_idx <- ifelse(is.na(rows$report_idx), far, rows$report_idx)
    received[rep_idx == far] <- 0
    lg$add("Method", sprintf("Settlement ratio from claims settled by valuation: %s overall%s.", fmt_pct(sr$overall),
           if (length(sr$own)) paste0("; own ratios for ", paste(sprintf("%s %s", sr$own, fmt_pct(sr$by_benefit[sr$own])), collapse = ", ")) else ""))
    Rc <- cumulate(incremental_triangle(rows$acc_idx, rep_idx, received, n, n, val))
    f_rec <- cl_factors(Rc)
    rcl <- chain_ladder(Rc, f_rec)
    pipe_k <- rep_idx <= val & pay_idx > val
    pipeline <- numeric(n)
    if (any(pipe_k)) { s <- rowsum(received[pipe_k], rows$acc_idx[pipe_k]); pipeline[as.integer(rownames(s))] <- s[, 1] }
    lg$add("Method", sprintf("Received claims chain ladder: ultimate KES %s, so outstanding KES %s against paid to date. Pipeline (received, not yet paid) KES %s.",
           formatC(sum(rcl$ultimate), format = "f", digits = 0, big.mark = ","),
           formatC(sum(rcl$ultimate) - sum(cl$latest), format = "f", digits = 0, big.mark = ","),
           formatC(sum(pipeline), format = "f", digits = 0, big.mark = ",")))

    avg_n <- as.integer(cfg$diagnostics$pipeline_avg_months)
    tr_months <- max(avg_n, val - 11L):val
    trend <- data.frame(cal_idx = tr_months,
      pipeline = vapply(tr_months, function(t) sum(received[rep_idx <= t & pay_idx > t]), numeric(1)),
      avg_paid = vapply(tr_months, function(t) mean(paid_cal[(t - avg_n + 1L):t]), numeric(1)))
    trend$months_of_paid <- trend$pipeline / trend$avg_paid

    gap <- as.integer(cfg$diagnostics$ae_fit_gap_months)
    ae <- NULL
    if (val > gap + 6L) {
      cfg_ae <- cfg
      cfg_ae$max_payment_lag_months <- n - 1L
      cfg_ae$diagnostics$ae_show_months <- min(12L, val - gap)
      ae <- tryCatch(ae_diagnostic(P, Rc, cfg_ae, val)$by_cal, error = function(e) NULL)
    }
    if (!is.null(ae)) {
      k3 <- utils::tail(seq_len(nrow(ae)), 3)
      lg$add("Check", sprintf("Backlog check: actual paid in the latest 3 months was %s of expected from received claims; in the %d months before, between %s and %s.",
             fmt_pct(sum(ae$actual[k3]) / sum(ae$expected[k3])), nrow(ae) - 3L, fmt_pct(min(ae$ae[-k3])), fmt_pct(max(ae$ae[-k3]))))
    }
    lg$add("Check", sprintf("Pipeline at valuation is %s months of paid claims; 12 months earlier it was %s.",
           formatC(utils::tail(trend$months_of_paid, 1), format = "f", digits = 1),
           formatC(trend$months_of_paid[1], format = "f", digits = 1)))
    out <- c(out, list(Rc = Rc, rcl = rcl, pipeline = pipeline, trend = trend, ae = ae, ratios = sr))
  }

  # BF, if there is premium.
  if (!is.null(prep$premium)) {
    out$premium <- prep$premium
    mature <- which(cl$dev > as.integer(up$mature_after_months))
    out$elr_default <- if (length(mature)) sum(cl$ultimate[mature]) / sum(prep$premium[mature]) else sum(cl$ultimate) / sum(prep$premium)
    out$elr_basis <- if (length(mature)) sprintf("chain ladder loss ratio of the %d accident months more than %d months old",
                                                 length(mature), as.integer(up$mature_after_months)) else "chain ladder loss ratio of all accident months"
    lg$add("Method", sprintf("Bornhuetter-Ferguson prior loss ratio starts at %s, the %s. You can change it on the page.",
           fmt_pct(out$elr_default), out$elr_basis))
    out$elr <- if (is.null(elr)) out$elr_default else elr
  }
  lg$add("Run", "Finished in ", secs(), ".")
  out
}

# BF on an upload run at a given loss ratio.
upload_bf <- function(run, elr) bornhuetter_ferguson(run$P, run$f, run$premium, elr)

# One row per accident month, plus a total row, for the page and the CSV download.
upload_results_table <- function(run, elr = run$elr) {
  cl <- run$cl
  df <- data.frame(accident_month = format(run$months, "%Y-%m"), paid_to_date_kes = cl$latest,
                   development_months = cl$dev, cdf_to_ultimate = cl$cdf,
                   cl_ultimate_kes = cl$ultimate, cl_outstanding_kes = cl$reserve, stringsAsFactors = FALSE)
  if (!is.null(run$mack)) df$mack_se_kes <- run$mack$by_origin$mack_se
  if (!is.null(run$premium)) {
    bf <- upload_bf(run, elr)
    df$earned_premium_kes <- run$premium
    df$bf_ultimate_kes <- bf$ultimate
    df$bf_outstanding_kes <- bf$reserve
  }
  if (!is.null(run$rcl)) {
    df$received_cl_ultimate_kes <- run$rcl$ultimate
    df$pipeline_kes <- run$pipeline
  }
  tot <- df[1, ]
  tot[1, ] <- NA
  tot$accident_month <- "Total"
  sum_cols <- setdiff(names(df), c("accident_month", "development_months", "cdf_to_ultimate", "mack_se_kes"))
  for (c in sum_cols) tot[[c]] <- sum(df[[c]])
  if (!is.null(run$mack)) tot$mack_se_kes <- run$mack$total_se
  rbind(df, tot)
}

upload_triangle_table <- function(run) {
  m <- run$P
  df <- data.frame(accident_month = format(run$months, "%Y-%m"), m, check.names = FALSE)
  names(df)[-1] <- paste0("dev_", seq_len(ncol(m)))
  df
}

# The sample claims extract: the synthetic book as it would look at valuation, in the upload format.
sample_extract <- function(claims, val_idx) {
  k <- claims$report_idx <= val_idx
  paid_by_val <- claims$pay_idx <= val_idx
  data.frame(
    accident_month = format(claims$accident_month[k], "%Y-%m"),
    report_month = format(claims$report_month[k], "%Y-%m"),
    payment_month = ifelse(paid_by_val[k], format(claims$payment_month[k], "%Y-%m"), ""),
    benefit_type = claims$benefit_type[k],
    amount_billed_kes = claims$amount_billed_kes[k],
    amount_paid_kes = ifelse(paid_by_val[k], claims$amount_paid_kes[k], 0),
    stringsAsFactors = FALSE)
}
