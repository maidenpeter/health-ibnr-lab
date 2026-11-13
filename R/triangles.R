# Paid and received triangles, and the pipeline (received but not yet paid).
# Rows are accident months, columns are development months. Development month 1 is the accident month itself.
# Cells after the valuation month are NA: nothing paid or reported after valuation enters a triangle.

expected_received <- function(claims, cfg) {
  claims$amount_billed_kes * unlist(cfg$settlement_ratio[claims$benefit_type])
}

incremental_triangle <- function(acc_idx, event_idx, amount, n_acc, n_dev, val_idx) {
  dev <- event_idx - acc_idx + 1L
  keep <- event_idx <= val_idx & dev <= n_dev
  m <- matrix(0, n_acc, n_dev)
  if (any(keep)) {
    s <- rowsum(amount[keep], (acc_idx[keep] - 1L) * n_dev + dev[keep])
    k <- as.integer(rownames(s))
    m[cbind((k - 1L) %/% n_dev + 1L, (k - 1L) %% n_dev + 1L)] <- s[, 1]
  }
  future <- outer(seq_len(n_acc), seq_len(n_dev), "+") - 1L > val_idx
  m[future] <- NA
  dimnames(m) <- list(origin = seq_len(n_acc), dev = seq_len(n_dev))
  m
}

cumulate <- function(incr) {
  out <- t(apply(incr, 1, cumsum))
  dimnames(out) <- dimnames(incr)
  out
}

decumulate <- function(cum) {
  out <- cum
  out[, -1] <- cum[, -1] - cum[, -ncol(cum)]
  out
}

paid_triangle <- function(claims, n_acc, n_dev, val_idx) {
  incremental_triangle(claims$acc_idx, claims$pay_idx, claims$amount_paid_kes, n_acc, n_dev, val_idx)
}

received_triangle <- function(claims, cfg, n_acc, n_dev, val_idx) {
  incremental_triangle(claims$acc_idx, claims$report_idx, expected_received(claims, cfg), n_acc, n_dev, val_idx)
}

# Latest observed value in each row of a cumulative triangle.
latest_diagonal <- function(cum) {
  apply(cum, 1, function(r) { r <- r[!is.na(r)]; if (length(r)) r[length(r)] else NA_real_ })
}

latest_dev <- function(cum) apply(cum, 1, function(r) sum(!is.na(r)))

# Received but not yet paid, at the end of each calendar month in months_idx.
pipeline_by_month <- function(claims, cfg, months_idx) {
  rec <- expected_received(claims, cfg)
  vapply(months_idx, function(t) sum(rec[claims$report_idx <= t & claims$pay_idx > t]), numeric(1))
}

# Pipeline at valuation by accident month.
pipeline_by_origin <- function(claims, cfg, n_acc, val_idx) {
  rec <- expected_received(claims, cfg)
  k <- claims$report_idx <= val_idx & claims$pay_idx > val_idx
  out <- numeric(n_acc)
  s <- rowsum(rec[k], claims$acc_idx[k])
  out[as.integer(rownames(s))] <- s[, 1]
  out
}

paid_by_calendar <- function(claims, months_idx) {
  vapply(months_idx, function(t) sum(claims$amount_paid_kes[claims$pay_idx == t]), numeric(1))
}
