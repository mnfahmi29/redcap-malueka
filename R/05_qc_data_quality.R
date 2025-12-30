# src/05_qc_data_quality.R
# ------------------------------------------------------------
# QC + Data Quality metrics (judge the data capturer work) :))
#
# Goal:
# Turn your cleaned + calibrated + feature-ready REDCap wide data
# into a *quality report* you can show in GitHub / dashboard.
#
# This script helps you answer:
# - Are there lots of missing values? (overall + per variable + per instance)
# - Are there suspicious patterns? (too many identical answers, impossible ranges)
# - Are there duplicates / weird ID issues?
# - Which fields are the biggest pain points for data entry?
#
# Design principle:
# QC should be *transparent* and *cheap to run*.
# No black magic. Just rules + summaries + simple flags ._.
# ------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(purrr)
})

# -----------------------------
# Tiny helpers :)
# -----------------------------

`%||%` <- function(a, b) if (!is.null(a)) a else b

is_blankish <- function(x) {
  # Treat "", "NA", "N/A", "null" as missing-ish (character fields)
  x_chr <- tolower(trimws(as.character(x)))
  is.na(x_chr) | x_chr %in% c("", "na", "n/a", "null")
}

# ------------------------------------------------------------
# 1) Missingness profile (overall + per column) :))
# ------------------------------------------------------------

qc_missingness <- function(df, id_col = "record_id") {
  stopifnot(is.data.frame(df))
  if (!id_col %in% names(df)) id_col <- NULL
  
  n <- nrow(df)
  p <- ncol(df)
  
  # column-wise missingness
  miss_by_col <- tibble(
    column = names(df),
    n_missing = map_int(df, ~ sum(is.na(.x) | is_blankish(.x))),
    pct_missing = if (n == 0) NA_real_ else n_missing / n
  ) %>%
    arrange(desc(pct_missing), desc(n_missing))
  
  # row-wise missingness (per patient/record)
  miss_by_row <- df %>%
    mutate(
      .row_missing_n = rowSums(across(everything(), ~ is.na(.x) | is_blankish(.x))),
      .row_missing_pct = .row_missing_n / p
    ) %>%
    select(any_of(id_col), .row_missing_n, .row_missing_pct) %>%
    arrange(desc(.row_missing_pct), desc(.row_missing_n))
  
  overall <- tibble(
    n_rows = n,
    n_cols = p,
    total_cells = n * p,
    total_missing = sum(miss_by_col$n_missing),
    pct_missing_overall = if (n * p == 0) NA_real_ else total_missing / (n * p)
  )
  
  list(
    overall = overall,
    by_col = miss_by_col,
    by_row = miss_by_row
  )
}

# ------------------------------------------------------------
# 2) Duplicate / ID sanity checks :))
# ------------------------------------------------------------

qc_id_checks <- function(df, id_col = "record_id") {
  if (!id_col %in% names(df)) {
    return(tibble(
      check = "id_col_present",
      status = "fail",
      details = paste0("Missing id_col: ", id_col)
    ))
  }
  
  dup_ids <- df %>%
    count(.data[[id_col]], name = "n") %>%
    filter(n > 1) %>%
    arrange(desc(n))
  
  tibble(
    check = c("id_col_present", "duplicate_ids"),
    status = c("pass", if (nrow(dup_ids) == 0) "pass" else "warn"),
    details = c(
      paste0("Found id_col: ", id_col),
      if (nrow(dup_ids) == 0) "No duplicate IDs detected"
      else paste0("Duplicate IDs: ", nrow(dup_ids), " (top shown in qc$duplicates$table)")
    )
  ) %>%
    mutate(extra = list(NULL, dup_ids))
}

# ------------------------------------------------------------
# 3) Range / validity rules (simple but powerful) ._.
# ------------------------------------------------------------
# You define rules like:
# rules <- list(
#   list(var = "age_1_num", min = 0, max = 120),
#   list(var = "bmi_1_num", min = 10, max = 70)
# )
# and we flag rows outside the range.

qc_range_rules <- function(df, rules) {
  stopifnot(is.list(rules))
  
  empty <- tibble::tibble(
    row_id = integer(),
    variable = character(),
    value = numeric(),
    is_missing = logical(),
    is_out_of_range = logical(),
    rule = character()
  )
  
  out_list <- list()
  
  for (r in rules) {
    var <- r$var
    if (is.null(var) || !var %in% names(df)) next
    
    minv <- r$min %||% -Inf
    maxv <- r$max %||%  Inf
    
    bad <- df %>%
      dplyr::transmute(
        row_id = dplyr::row_number(),
        variable = var,
        value = suppressWarnings(as.numeric(.data[[var]]))
      ) %>%
      dplyr::mutate(
        is_missing = is.na(value),
        is_out_of_range = !is_missing & (value < minv | value > maxv),
        rule = paste0("[", minv, ", ", maxv, "]")
      ) %>%
      dplyr::filter(is_out_of_range)
    
    out_list[[var]] <- bad
  }
  
  out <- dplyr::bind_rows(out_list)
  if (nrow(out) == 0) return(empty)
  
  out %>% dplyr::arrange(variable, row_id)
}


# ------------------------------------------------------------
# 4) “Too perfect” / low-variance checks (data entry smell test) :'))
# ------------------------------------------------------------
# If a column is basically constant, it might be:
# - truly constant clinically (fine)
# - or copy-pasted defaults / lazy entry (not fine)
#
# This checks:
# - number of unique values (excluding missing)
# - most common value dominance

qc_low_variance <- function(df, dominance_threshold = 0.95) {
  tibble(column = names(df)) %>%
    mutate(
      non_missing = map(column, ~ df[[.x]][!(is.na(df[[.x]]) | is_blankish(df[[.x]]))]),
      n_non_missing = map_int(non_missing, length),
      n_unique = map_int(non_missing, ~ length(unique(.x))),
      top_share = map_dbl(non_missing, ~ {
        if (length(.x) == 0) return(NA_real_)
        tab <- sort(table(.x), decreasing = TRUE)
        as.numeric(tab[1]) / sum(tab)
      })
    ) %>%
    select(-non_missing) %>%
    mutate(
      status = case_when(
        n_non_missing == 0 ~ "info",
        n_unique <= 1 ~ "warn",
        !is.na(top_share) & top_share >= dominance_threshold ~ "warn",
        TRUE ~ "pass"
      ),
      note = case_when(
        n_non_missing == 0 ~ "All missing (maybe not collected?)",
        n_unique <= 1 ~ "Nearly constant (check if intended or defaulted)",
        !is.na(top_share) & top_share >= dominance_threshold ~ paste0(
          "Dominant value share = ", round(top_share * 100, 1), "% (possible copy/paste)"
        ),
        TRUE ~ "Looks okay"
      )
    ) %>%
    arrange(desc(status == "warn"), desc(top_share))
}

# ------------------------------------------------------------
# 5) Main: run all QC and return a “qc object” you can save :))
# ------------------------------------------------------------

run_qc_data_quality <- function(
    df,
    id_col = "record_id",
    range_rules = list(),
    dominance_threshold = 0.95
) {
  missing <- qc_missingness(df, id_col = id_col)
  
  id_checks <- qc_id_checks(df, id_col = id_col)
  # extract duplicates table if present
  dup_tbl <- id_checks$extra[[2]] %||% tibble()
  
  range_tbl <- qc_range_rules(df, rules = range_rules)
  lowvar_tbl <- qc_low_variance(df, dominance_threshold = dominance_threshold)
  
  # Summarize into a lightweight “QC scoreboard”
  scoreboard <- tibble(
    metric = c(
      "overall_missing_pct",
      "n_cols_with_50pct_missing",
      "n_duplicate_ids",
      "n_range_violations",
      "n_warn_low_variance_cols"
    ),
    value = c(
      missing$overall$pct_missing_overall,
      sum(missing$by_col$pct_missing >= 0.50, na.rm = TRUE),
      nrow(dup_tbl),
      nrow(range_tbl),
      sum(lowvar_tbl$status == "warn", na.rm = TRUE)
    )
  )
  
  list(
    scoreboard = scoreboard,
    missing = missing,
    duplicates = list(checks = id_checks %>% select(check, status, details),
                      table = dup_tbl),
    range_violations = range_tbl,
    low_variance = lowvar_tbl
  )
}

# ------------------------------------------------------------
# Example usage (in run.R) :))
# ------------------------------------------------------------
# source("src/05_qc_data_quality.R")
#
# range_rules <- list(
#   list(var = "age_1_num", min = 0, max = 120),
#   list(var = "bmi_1_num", min = 10, max = 70)
# )
#
# qc <- run_qc_data_quality(
#   df,
#   id_col = "record_id",
#   range_rules = range_rules,
#   dominance_threshold = 0.95
# )
#
# qc$scoreboard
# qc$missing$by_col %>% head(20)
# qc$low_variance %>% filter(status == "warn") %>% head(20)
#
# saveRDS(qc, file = "results/qc/qc_report.rds")
# write.csv(qc$missing$by_col, "results/qc/missing_by_col.csv", row.names = FALSE)
# ------------------------------------------------------------
