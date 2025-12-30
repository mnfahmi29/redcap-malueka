# src/03_calibration_rules.R
# ------------------------------------------------------------
# Calibration rules (turn REDCap chaos into analysis-friendly data) :))
#
# Goal:
# - Convert Yes/No, checkbox, Likert, and numeric-ish strings into clean numeric
# - Create consistent "calibrated" columns (so later feature extraction is easy)
#
# Why this exists:
# REDCap exports are honest but not always analysis-ready:
# - "Yes"/"No" might be text
# - checkboxes might become multiple columns
# - numeric fields might come as "1,234" or "  5 "
# - Likert scales might be "Strongly Agree" ._. (depends on form design)
#
# This file gives you a *rule-based* way to calibrate variables:
# - define what to calibrate
# - apply it consistently across instances (e.g., *_1, *_2, *_3)
# ------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
})

# -----------------------------
# Small conversion helpers :)
# -----------------------------

yn_to_int <- function(x) {
  # Convert Yes/No-ish values into 1/0 (keeps NA as NA)
  # Accepts common variants: yes/no, y/n, true/false, 1/0, etc.
  x_chr <- tolower(trimws(as.character(x)))
  
  case_when(
    x_chr %in% c("yes", "y", "true", "1") ~ 1L,
    x_chr %in% c("no", "n", "false", "0") ~ 0L,
    x_chr %in% c("", "na", "n/a", "null") ~ NA_integer_,
    is.na(x_chr) ~ NA_integer_,
    TRUE ~ NA_integer_  # unknowns become NA (no guessing :'))
  )
}

clean_numeric <- function(x) {
  # Convert "1,234" / " 5 " / "5.0" into numeric
  # Anything weird becomes NA (better than silently wrong)
  x_chr <- as.character(x)
  x_chr <- str_replace_all(x_chr, ",", "")
  x_chr <- str_trim(x_chr)
  suppressWarnings(as.numeric(x_chr))
}

likert_to_int <- function(x, mapping) {
  # mapping: named integer vector like:
  # c("strongly disagree"=1, "disagree"=2, "neutral"=3, "agree"=4, "strongly agree"=5)
  x_chr <- tolower(str_trim(as.character(x)))
  out <- unname(mapping[x_chr])
  as.integer(out)
}

# ------------------------------------------------------------
# Main engine: apply calibration rules to wide REDCap data :)
# ------------------------------------------------------------

apply_calibration_rules <- function(df, rules) {
  # rules is a list, each element describes ONE calibration pattern:
  #
  # rules <- list(
  #   list(
  #     type    = "yn",
  #     pattern = "^smoker_\\d+$",      # regex to match columns across instances
  #     suffix  = "_cal"               # new columns appended with this suffix
  #   ),
  #   list(
  #     type    = "numeric",
  #     pattern = "^age_\\d+$",
  #     suffix  = "_num"
  #   )
  # )
  #
  # Supported types: "yn", "numeric", "likert"
  #
  # Output: df + new calibrated columns
  
  stopifnot(is.list(rules))
  
  for (r in rules) {
    stopifnot(!is.null(r$type), !is.null(r$pattern))
    
    type    <- r$type
    pattern <- r$pattern
    suffix  <- r$suffix %||% "_cal"
    
    cols <- grep(pattern, names(df), value = TRUE)
    if (length(cols) == 0) next  # nothing to do, move on :))
    
    # choose transformer
    if (type == "yn") {
      df <- df %>%
        mutate(across(all_of(cols), yn_to_int, .names = paste0("{.col}", suffix)))
      
    } else if (type == "numeric") {
      df <- df %>%
        mutate(across(all_of(cols), clean_numeric, .names = paste0("{.col}", suffix)))
      
    } else if (type == "likert") {
      mapping <- r$mapping
      if (is.null(mapping)) stop("likert rule needs r$mapping")
      df <- df %>%
        mutate(across(all_of(cols),
                      ~ likert_to_int(.x, mapping = mapping),
                      .names = paste0("{.col}", suffix)))
      
    } else {
      stop("Unknown calibration type: ", type)
    }
  }
  
  df
}

# ------------------------------------------------------------
# Checkbox note (important!) ._.
# ------------------------------------------------------------
# REDCap checkboxes usually export as multiple columns already:
#   symptoms___1, symptoms___2, symptoms___3 ...
# Often those are 0/1 already, but sometimes "Checked"/"Unchecked".
# If yours are text, you can calibrate them with type="yn" or a custom mapper.
#
# Example rule:
# list(type="yn", pattern="^symptoms___\\d+_\\d+$", suffix="_bin")
#
# (pattern depends on how your checkbox columns look after wide pivot)
# ------------------------------------------------------------

# tiny helper: provide default value for NULL (like %||% in purrr)
`%||%` <- function(a, b) if (!is.null(a)) a else b

# ------------------------------------------------------------
# Example usage (in run.R or your notebook) :))
# ------------------------------------------------------------
# source("src/03_calibration_rules.R")
#
# rules <- list(
#   list(type="yn",      pattern="^smoker_\\d+$",  suffix="_bin"),
#   list(type="numeric", pattern="^age_\\d+$",     suffix="_num"),
#   list(type="likert",  pattern="^pain_\\d+$",    suffix="_lik",
#        mapping = c(
#          "strongly disagree"=1,
#          "disagree"=2,
#          "neutral"=3,
#          "agree"=4,
#          "strongly agree"=5
#        ))
# )
#
# df <- apply_calibration_rules(df, rules)
# ------------------------------------------------------------
