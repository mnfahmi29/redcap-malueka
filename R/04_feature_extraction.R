# src/04_feature_extraction.R
# ------------------------------------------------------------
# Feature extraction (turn calibrated columns into usable signals) :)
#
# This file has TWO roles:
# (A) Numeric/checkbox feature builders (sum/mean/any/completeness)
# (B) Text feature extraction for messy clinical text (Diagnosis coding)
#     Common pitfalls --> The Diagnosis Text Feature only for CNS Tumors as 
#     Malueka works on it, you could create the same by exploring 
#     your diagnosis column too
#
# Why (B) matters:
# Your original workflow does:
# - detect Diagnosis_* columns
# - coalesce them into Diagnosis_text
# - filter brain/spine cases using keywords ("glioma", "meningioma", etc.)
# - assign Brain_code categories ("Glioma", "PCNSL", ...)
#
# So we package that as functions, so run.R stays clean :'))
# ------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(rlang)
})

`%||%` <- function(a, b) if (!is.null(a)) a else b

# =============================
# (A) Numeric feature helpers :)
# =============================

row_sum_safe <- function(df, cols, na_as_zero = FALSE) {
  if (length(cols) == 0) return(rep(NA_real_, nrow(df)))
  x <- df[, cols, drop = FALSE]
  if (na_as_zero) x[is.na(x)] <- 0
  out <- rowSums(as.matrix(x), na.rm = na_as_zero)
  if (!na_as_zero) {
    all_na <- apply(x, 1, function(r) all(is.na(r)))
    out[all_na] <- NA_real_
  }
  out
}

row_mean_safe <- function(df, cols, min_non_na = 1) {
  if (length(cols) == 0) return(rep(NA_real_, nrow(df)))
  x <- df[, cols, drop = FALSE]
  nn <- apply(x, 1, function(r) sum(!is.na(r)))
  out <- rowMeans(as.matrix(x), na.rm = TRUE)
  out[nn < min_non_na] <- NA_real_
  out
}

row_count_non_na <- function(df, cols) {
  if (length(cols) == 0) return(rep(0L, nrow(df)))
  x <- df[, cols, drop = FALSE]
  as.integer(apply(x, 1, function(r) sum(!is.na(r))))
}

row_any_true <- function(df, cols) {
  if (length(cols) == 0) return(rep(NA_integer_, nrow(df)))
  x <- df[, cols, drop = FALSE]
  out <- apply(x, 1, function(r) {
    if (all(is.na(r))) return(NA_integer_)
    any(r == 1, na.rm = TRUE)
  })
  as.integer(out)
}

row_count_true <- function(df, cols) {
  if (length(cols) == 0) return(rep(NA_integer_, nrow(df)))
  x <- df[, cols, drop = FALSE]
  out <- apply(x, 1, function(r) {
    if (all(is.na(r))) return(NA_integer_)
    sum(r == 1, na.rm = TRUE)
  })
  as.integer(out)
}

cols_by_pattern <- function(df, pattern) {
  grep(pattern, names(df), value = TRUE)
}

add_feature_sum <- function(df, new_col, pattern, na_as_zero = FALSE) {
  cols <- cols_by_pattern(df, pattern)
  df[[new_col]] <- row_sum_safe(df, cols, na_as_zero = na_as_zero)
  df
}

add_feature_mean <- function(df, new_col, pattern, min_non_na = 1) {
  cols <- cols_by_pattern(df, pattern)
  df[[new_col]] <- row_mean_safe(df, cols, min_non_na = min_non_na)
  df
}

add_feature_any <- function(df, new_col, pattern) {
  cols <- cols_by_pattern(df, pattern)
  df[[new_col]] <- row_any_true(df, cols)
  df
}

add_feature_count_true <- function(df, new_col, pattern) {
  cols <- cols_by_pattern(df, pattern)
  df[[new_col]] <- row_count_true(df, cols)
  df
}

add_feature_completeness <- function(df, new_col, pattern) {
  cols <- cols_by_pattern(df, pattern)
  df[[new_col]] <- row_count_non_na(df, cols)
  df
}

build_features <- function(df, spec) {
  stopifnot(is.list(spec))
  for (s in spec) {
    fn      <- s$fn
    name    <- s$name
    pattern <- s$pattern
    if (is.null(fn) || is.null(name) || is.null(pattern)) {
      stop("Each feature spec must include fn, name, pattern")
    }
    
    if (fn == "sum") {
      df <- add_feature_sum(df, name, pattern, na_as_zero = s$na_as_zero %||% FALSE)
    } else if (fn == "mean") {
      df <- add_feature_mean(df, name, pattern, min_non_na = s$min_non_na %||% 1)
    } else if (fn == "any") {
      df <- add_feature_any(df, name, pattern)
    } else if (fn == "count_true") {
      df <- add_feature_count_true(df, name, pattern)
    } else if (fn == "completeness") {
      df <- add_feature_completeness(df, name, pattern)
    } else {
      stop("Unknown feature fn: ", fn)
    }
  }
  df
}

# ==========================================
# (B) Diagnosis text extraction + coding :)
# ==========================================

make_diagnosis_text <- function(df, diag_prefix = "^Diagnosis_") {
  # 1) Find all Diagnosis columns (THIS is where grep belongs) :)
  diag_cols <- grep(diag_prefix, names(df), value = TRUE)
  if (length(diag_cols) == 0) {
    df$Diagnosis_text <- NA_character_
    return(df)
  }
  
  # 2) Ensure character
  df <- df %>% mutate(across(all_of(diag_cols), as.character))
  
  # 3) Coalesce into one field (first non-NA)
  df <- df %>%
    mutate(
      Diagnosis_text = if (length(diag_cols) == 1) {
        .data[[diag_cols]]
      } else {
        dplyr::coalesce(!!!rlang::syms(diag_cols))
      }
    )
  
  df
}

diagnosis_patterns <- function() {
  list(
    brain = regex(
      paste(
        "brain", "intracranial",
        "glioma", "glioblastoma", "astrocytoma", "oligodendroglioma",
        "ependymoma", "ependimoma",
        "pcnsl", "primary cns lymphoma", "cns lymphoma",
        "schwan",
        "adenoma hipofis", "pituitary adenoma", "hipofisis",
        "craniopharyngioma", "cranipharyngioma",
        "cavernoma",
        "hemangioblastoma",
        "medulloblastoma",
        "meningioma",
        "brain metast",
        sep = "|"
      ),
      ignore_case = TRUE
    ),
    spine = regex(
      paste(
        "spine", "spinal", "vertebra", "vertebral",
        "cauda equina",
        "intramedullary", "extramedullary",
        "mescc", "escc",
        sep = "|"
      ),
      ignore_case = TRUE
    )
  )
}

filter_brain_spine <- function(df, diagnosis_col = "Diagnosis_text") {
  pat <- diagnosis_patterns()
  
  df_brain <- df %>%
    filter(!is.na(.data[[diagnosis_col]]) & str_detect(.data[[diagnosis_col]], pat$brain))
  
  df_spine <- df %>%
    filter(!is.na(.data[[diagnosis_col]]) & str_detect(.data[[diagnosis_col]], pat$spine))
  
  list(brain = df_brain, spine = df_spine)
}

add_brain_code <- function(df, diagnosis_col = "Diagnosis_text") {
  df %>%
    mutate(
      Brain_code = case_when(
        str_detect(.data[[diagnosis_col]],
                   regex("glioma|glioblastoma|astrocytoma|oligodendroglioma|hgg|lgg",
                         ignore_case = TRUE)) ~ "Glioma",
        
        str_detect(.data[[diagnosis_col]],
                   regex("ependymoma|ependimoma", ignore_case = TRUE)) ~ "Ependymoma",
        
        str_detect(.data[[diagnosis_col]],
                   regex("pcnsl|primary cns lymphoma|cns lymphoma",
                         ignore_case = TRUE)) ~ "PCNSL",
        
        str_detect(.data[[diagnosis_col]],
                   regex("schwan", ignore_case = TRUE)) ~ "Schwannoma",
        
        str_detect(.data[[diagnosis_col]],
                   regex("adenoma hipofis|pituitary adenoma|hipofisis",
                         ignore_case = TRUE)) ~ "Adenoma Hypophysis",
        
        str_detect(.data[[diagnosis_col]],
                   regex("craniopharyngioma|cranipharyngioma", ignore_case = TRUE)) ~ "Craniopharyngioma",
        
        str_detect(.data[[diagnosis_col]],
                   regex("cavernoma", ignore_case = TRUE)) ~ "Cavernoma",
        
        str_detect(.data[[diagnosis_col]],
                   regex("hemangioblastoma", ignore_case = TRUE)) ~ "Hemangioblastoma",
        
        str_detect(.data[[diagnosis_col]],
                   regex("medulloblastoma", ignore_case = TRUE)) ~ "Medulloblastoma",
        
        str_detect(.data[[diagnosis_col]],
                   regex("meningioma", ignore_case = TRUE)) ~ "Meningioma",
        
        str_detect(.data[[diagnosis_col]],
                   regex("brain metast", ignore_case = TRUE)) ~ "Brain Metastases",
        
        TRUE ~ "Unknown"
      )
    )
}
