# src/01_import_merge.R
# ------------------------------------------------------------
# REDCap repeat-instance shaping 
# Goal: transform long-ish export (original Redcap) -> dashboard-friendly wide table
#
# Why this file exists
# - REDCap exports repeat instruments/instances as multiple rows per patient.
# - For Excel dashboarding, in Malueka Group, we often want ONE row per "Record ID", 
#   with instance-specific fields spread horizontally:  var_1, var_2, var_3, ...
# - This script performs that reshape while keeping "static" demographics only once.
#   static demographics like -- Record Id, Name, Birth Place, etc. these varies 
#   for each CRF model
#
# What this script does
# 1) Read input (xlsx or csv) exported from REDCap / data entry sheet
# 2) Drop REDCap technical column: "Repeat Instrument" (if present)
# 3) Separate:
#    - static variables (do NOT vary across Repeat Instance; keep once per Record ID)
#    - instance variables (vary across Repeat Instance; spread across columns)
# 4) Merge duplicate rows within each (Record ID, Repeat Instance):
#    - if a variable has multiple entries, take the first non-NA
# 5) Pivot to wide format:
#    - columns become: <variable>_<instance>
# 6) (Optional but dashboard-nice) Interleave "Instance_<k>" marker columns between blocks
#
# Inputs
# - A file that contains at least:
#   "Record ID", "Repeat Instance"
# - And optionally:
#   "Repeat Instrument" (will be removed)
#
# Outputs
# - Returns a wide dataframe (one row per Record ID)
# - Optionally writes an .xlsx file to output/
#
# Notes / assumptions (based on your current code)
# - "static_vars" are currently hard-coded (you can adjust later).
# - Instance suffixes are treated as the values in "Repeat Instance" (e.g. 1,2,3,...)
# - When duplicates exist inside one instance, we keep the first non-missing value.
# COMMON PITFALLS ==> 
# 1. you should take notes for the naming of varibles, this function
#    only works for SYNERGY (our Malueka's Redcap Format)
# 2. Redcap has two ways for exporting CSV --> using 'Record ID' or 'record_id' ._.
# ------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readxl)
  library(readr)
  library(writexl)
})

# ---- helpers ------------------------------------------------

read_input_table <- function(path) {
  stopifnot(file.exists(path))
  
  ext <- tolower(tools::file_ext(path))
  
  if (ext %in% c("xlsx", "xls")) {
    readxl::read_excel(path)
  } else if (ext %in% c("csv")) {
    readr::read_csv(path, show_col_types = FALSE)
  } else {
    stop("Unsupported input type: ", ext, " (use .xlsx or .csv)")
  }
}

# Within each (Record ID, Repeat Instance), collapse duplicates by taking the first non-NA
first_non_na <- function(x) {
  non_na_vals <- stats::na.omit(x)
  if (length(non_na_vals) == 0) NA else non_na_vals[1]
}

# Interleave: Instance_k column + block of variables for instance k
build_interleaved_instance_blocks <- function(df, instances, instance_vars,
                                              static_mapping = list(
                                                "Record ID"            = "Record ID",
                                                "Patient Initial"      = "Patient initial",
                                                "Patient Name"         = "Patient name",
                                                "Medical Record Number"= "Medical record number",
                                                "Sex"                  = "Sex",
                                                "Place of Birth"       = "Place of Birth",
                                                "Birth Date"           = "Birth date"
                                              )) {
  
  final_list <- vector("list", nrow(df))
  
  for (i in seq_len(nrow(df))) {
    row <- df[i, , drop = FALSE]
    combined_row <- list()
    
    # Static columns in your preferred naming (dashboard-friendly)
    for (new_name in names(static_mapping)) {
      old_name <- static_mapping[[new_name]]
      combined_row[[new_name]] <- if (old_name %in% names(row)) row[[old_name]] else NA
    }
    
    # Instance blocks
    for (inst in instances) {
      combined_row[[paste0("Instance_", inst)]] <- inst
      
      for (var in instance_vars) {
        col_name <- paste0(var, "_", inst)
        combined_row[[col_name]] <- if (col_name %in% names(row)) row[[col_name]] else NA
      }
    }
    
    final_list[[i]] <- combined_row
  }
  
  dplyr::bind_rows(final_list)
}

# ---- main function ------------------------------------------

import_and_merge_instances <- function(input_path,
                                       static_vars = c(
                                         "Record ID",
                                         "Patient initial",
                                         "Patient name",
                                         "Medical record number",
                                         "Sex",
                                         "Place of Birth",
                                         "Birth date"
                                       ),
                                       drop_repeat_instrument = TRUE,
                                       write_xlsx_out = FALSE,
                                       output_path = "output/01_wide_instances.xlsx") {
  
  data <- read_input_table(input_path)
  
  # Optional: drop REDCap technical column
  if (drop_repeat_instrument && "Repeat Instrument" %in% names(data)) {
    data <- data %>% select(-`Repeat Instrument`)
  }
  
  # Basic required columns
  required_cols <- c("Record ID", "Repeat Instance")
  missing_required <- setdiff(required_cols, names(data))
  if (length(missing_required) > 0) {
    stop("Missing required columns: ", paste(missing_required, collapse = ", "))
  }
  
  # Identify instance-varying variables
  instance_vars <- setdiff(names(data), c(static_vars, "Repeat Instance"))
  
  # Merge duplicates within (Record ID, Repeat Instance)
  merged_instance <- data %>%
    group_by(`Record ID`, `Repeat Instance`) %>%
    summarise(
      across(all_of(instance_vars), first_non_na),
      .groups = "drop"
    ) %>%
    rename(Instance = `Repeat Instance`)
  
  # Keep only one row per Record ID for static fields
  static_data <- data %>%
    select(any_of(static_vars)) %>%
    distinct(`Record ID`, .keep_all = TRUE)
  
  # Pivot wide: var_instance
  wide_instance <- merged_instance %>%
    pivot_wider(
      id_cols     = `Record ID`,
      names_from  = Instance,
      values_from = all_of(instance_vars),
      names_sep   = "_"
    )
  
  # Combine static + wide instance
  combined_data <- static_data %>%
    left_join(wide_instance, by = "Record ID")
  
  # Optional: make dashboard layout nicer by inserting Instance_k markers
  # This would help you to give some orientation of which inpatient it is: the first, second, or else
  instances <- sort(unique(merged_instance$Instance))
  final_data <- build_interleaved_instance_blocks(
    df = combined_data,
    instances = instances,
    instance_vars = instance_vars
  )
  
  if (write_xlsx_out) {
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    writexl::write_xlsx(final_data, output_path)
  }
  
  final_data
}

# How to use this?
# 1. Load this to your R script
# 2. Use the function into your data filepath
# 3. Input the output path, then viola, finish
# 
# ==Example: my data is 'Book191125'
# 
# df01 <- import_and_merge_instances(
#   input_path = "input/Book191125.xlsx",
#   write_xlsx_out = TRUE,
#   output_path = "output/01_final19nov25.xlsx"
# )
# View(df01)
