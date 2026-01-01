# src/02_structure_columns.R
# ------------------------------------------------------------
# Column relocation helpers (make the sheet readable) :)
#
# Why this exists:
# After we pivot to wide format, columns can feel like a jungle ._.
# This helper lets us insert "new cols" right after a *block* of columns
# that match a regex pattern — super useful for keeping instance blocks tidy.
#
# Example idea:
# - You create a derived column (say: "score_1")
# - You want it to sit after all columns for instance 1 (or after a specific variable block)
# -> use relocate_after_pattern()
# ------------------------------------------------------------

# R/02_structure_columns.R

suppressPackageStartupMessages({ library(dplyr) })

relocate_after_pattern <- function(data, new_cols, pattern) {
  # 1) if the new cols don't exist yet → skip (prevents your error)
  new_cols <- intersect(new_cols, names(data))
  if (length(new_cols) == 0) return(data)
  
  # 2) find the block anchor
  block_cols <- grep(pattern, names(data), value = TRUE)
  if (length(block_cols) == 0) return(data)
  
  anchor <- tail(block_cols, 1)
  relocate(data, all_of(new_cols), .after = all_of(anchor))
}


# ------------------------------------------------------------
# Usage pattern :
# 
# data --> your data from scr1, or not (depends if you want to use this seperately ._.)
# 
# df <- relocate_after_pattern(
#   data    = df,
#   new_cols = c("some_new_col_1", "some_new_col_2", "etc"),
#   pattern = "_1$"   # e.g., after all instance-1 columns
# )
# ------------------------------------------------------------
