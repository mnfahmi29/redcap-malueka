############################################################
# run.R
# ----------------------------------------------------------
# 🚀 End-to-end REDCap → dashboard → QC pipeline
#
# What this does (high level):
# 1) Read messy REDCap export
# 2) Flatten repeat instances into a wide table
# 3) Calibrate clinical chaos into analysis-ready data
# 4) Extract meaningful features (yes, including glioma 👀)
# 5) Judge data quality like a strict but fair supervisor
#
# Philosophy:
# - Heavy logic lives in src/
# - This file tells the *story* of the pipeline
############################################################


# -----------------------------
# 0. Load all modules 🧩
# -----------------------------
# Each src file has ONE responsibility.
# If something breaks, you know where to look ._.

source("src/01_import_merge.R")        # REDCap → wide table
source("src/02_structure_columns.R")   # relocate_after_pattern()
source("src/03_calibration_rules.R")   # Yes/No, numeric, checkbox sanity
source("src/04_feature_extraction.R")  # Diagnosis text & brain coding
source("src/05_qc_data_quality.R")     # Data-entry reality check

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(writexl)
})


# -----------------------------
# 1. Import + reshape REDCap 📥
# -----------------------------
# One row per Record ID.
# Repeat instances become *_1, *_2, *_3, ...

df <- import_and_merge_instances(
  input_path     = "input/Book191125.xlsx",
  write_xlsx_out = TRUE,
  output_path    = "output/01_wide_instances.xlsx"
)

# At this point:
# ✔ one row per patient
# ✔ instance-specific columns
# ✔ still messy, but structurally sane


# -----------------------------
# 3. Calibration 🧼
# -----------------------------
# Turn REDCap honesty into analysis-ready truth.

# === 3A. Generic rules ===
# These replace many repetitive mutate(across(...)) blocks.

rules <- list(
  list(type = "yn",      pattern = "^Smoking currently_\\d+$"),
  list(type = "yn",      pattern = "^Occupational Exposure \\(choice=.*_\\d+$"),
  list(type = "numeric", pattern = "^Body Temperature_\\d+$"),
  list(type = "numeric", pattern = "^Body Mass Index \\(BMI\\)_\\d+$"),
  list(type = "numeric", pattern = "^Body Surface Area \\(BSA\\)_\\d+$")
)

df <- apply_calibration_rules(df, rules)

# 🧠 Mental model:
# - patterns define WHAT to touch
# - rules define HOW to touch it
# - instances handled automatically


# -----------------------------
# 3B. Custom calibration blocks 🧪
# -----------------------------
# These are the “clinical logic” parts.
# We keep them explicit so future reviewers can audit them.

checkbox_to01 <- function(x)
  dplyr::case_when(
    x == "Checked"   ~ 1L,
    x == "Unchecked" ~ 0L,
    TRUE             ~ 0L
  )

# ---- Paresis & Seizure example ----
paresis_cols <- grep("^Paresis / paralysis", names(df), value = TRUE)
seizure_cols <- grep("^Seizure \\(choice=", names(df), value = TRUE)

df <- df %>%
  mutate(across(all_of(c(paresis_cols, seizure_cols)), checkbox_to01))

# Detect how many repeat instances we actually have
inst_suffixes <- sort(unique(str_extract(names(df), "_\\d+$")))
inst_suffixes <- inst_suffixes[!is.na(inst_suffixes)]

# ⚠️ Your original loops (paresis summary, seizure awareness,
# CN deficits, labs, MRI, spine logic, etc.)
# live HERE unchanged.
#
# We are NOT rewriting medical logic — just relocating it.


# -----------------------------
# ⭐ Column relocation magic ✨
# -----------------------------
# This is the missing piece you asked about.
# Derived summaries should sit *right after* their checkbox blocks.
# Humans read tables left → right. Respect that.

for (inst in 1:4) {
  
  # Paresis summary right after its checkbox block
  df <- df %>%
    relocate_after_pattern(
      new_cols = paste0("Paresis_paralysis_", inst),
      pattern  = paste0("^Paresis / paralysis \\(choice=.*_", inst, "$")
    )
  
  # Seizure summary right after seizure checkboxes
  df <- df %>%
    relocate_after_pattern(
      new_cols = paste0("Seizure_Awareness_", inst),
      pattern  = paste0("^Seizure \\(choice=.*_", inst, "$")
    )
}

# 🎯 Result:
# - Same values as original
# - Much more readable Excel/dashboard
# - Zero guessing where summaries belong


# -----------------------------
# 4. Feature extraction 🧬
# -----------------------------
# This is where grep(glioma) finally lives peacefully.

# Merge all Diagnosis_* into one coherent text field
df <- make_diagnosis_text(df)

# Brain vs spine split (keyword-based, explicit rules)
brain_spine <- filter_brain_spine(df)

write_xlsx(brain_spine$brain, "output/final_brain_only.xlsx")
write_xlsx(brain_spine$spine, "output/final_spine_only.xlsx")

# High-level brain tumor coding
df <- add_brain_code(df)


# -----------------------------
# 5. QC / Data quality 🧾
# -----------------------------
# Time to judge the data capturer (kindly, but firmly).

qc <- run_qc_data_quality(
  df,
  id_col = "Record ID",
  range_rules = list(
    list(var = "Body Mass Index (BMI)_1_corr", min = 10, max = 70),
    list(var = "Body Temperature_1_num",      min = 34, max = 42)
  )
)

saveRDS(qc, "results/qc/qc_report.rds")
write.csv(
  qc$missing$by_col,
  "results/qc/missing_by_column.csv",
  row.names = FALSE
)

############################################################
# 🎉 End of pipeline
#
# If something looks wrong:
# - Structure issue? → src/01
# - Column layout?  → src/02
# - Clinical logic? → src/03
# - Features?       → src/04
# - Data quality?   → src/05
#
# No more 3,000-line scripts. Future-you says thanks :))
############################################################
