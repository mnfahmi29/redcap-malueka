# script/BookDummy.R
# ------------------------------------------------------------
# Dummy REDCap-like input generator 🧪
#
# Purpose:
# - Create a SMALL but REALISTIC fake REDCap export
# - Enough chaos to stress-test:
#     * repeat instances
#     * NA handling
#     * checkbox calibration
#     * unit correction (BMI, temperature, labs)
#     * diagnosis text parsing (brain vs spine)
#
# Philosophy:
# ❌ Not statistically meaningful
# ✅ Structurally meaningful
#
# If your pipeline survives THIS,
# it will probably survive real hospital data :')
# ------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(writexl)
  library(tibble)
})

set.seed(42)  # reproducibility = kindness to future-you

# -----------------------------
# Config section 🔧
# -----------------------------
n_patients <- 200   # enough to see patterns, not enough to cry
max_inst   <- 4    # REDCap repeat instances (admission + follow-ups)

# -----------------------------
# 1) Static patient table
# -----------------------------
# These variables SHOULD NOT change across instances
# (and if they do in real life… that’s another QC story)
patients <- tibble(
  `Record ID` = sprintf("%03d", 1:n_patients),
  `Patient initial` = str_c(
    sample(LETTERS, n_patients, TRUE),
    sample(LETTERS, n_patients, TRUE)
  ),
  `Patient name` = paste("Patient", 1:n_patients),
  `Medical record number` = sprintf("MR%04d", 1001:(1000 + n_patients)),
  Sex = sample(c("M", "F"), n_patients, TRUE),
  `Place of Birth` = sample(c("CityA","CityB","CityC","CityD"),
                            n_patients, TRUE),
  `Birth date` = as.Date("1975-01-01") + sample(0:20000, n_patients, TRUE)
)

# -----------------------------
# 2) Repeat-instance structure
# -----------------------------
# Each patient can appear 1–4 times,
# mimicking admission + follow-up visits.
inst_tbl <- tibble(
  `Record ID` = rep(
    patients$`Record ID`,
    times = sample(1:max_inst, n_patients, TRUE)
  )
) %>%
  group_by(`Record ID`) %>%
  mutate(`Repeat Instance` = row_number()) %>%
  ungroup()

# Attach static demographics to EVERY row
# (this is how REDCap exports look)
dummy <- inst_tbl %>%
  left_join(patients, by = "Record ID") %>%
  mutate(
    `Repeat Instrument` = if_else(
      `Repeat Instance` == 1,
      "admission",
      "followup"
    )
  )

# -----------------------------
# Helper functions (controlled chaos) 🎲
# -----------------------------

# Checkbox generator:
# produces Checked / Unchecked / NA
chk <- function(n, p_checked = 0.25, p_na = 0.15) {
  sample(
    c("Checked","Unchecked", NA_character_),
    size = n,
    replace = TRUE,
    prob = c(p_checked, 1 - p_checked - p_na, p_na)
  )
}

# Sprinkle NA into numeric vectors
maybe_na_num <- function(x, p_na = 0.15) {
  x[sample.int(length(x), size = floor(p_na * length(x)))] <- NA
  x
}

# Temperature generator
# includes:
# - correct values (36.5)
# - x10 mistakes (365)
gen_temp <- function(n) {
  base <- sample(c(365,372,378,387,401, 36.5, 37.2, 38.0),
                 n, TRUE)
  maybe_na_num(base, p_na = 0.20)
}

# BMI generator
# includes classic disasters like 234 (→ 23.4)
gen_bmi <- function(n) {
  base <- sample(c(234,194,180,289,310, 23.4, 19.8, 30.2),
                 n, TRUE)
  maybe_na_num(base, p_na = 0.20)
}

# Lab generators (for QC & unit correction)
gen_hb <- function(n) {
  base <- sample(c(108,121,132,145, 10.8, 12.5, 13.1),
                 n, TRUE)
  maybe_na_num(base, p_na = 0.25)
}

gen_wbc <- function(n) {
  base <- sample(c(127,85,63,102, 8.5, 12.7, 6.4, 10.2),
                 n, TRUE)
  maybe_na_num(base, p_na = 0.25)
}

# Diagnosis pool
# deliberately mixed:
# - brain
# - spine
# - unknown
dx_pool <- c(
  "Glioblastoma multiforme",
  "Astrocytoma low grade",
  "Oligodendroglioma",
  "Meningioma",
  "PCNSL (primary CNS lymphoma)",
  "Ependymoma",
  "Brain metastasis",
  "Spinal intramedullary tumor",
  "Spinal extradural lesion",
  "Unknown mass"
)

pick_dx <- function(n, p_na = 0.25) {
  sample(
    c(dx_pool, NA_character_),
    n,
    TRUE,
    prob = c(rep((1 - p_na) / length(dx_pool), length(dx_pool)), p_na)
  )
}

# -----------------------------
# 3) Add REDCap-style variables
# -----------------------------
n_rows <- nrow(dummy)

dummy <- dummy %>%
  mutate(
    # Diagnosis columns
    # (multiple columns so coalesce() matters)
    `Diagnosis_1` = pick_dx(n_rows, p_na = 0.20),
    `Diagnosis_2` = pick_dx(n_rows, p_na = 0.55),
    
    # Paresis / seizure checkbox blocks
    `Paresis / paralysis (choice=Paraparesis)` = chk(n_rows, 0.20, 0.25),
    `Paresis / paralysis (choice=Hemiparesis)` = chk(n_rows, 0.15, 0.25),
    `Paresis / paralysis (choice=No paresis)`  = chk(n_rows, 0.40, 0.20),
    
    `Seizure (choice=GTCS)`       = chk(n_rows, 0.10, 0.20),
    `Seizure (choice=No seizure)` = chk(n_rows, 0.55, 0.15),
    `Awareness (choice=Aware)`    = chk(n_rows, 0.45, 0.25),
    
    # Smoking + vitals
    `Smoking currently` = sample(
      c("Yes","No","Not smoking", NA_character_),
      n_rows, TRUE,
      prob = c(0.20, 0.45, 0.20, 0.15)
    ),
    `Body Temperature` = gen_temp(n_rows),
    `Body Mass Index (BMI)` = gen_bmi(n_rows),
    
    # Labs
    `Haemoglobin (g/dL)` = gen_hb(n_rows),
    `White blood cell (WBC) count (10^3/mcL)` = gen_wbc(n_rows)
  )

# -----------------------------
# IMPORTANT NOTE ⚠️
# -----------------------------
# These columns DO NOT have _1, _2 suffixes here.
# That is INTENTIONAL.
#
# src/01_import_merge.R will:
# - use Repeat Instance
# - pivot to wide
# - produce *_1, *_2, *_3 automatically
#
# This mimics REAL REDCap exports. -_- at least, Malueka's REDCap
# -----------------------------

# -----------------------------
# 4) Write output 📦
# -----------------------------
dir.create("input", showWarnings = FALSE, recursive = TRUE)
write_xlsx(dummy, "input/BookDummy.xlsx")

message("✅ Dummy REDCap input written")
message(
  "Rows (long): ", nrow(dummy),
  " | Patients: ", n_patients,
  " | Instances per patient: 1–", max_inst
)

# Now now, let's go to run.R hehehe...
