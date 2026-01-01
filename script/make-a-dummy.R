# script/make-a-dummy.R
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
#     * capturer ID (Resident's initial) for Phase 3 scoring
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
max_inst   <- 4     # REDCap repeat instances (admission + follow-ups)

# -----------------------------
# 1) Static patient table
# -----------------------------
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
inst_tbl <- tibble(
  `Record ID` = rep(
    patients$`Record ID`,
    times = sample(1:max_inst, n_patients, TRUE)
  )
) %>%
  group_by(`Record ID`) %>%
  mutate(`Repeat Instance` = row_number()) %>%
  ungroup()

dummy <- inst_tbl %>%
  left_join(patients, by = "Record ID") %>%
  mutate(
    `Repeat Instrument` = if_else(
      `Repeat Instance` == 1,
      "admission",
      "followup"
    )
  )

# ------------------------------------------------------------
# Resident / capturer ID (per instance) 👩‍⚕️👨‍⚕️
#
# This is the KEY for Phase 3:
# "Who entered this instance?"
#
# After wide pivot, this becomes:
#   Resident's initial_1, _2, _3, _4
# ------------------------------------------------------------
resident_pool <- c("AA","BB","CC","DD","EE","FF","GG","HH")

dummy <- dummy %>%
  mutate(
    `Resident's initial` = sample(
      c(resident_pool, NA_character_),
      size = n(),
      replace = TRUE,
      prob = c(rep(0.11, length(resident_pool)), 0.12)  # some NA too :')
    )
  )

# -----------------------------
# Helper functions (controlled chaos) 🎲
# -----------------------------
chk <- function(n, p_checked = 0.25, p_na = 0.15) {
  sample(
    c("Checked","Unchecked", NA_character_),
    size = n,
    replace = TRUE,
    prob = c(p_checked, 1 - p_checked - p_na, p_na)
  )
}

maybe_na_num <- function(x, p_na = 0.15) {
  x[sample.int(length(x), size = floor(p_na * length(x)))] <- NA
  x
}

gen_temp <- function(n) {
  base <- sample(c(365,372,378,387,401, 36.5, 37.2, 38.0),
                 n, TRUE)
  maybe_na_num(base, p_na = 0.20)
}

gen_bmi <- function(n) {
  base <- sample(c(234,194,180,289,310, 23.4, 19.8, 30.2),
                 n, TRUE)
  maybe_na_num(base, p_na = 0.20)
}

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
    `Diagnosis_1` = pick_dx(n_rows, p_na = 0.20),
    `Diagnosis_2` = pick_dx(n_rows, p_na = 0.55),
    
    `Paresis / paralysis (choice=Paraparesis)` = chk(n_rows, 0.20, 0.25),
    `Paresis / paralysis (choice=Hemiparesis)` = chk(n_rows, 0.15, 0.25),
    `Paresis / paralysis (choice=No paresis)`  = chk(n_rows, 0.40, 0.20),
    
    `Seizure (choice=GTCS)`       = chk(n_rows, 0.10, 0.20),
    `Seizure (choice=No seizure)` = chk(n_rows, 0.55, 0.15),
    `Awareness (choice=Aware)`    = chk(n_rows, 0.45, 0.25),
    
    `Smoking currently` = sample(
      c("Yes","No","Not smoking", NA_character_),
      n_rows, TRUE,
      prob = c(0.20, 0.45, 0.20, 0.15)
    ),
    `Body Temperature` = gen_temp(n_rows),
    `Body Mass Index (BMI)` = gen_bmi(n_rows),
    
    `Haemoglobin (g/dL)` = gen_hb(n_rows),
    `White blood cell (WBC) count (10^3/mcL)` = gen_wbc(n_rows)
  )

# -----------------------------
# IMPORTANT NOTE ⚠️
# -----------------------------
# No _1/_2 suffixes here on purpose.
# src/01_import_merge.R will pivot using Repeat Instance
# -> generating *_1, *_2, *_3 automatically (including Resident's initial_1..4)
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
