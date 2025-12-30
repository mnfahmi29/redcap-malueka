library(dplyr)
library(readxl)
library(tidyr)
library(writexl)
library(stringr)
library(rlang)

# Load the Excel file
data <- read_excel("input/Book191125.xlsx")

# Remove 'Repeat Instrument' column
data <- data %>% select(-`Repeat Instrument`)

# Identify static and instance-specific variables
# Assume 'Name' and 'Sex' are static (do not vary across Instances)
static_vars <- c("Record ID", "Patient initial", "Patient name", "Medical record number", "Sex", "Place of Birth", "Birth date")
instance_vars <- setdiff(names(data), c(static_vars, "Repeat Instance"))

# Merge rows by 'Record ID' and 'Repeat Instance', combining non-NA for instance vars
merged_instance <- data %>%
  group_by(`Record ID`, `Repeat Instance`) %>%
  summarise(across(all_of(instance_vars), ~ {
    non_na_vals <- na.omit(.)
    if(length(non_na_vals) == 0) {
      return(NA)
    } else {
      return(non_na_vals[1])
    }
  }), .groups = "drop") %>%
  rename(Instance = `Repeat Instance`)

# Keep only one row per Record ID
static_data <- data %>%
  select(all_of(static_vars)) %>%
  distinct(`Record ID`, .keep_all = TRUE)

# Pivot wider for instance variables only
wide_instance <- merged_instance %>%
  pivot_wider(
    id_cols = `Record ID`,
    names_from = Instance,
    values_from = all_of(instance_vars),
    names_sep = "_"
  )

# Combine static data with wide instance data
combined_data <- static_data %>%
  left_join(wide_instance, by = "Record ID")

# Manually add Instance columns between variable blocks
instances <- sort(unique(merged_instance$Instance))

# Function to interleave Instance columns with variable blocks
build_final <- function(df, instances, instance_vars) {
  final_list <- vector("list", nrow(df))
  
  for(i in seq_len(nrow(df))) {
    row <- df[i, ]
    combined_row <- list()
    combined_row[["Record ID"]] <- row$`Record ID`
    combined_row[["Patient Initial"]] <- row$`Patient initial`
    combined_row[["Patient Name"]] <- row$`Patient name`
    combined_row[["Medical Record Number"]] <- row$`Medical record number`
    combined_row[["Sex"]] <- row$`Sex`
    combined_row[["Place of Birth"]] <- row$`Place of Birth`
    combined_row[["Birth Date"]] <- row$`Birth date`
    
    for(inst in instances) {
      # Add Instance column
      combined_row[[paste0("Instance_", inst)]] <- inst
      # Add variables for this instance
      for(var in instance_vars) {
        col_name <- paste0(var, "_", inst)
        combined_row[[col_name]] <- row[[col_name]]
      }
    }
    final_list[[i]] <- combined_row
  }
  
  # Convert list to dataframe
  final_df <- bind_rows(final_list)
  return(final_df)
}

final_data <- build_final(combined_data, instances, instance_vars)
df <- final_data

write_xlsx(final_data, "final19nov25.xlsx")

###############################################################

# Variable Relocation

relocate_after_pattern <- function(data, new_cols, pattern) {
  # new_cols: character vector (one or more column names)
  # pattern : regex to find the *source* block to sit after
  block_cols <- grep(pattern, names(data), value = TRUE)
  if (length(block_cols) == 0) return(data)  # nothing matched → do nothing
  
  anchor <- tail(block_cols, 1)  # last column of that block
  relocate(data, all_of(new_cols), .after = all_of(anchor))
}


###############################################################

# Checkbox REDCap: Checked/Unchecked -> 1/0
checkbox_to01 <- function(x) dplyr::case_when(
  x == "Checked"   ~ 1L,
  x == "Unchecked" ~ 0L,
  TRUE             ~ 0L 
)

# Yes/No -> Yes/No (Assume NA  as No)
yn_na_to_no <- function(x) dplyr::case_when(
  x %in% c("Checked", "Yes","Y") ~ "Yes",
  x %in% c("Unchecked", "No","N") ~ "No",
  TRUE ~ as.character(x)
)

# 1. Paresis/Paralysis & Seizure Checkbox Calibration
paresis_cols   <- grep("^Paresis / paralysis", names(df), value = TRUE)
seizure_cols   <- grep("^Seizure \\(choice=", names(df), value = TRUE)
awareness_cols <- grep("^Awareness \\(choice=", names(df), value = TRUE)

df <- df %>%
  mutate(
    across(all_of(c(paresis_cols, seizure_cols, awareness_cols)),
           checkbox_to01)
  )

# Detect all instance suffixes: "_1", "_2", "_3", ...
inst_suffixes <- sort(unique(str_extract(names(df), "_\\d+$")))
inst_suffixes <- inst_suffixes[!is.na(inst_suffixes)]

## ============ 1) PARESIS / PARALYSIS SUMMARY ============

for (suf in inst_suffixes) {
  inst_paresis_cols <- paresis_cols[endsWith(paresis_cols, suf)]
  
  if (length(inst_paresis_cols) > 0) {
    other_col_name <- paste0("other paresis", suf)
    has_other_col  <- other_col_name %in% names(df)
    
    df <- df %>%
      rowwise() %>%
      mutate(
        !!paste0("Paresis_paralysis", suf) := {
          vals    <- c_across(all_of(inst_paresis_cols))
          on_cols <- inst_paresis_cols[which(vals == 1)]
          
          if (length(on_cols) == 0) {
            # No checkbox selected → treat as "no paresis"
            "no"
          } else {
            base_labels <- on_cols %>%
              str_remove(suf) %>%
              str_remove("^Paresis / paralysis \\(choice=") %>%
              str_remove("\\)$")
            
            # Normalize labels a bit
            base_labels <- case_when(
              base_labels %in% c("paraparesis superior", "paraparesis inferior") ~
                "paraparesis",
              base_labels == "other paresis {other_paresis}" & has_other_col ~
                paste0("other: ", .data[[other_col_name]]),
              base_labels == "other paresis {other_paresis}" ~
                "other",
              TRUE ~ base_labels
            )
            
            paste(unique(base_labels), collapse = "; ")
          }
        }
      ) %>%
      ungroup()
  }
}

## ============ 2) SEIZURE + AWARENESS SUMMARY ============

for (suf in inst_suffixes) {
  inst_seizure_cols   <- seizure_cols[endsWith(seizure_cols, suf)]
  inst_awareness_cols <- awareness_cols[endsWith(awareness_cols, suf)]
  
  if (length(inst_seizure_cols) > 0) {
    df <- df %>%
      rowwise() %>%
      mutate(
        !!paste0("Seizure_Awareness", suf) := {
          # --- Seizure part ---
          seiz_vals <- c_across(all_of(inst_seizure_cols))
          seiz_on   <- inst_seizure_cols[which(seiz_vals == 1)]
          
          seiz_labels <- if (length(seiz_on) == 0) character(0) else
            seiz_on %>%
            str_remove(suf) %>%
            str_remove("^Seizure \\(choice=") %>%
            str_remove("\\)$")
          
          # --- Awareness part ---
          aw_labels <- character(0)
          if (length(inst_awareness_cols) > 0) {
            aw_vals <- c_across(all_of(inst_awareness_cols))
            aw_on   <- inst_awareness_cols[which(aw_vals == 1)]
            
            aw_labels <- if (length(aw_on) == 0) character(0) else
              aw_on %>%
              str_remove(suf) %>%
              str_remove("^Awareness \\(choice=") %>%
              str_remove("\\)$")
          }
          
          # --- Build final string ---
          # If no seizure or explicitly "No seizure"
          if (length(seiz_labels) == 0 || all(seiz_labels == "No seizure")) {
            "no seizure"
          } else {
            seiz_text <- paste(setdiff(seiz_labels, "No seizure"),
                               collapse = "; ")
            
            aw_text <- if (length(aw_labels) == 0) {
              "awareness NA"
            } else {
              paste(aw_labels, collapse = "; ")
            }
            
            # Final form: "GTCS; Motor seizure (Aware)" etc
            paste0(seiz_text, " (", aw_text, ")")
          }
        }
      ) %>%
      ungroup()
  }
}


for (inst in 1:4) {
  df <- df %>%
    relocate_after_pattern(
      paste0("Paresis_paralysis_", inst),
      pattern = paste0("^Paresis / paralysis \\(choice=.*_", inst, "$")
    ) %>%
    relocate_after_pattern(
      paste0("Seizure_Awareness_", inst),
      pattern = paste0("^Seizure \\(choice=.*_", inst, "$")
    )
}


# 2. Past Medical History Calibration
past_hist_cols <- grep("^(Diabetes mellitus|Renal insufficiency|Hepatic insufficiency|Cardiovascular disease|Sepsis|Trauma|Stroke|HIV / AIDS|Others_|History of cancer)_",
                       names(df), value = TRUE)

df <- df %>%
  mutate(
    across(all_of(past_hist_cols), yn_na_to_no)
  )


# 3. Smoking Status Calibration
smoking_cols <- grep("^Smoking currently_", names(df), value = TRUE)

df <- df %>%
  mutate(
    across(all_of(smoking_cols),
           ~ case_when(
             .x %in% c("Not smoking","No","None") ~ "No",
             .x %in% c("Yes","Smoking")           ~ "Yes",
             is.na(.x)                            ~ "No",
             TRUE                                 ~ as.character(.x)
           ))
  )

occ_cols <- grep("^Occupational Exposure \\(choice=", names(df), value = TRUE)

df <- df %>%
  mutate(
    across(all_of(occ_cols), yn_na_to_no)
  )



# 4. Alcohol Consumption Calibration

alcohol_type_cols <- grep("^Alcohol Type \\((Traditional|Western)", 
                          names(df), value = TRUE)

df <- df %>% 
  mutate(
    across(all_of(alcohol_type_cols),
           yn_na_to_no)
  )

alcohol_total_cc_cols  <- grep("^Total Alcohol Consumption \\(cc/day\\)_", names(df), value = TRUE)
alcohol_total_gr_cols  <- grep("^Total Alcohol Consumption \\(gr/day\\)_", names(df), value = TRUE)
alcohol_years_cols     <- grep("^Number of years drinking alcohol_", names(df), value = TRUE)

df <- df %>%
  mutate(
    Alcohol_yes = if_else(
      rowSums(
        cbind(
          as.matrix(dplyr::select(., all_of(alcohol_total_cc_cols))),
          as.matrix(dplyr::select(., all_of(alcohol_total_gr_cols)))
        ),
        na.rm = TRUE
      ) > 0,
      "Yes", "No"
    ),
    Alcohol_years = dplyr::coalesce(!!!rlang::syms(alcohol_years_cols))
  )

df <- df %>%
  relocate_after_pattern(
    c("Alcohol_yes", "Alcohol_years"),
    pattern = "^(Total Alcohol Consumption \\(cc/day\\)_|Total Alcohol Consumption \\(gr/day\\)_|Number of years drinking alcohol_)"
  )


# 5. Contraceptive Use Calibration
# 5a. Identify all contraceptive checkbox columns (all instances)
contraceptive_type_cols <- grep(
  "^Type\\(s\\) of contraceptive\\(s\\) \\(choice=",
  names(df),
  value = TRUE
)

# (Optional but recommended) convert Checked/Unchecked/NA -> 1/0
df <- df %>%
  mutate(
    across(all_of(contraceptive_type_cols), checkbox_to01)
  )

# 5b. Find which instance suffixes exist: _1, _2, _3, ...
instances <- sort(unique(stringr::str_extract(contraceptive_type_cols, "(?<=_)\\d+$")))

# 5c. For each instance, create a single combined text column
for (inst in instances) {
  # all checkbox columns for this instance
  inst_cols <- grep(
    paste0("^Type\\(s\\) of contraceptive\\(s\\) \\(choice=.*_", inst, "$"),
    names(df),
    value = TRUE
  )
  
  # name of the new combined column
  new_col <- paste0("Type(s) of contraceptive(s)_", inst)
  
  # optional: corresponding "Other Contraceptive_inst" free text
  other_col <- paste0("Other Contraceptive_", inst)
  has_other <- other_col %in% names(df)
  
  # build combined text per row
  df[[new_col]] <- apply(df[inst_cols], 1, function(row_vals) {
    # row_vals are 0/1 or NA now
    chosen_idx <- which(row_vals == 1)
    if (length(chosen_idx) == 0) {
      # maybe only 'Other' text is filled
      if (has_other) {
        other_txt <- df[[other_col]][rownames(df)[1] %in% rownames(df)] # safe default
      }
      return(NA_character_)
    }
    
    chosen_names <- inst_cols[chosen_idx]
    
    # strip the common prefix & instance suffix → get just the label
    # e.g. "Type(s) of contraceptive(s) (choice=Impant)_1" -> "Impant"
    labels <- sub("^Type\\(s\\) of contraceptive\\(s\\) \\(choice=", "", chosen_names)
    labels <- sub("\\)_\\d+$", "", labels)
    
    # join with comma
    paste(labels, collapse = ", ")
  })
}

# 5d. (Optional) Overall Yes/No flag per instance
for (inst in instances) {
  combined_col <- paste0("Type(s) of contraceptive(s)_", inst)
  yes_col      <- paste0("Contraceptive_yes_", inst)
  
  df[[yes_col]] <- ifelse(
    is.na(df[[combined_col]]) | df[[combined_col]] == "",
    "No", "Yes"
  )
}

for (inst in 1:4) {
  df <- df %>%
    relocate_after_pattern(
      c(paste0("Type(s) of contraceptive(s)_", inst),
        paste0("Contraceptive_yes_", inst)),
      pattern = paste0("^Type\\(s\\) of contraceptive\\(s\\) \\(choice=.*_", inst, "$")
    )
}


# 6. Body Temperature Correction
temp_cols <- grep("^Body Temperature_", names(df), value = TRUE)

df <- df %>%
  mutate(
    across(all_of(temp_cols), ~ suppressWarnings(as.numeric(.x)))
  ) %>%
  mutate(
    across(
      all_of(temp_cols),
      ~ dplyr::case_when(
        !is.na(.x) & .x >= 350 & .x <= 450 ~ .x / 10,
        TRUE ~ .x
      )
    )
  )

# 7. BMI and BSA Correction
bmi_cols <- grep("^Body Mass Index \\(BMI\\)_", names(df), value = TRUE)
bsa_cols <- grep("^Body Surface Area \\(BSA\\)_", names(df), value = TRUE)

df <- df %>%
  # make sure numeric first
  mutate(
    across(all_of(c(bmi_cols, bsa_cols)), ~ suppressWarnings(as.numeric(.x)))
  ) %>%
  # create corrected columns
  mutate(
    across(
      all_of(bmi_cols),
      ~ case_when(
        !is.na(.x) & .x >= 1000 ~ .x / 100,  # 2344 -> 23.44
        !is.na(.x) & .x >= 100  ~ .x / 10,   # 194  -> 19.4
        TRUE                    ~ .x
      ),
      .names = "{.col}_corr"
    ),
    across(
      all_of(bsa_cols),
      ~ case_when(
        !is.na(.x) & .x >= 100 ~ .x / 100,   # 163 -> 1.63
        !is.na(.x) & .x >= 10  ~ .x / 10,    # 14  -> 1.4, 15 -> 1.5
        TRUE                   ~ .x
      ),
      .names = "{.col}_corr"
    )
  )

for (inst in 1:4) {
  df <- df %>%
    relocate_after_pattern(
      paste0("Body Mass Index (BMI)_", inst, "_corr"),
      pattern = paste0("^Body Mass Index \\(BMI\\)_", inst, "$")
    ) %>%
    relocate_after_pattern(
      paste0("Body Surface Area (BSA)_", inst, "_corr"),
      pattern = paste0("^Body Surface Area \\(BSA\\)_", inst, "$")
    )
}


# 8. Glasgow Coma Scale (GCS) Integration
# 9. Cranial Nerves Assessment Calibration & 10. Papilledema Calibration
# --- identify columns ---
cn_main_cols    <- grep("^CN [IVX]+_[0-9]+$", names(df), value = TRUE)
cn_right_cols   <- grep("^CN [IVX]+ Right/Left \\(choice=Right\\)_[0-9]+$",   names(df), value = TRUE)
cn_left_cols    <- grep("^CN [IVX]+ Right/Left \\(choice=Left\\)_[0-9]+$",    names(df), value = TRUE)
cn_unknown_cols <- grep("^CN [IVX]+ Right/Left \\(choice=Unknown\\)_[0-9]+$", names(df), value = TRUE)
pap_cols        <- grep("^Papilledema_", names(df), value = TRUE)

# make sure CN side checkboxes are 0/1
df <- df %>%
  mutate(
    across(all_of(cn_right_cols),   checkbox_to01),
    across(all_of(cn_left_cols),    checkbox_to01),
    across(all_of(cn_unknown_cols), checkbox_to01)
  )

# helper to build deficits + main CN for ONE nerve (CN I, CN II, ...)
make_cn_deficit <- function(df, nerve_label) {
  # e.g. nerve_label = "CN I"
  main_cols <- grep(paste0("^", nerve_label, "_[0-9]+$"), names(df), value = TRUE)
  r_cols    <- grep(paste0("^", nerve_label, " Right/Left \\(choice=Right\\)_[0-9]+$"),
                    names(df), value = TRUE)
  l_cols    <- grep(paste0("^", nerve_label, " Right/Left \\(choice=Left\\)_[0-9]+$"),
                    names(df), value = TRUE)
  u_cols    <- grep(paste0("^", nerve_label, " Right/Left \\(choice=Unknown\\)_[0-9]+$"),
                    names(df), value = TRUE)
  
  # loop over instances (_1, _2, _3, ...)
  for (i in seq_along(main_cols)) {
    m_col <- main_cols[i]
    r_col <- r_cols[i]
    l_col <- l_cols[i]
    u_col <- u_cols[i]
    
    # instance suffix: "_1", "_2", ...
    inst_suffix <- stringr::str_extract(m_col, "_[0-9]+$")
    def_col <- paste0(nerve_label, " Deficits", inst_suffix)  # e.g. "CN I Deficits_1"
    
    df <- df %>%
      mutate(
        # summarize side deficits
        !!def_col := dplyr::case_when(
          .data[[r_col]] == 1 & .data[[l_col]] == 1 ~ "Both",
          .data[[r_col]] == 1 & .data[[l_col]] == 0 ~ "Right",
          .data[[r_col]] == 0 & .data[[l_col]] == 1 ~ "Left",
          .data[[u_col]] == 1                      ~ "Unknown",   # 0,0,1 → Unknown
          TRUE                                     ~ "No"         # 0,0,0 → No
        ),
        # main CN status from side info (ignore old NA / messy values)
        !!m_col := dplyr::case_when(
          .data[[r_col]] == 1 | .data[[l_col]] == 1 ~ "Yes",      # any real deficit
          .data[[u_col]] == 1                      ~ "Unknown",   # only unknown ticked
          TRUE                                     ~ "No"         # otherwise no deficit
        )
      )
  }
  df
}

# apply to all cranial nerves I–XII
nerves <- paste("CN", c("I","II","III","IV","V","VI","VII","VIII","IX","X","XI","XII"))
for (nerve in nerves) {
  df <- make_cn_deficit(df, nerve)
}

nerves <- c("I","II","III","IV","V","VI","VII","VIII","IX","X","XI","XII")

for (inst in 1:4) {
  for (n in nerves) {
    new_col <- paste0("CN ", n, " Deficits_", inst)
    df <- df %>%
      relocate_after_pattern(
        new_col,
        pattern = paste0("^CN ", n, ".*_", inst, "$")  # CN n & its side cols
      )
  }
}


# 10. Papilledema: Unknown/NA -> "No"
df <- df %>%
  mutate(
    across(
      all_of(pap_cols),
      ~ dplyr::case_when(
        is.na(.x)            ~ "No",
        .x == "Unknown"      ~ "No",
        TRUE                 ~ as.character(.x)
      )
    )
  )


# 11. Vision Loss Calibration
df <- df %>%
  mutate(
    Loss_vision_any_1 = case_when(
      `Loss of Vision: Complete_1` == "Yes" |
        `Loss of vision: Partial_1`  == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),
    Loss_vision_type_1 = case_when(
      `Loss of Vision: Complete_1` == "Yes" ~ "Complete",
      `Loss of vision: Partial_1`  == "Yes" ~ "Partial",
      TRUE ~ "None"
    )
  )

df <- df %>%
  relocate_after_pattern(
    c("Loss_vision_any_1", "Loss_vision_type_1"),
    pattern = "^Loss of [Vv]ision"   # Loss of vision / Loss of Vision: Partial/Complete
  )


# 12. Paresis Summary
make_limb_paresis <- function(df, body_label_id, body_label_en) {
  # e.g. body_label_id = "Tubuh Kanan Atas"
  #      body_label_en = "Upper Right"
  
  # main text columns: Tubuh Kanan Atas_1, Tubuh Kanan Atas_2, ...
  main_cols <- grep(paste0("^", body_label_id, "_[0-9]+$"), names(df), value = TRUE)
  
  for (m_col in main_cols) {
    # instance suffix: "_1", "_2", ...
    inst_suffix <- stringr::str_extract(m_col, "_[0-9]+$")
    
    # all numeric scale columns for this limb & this instance
    # e.g. "Paresis Tubuh Kanan Atas_1_1", "Paresis Tubuh Kanan Atas_9_1", ...
    scale_cols <- grep(
      paste0("^Paresis ", body_label_id, ".*", inst_suffix, "$"),
      names(df),
      value = TRUE
    )
    
    # sort by the internal index (the number before last "_")
    if (length(scale_cols) > 1) {
      idx <- as.integer(stringr::str_match(scale_cols, paste0(body_label_id, "_([0-9]+", inst_suffix, ")"))[,2])
      scale_cols <- scale_cols[order(idx)]
    }
    
    # new columns: status + scale
    status_col <- paste0("Pareses (", body_label_en, ")", inst_suffix)
    scale_col  <- paste0("Pareses (", body_label_en, ") Scale", inst_suffix)
    
    df <- df %>%
      rowwise() %>%
      mutate(
        # Status: Normal / Plegia / Paresis
        !!status_col := case_when(
          is.na(.data[[m_col]]) ~ NA_character_,
          str_detect(.data[[m_col]], regex("^Normal",  ignore_case = TRUE)) ~ "Normal",
          str_detect(.data[[m_col]], regex("^Plegia",  ignore_case = TRUE)) ~ "Plegia",
          str_detect(.data[[m_col]], regex("^Paresis", ignore_case = TRUE)) ~ "Paresis",
          TRUE ~ as.character(.data[[m_col]])   # just in case there are other labels
        ),
        # Scale: join non-NA scores with "-" (e.g. "4-4-4"), or NA if all empty
        !!scale_col := {
          if (length(scale_cols) == 0) {
            NA_character_
          } else {
            vals <- c_across(all_of(scale_cols))
            if (all(is.na(vals))) {
              NA_character_
            } else {
              paste(vals, collapse = "-")
            }
          }
        }
      ) %>%
      ungroup()
  }
  
  df
}

# apply to all limbs & all instances
limbs <- list(
  "Tubuh Kanan Atas" = "Upper Right",
  "Tubuh Kiri Atas"  = "Upper Left",
  "Tubuh Kanan Bawah"= "Lower Right",
  "Tubuh Kiri Bawah" = "Lower Left"
)

for (id_label in names(limbs)) {
  en_label <- limbs[[id_label]]
  df <- make_limb_paresis(df, id_label, en_label)
}

# mapping body region → its original “Tubuh …” block
regions <- list(
  "Upper Right" = "Tubuh Kanan Atas",
  "Upper Left"  = "Tubuh Kiri Atas",
  "Lower Right" = "Tubuh Kanan Bawah",
  "Lower Left"  = "Tubuh Kiri Bawah"
)

for (inst in 1:4) {
  for (reg in names(regions)) {
    new_cols <- c(
      paste0("Pareses (", reg, ")_", inst),
      paste0("Pareses (", reg, ") Scale_", inst)
    )
    pattern <- paste0("^", regions[[reg]], ".*_", inst, "$")
    df <- df %>%
      relocate_after_pattern(new_cols, pattern = pattern)
  }
}


# 13. Sensory Deficit Calibration
sens_cols <- grep("^Sensory deficits_", names(df), value = TRUE)

df <- df %>%
  mutate(
    across(all_of(sens_cols),
           ~ if_else(is.na(.x), "No", as.character(.x)))
  )

#14. Lab
library(dplyr)

### 11.1 CBC / Differential -----------------------------------------

hb_cols  <- grep("^Haemoglobin \\(g/dL\\)_",                           names(df), value = TRUE)
hct_cols <- grep("^Haematocrit \\(%\\)_",                              names(df), value = TRUE)
plt_cols <- grep("^Platelets \\(10\\^3/mcL\\)_",                       names(df), value = TRUE)
rbc_cols <- grep("^Red blood cell \\(RBC\\) count \\(10\\^6/mcL\\)_",  names(df), value = TRUE)
wbc_cols <- grep("^White blood cell \\(WBC\\) count \\(10\\^3/mcL\\)_",names(df), value = TRUE)

neu_cols  <- grep("^Neutrophils \\(%Neu\\)_",   names(df), value = TRUE)
lym_cols  <- grep("^Lymphoblasts \\(%Lim\\)_",  names(df), value = TRUE)
mono_cols <- grep("^Monocytes \\(%Mono\\)_",    names(df), value = TRUE)
eos_cols  <- grep("^Eosinophils \\(%Eos\\)_",   names(df), value = TRUE)
bas_cols  <- grep("^Basophils \\(%Bas\\)_",     names(df), value = TRUE)

df <- df %>%
  mutate(
    # Hb 108 -> 10.8
    across(all_of(hb_cols),
           ~ if_else(!is.na(.x) & .x > 50, .x/10, .x)),
    
    # Hct 386 -> 38.6
    across(all_of(hct_cols),
           ~ if_else(!is.na(.x) & .x > 80, .x/10, .x)),
    
    # RBC 469 -> 4.69
    across(all_of(rbc_cols),
           ~ if_else(!is.na(.x) & .x > 20, .x/100, .x)),
    
    # WBC 127 -> 12.7
    across(all_of(wbc_cols),
           ~ if_else(!is.na(.x) & .x > 50, .x/10, .x)),
    
    # Diff %: 664 -> 66.4 etc.
    across(all_of(c(neu_cols, lym_cols, mono_cols, eos_cols, bas_cols)),
           ~ if_else(!is.na(.x) & .x > 100, .x/10, .x))
  )


### 11.2 Biochemistry / Proteins ------------------------------------

alb_cols  <- grep("^Serum albumin \\(g/dL\\)_",  names(df), value = TRUE)
glob_cols <- grep("^Serum globulin \\(g/dL\\)_", names(df), value = TRUE)

df <- df %>%
  # 1) make sure albumin & globulin are numeric
  mutate(
    across(all_of(c(alb_cols, glob_cols)),
           ~ suppressWarnings(as.numeric(.x)))
  ) %>%
  # 2) do the scaling fix
  mutate(
    # Albumin 383 -> 3.83; 327 -> 3.27
    across(all_of(alb_cols),
           ~ if_else(!is.na(.x) & .x > 50, .x / 100, .x)),
    # Globulin 383 -> 3.83 (if you ever have 3-digit values there)
    across(all_of(glob_cols),
           ~ if_else(!is.na(.x) & .x > 50, .x / 100, .x))
  )
# SGOT/SGPT/BUN are already in plausible ranges, so we leave them as-is.


### 11.3 Coagulation -----------------------------------------------

aptt_cols  <- grep("^APTT \\(seconds\\)_",           names(df), value = TRUE)
apttc_cols <- grep("^APTT control \\(seconds\\)_",   names(df), value = TRUE)
pt_cols    <- grep("^PT \\(seconds\\)_",             names(df), value = TRUE)
ptc_cols   <- grep("^PT control \\(seconds\\)_",     names(df), value = TRUE)
inr_cols   <- grep("^INR \\(seconds\\)_",            names(df), value = TRUE)

df <- df %>%
  mutate(
    # APTT / control 298 -> 29.8; 312 -> 31.2
    across(all_of(c(aptt_cols, apttc_cols)),
           ~ if_else(!is.na(.x) & .x > 100, .x/10, .x)),
    
    # PT / control 104 -> 10.4; 110 -> 11.0
    across(all_of(c(pt_cols, ptc_cols)),
           ~ if_else(!is.na(.x) & .x > 50, .x/10, .x)),
    
    # INR 95 -> 0.95; 106 -> 1.06; 1 stays 1
    across(all_of(inr_cols),
           ~ case_when(
             !is.na(.x) & .x > 10 ~ .x/100,
             !is.na(.x) & .x > 5  ~ .x/10,
             TRUE ~ .x
           ))
  )
# D-dimer and fibrinogen values you showed already look reasonable, so unchanged.


### 11.4 Electrolytes ----------------------------------------------

na_cols <- grep("^Sodium \\(Na\\) \\(mmol/L\\)_",    names(df), value = TRUE)
k_cols  <- grep("^Potassium \\(K\\) \\(mmol/L\\)_",  names(df), value = TRUE)
mg_cols <- grep("^Magnesium \\(Mg\\) \\(mEq/L\\)_",  names(df), value = TRUE)
ca_cols <- grep("^Calcium \\(Ca\\) \\(mmol/L\\)_",   names(df), value = TRUE)

df <- df %>%
  mutate(
    # Na already OK; we leave na_cols alone.
    
    # K 39 -> 3.9; 42 -> 4.2
    across(all_of(k_cols),
           ~ if_else(!is.na(.x) & .x > 15, .x/10, .x)),
    
    # Mg 237 -> 2.37; 17 -> 1.7
    across(all_of(mg_cols),
           ~ if_else(!is.na(.x) & .x > 10, .x/100, .x)),
    
    # Ca 17 -> 1.7; 223 -> 2.23
    across(all_of(ca_cols),
           ~ case_when(
             !is.na(.x) & .x > 100 ~ .x/100,
             !is.na(.x) & .x > 5   ~ .x/10,
             TRUE ~ .x
           ))
  )


# 15. CT Brain Location Extraction

# 1) Grab ALL location checkbox columns for ALL instances
ct_loc_cols_all <- grep(
  paste0(
    "^Cerebrum (Frontal Lobe|Parietal Lobe|Temporal Lobe|Occipital Lobe|Central Lobe / Insula) \\(choice=",
    "|^Brain Stem / Truncus Cerebri \\(choice=",
    "|^Cerebellum \\(choice="
  ),
  names(df),
  value = TRUE
)

# 2) Convert Checked / Unchecked / NA -> 1 / 0
df <- df %>%
  mutate(
    across(all_of(ct_loc_cols_all), checkbox_to01)
  )

# 3) Helper to make pretty labels from column names
#    "Cerebrum Central Lobe / Insula (choice=Right)_1"
# -> "Cerebrum Central Lobe / Insula (Right)"
make_loc_labels <- function(cols) {
  labs <- gsub("_\\d+$", "", cols)                                 # drop _1 / _2
  labs <- sub("^(.+?) \\(choice=", "\\1 (", labs)                  # replace "(choice=" with "("
  labs <- sub("\\)$", ")", labs)                                   # ensure closing )
  labs
}

# 4) Get all instance numbers present in those columns: "1","2","3",...
instances <- sort(unique(sub(".*_(\\d+)$", "\\1", ct_loc_cols_all)))

# 5) Loop per instance, create Location_<instance> summary column
for (inst in instances) {
  
  # columns for that instance only, e.g. *_1
  cols_inst <- grep(paste0("_", inst, "$"), ct_loc_cols_all, value = TRUE)
  if (length(cols_inst) == 0) next
  
  # label map: colname -> pretty label
  labels_inst <- setNames(make_loc_labels(cols_inst), cols_inst)
  
  loc_col_name <- paste0("Location_", inst)   # e.g. "Location_1"
  
  df <- df %>%
    rowwise() %>%
    mutate(
      !!loc_col_name := {
        on <- cols_inst[which(c_across(all_of(cols_inst)) == 1)]
        if (length(on) == 0) NA_character_ else paste(labels_inst[on], collapse = ", ")
      }
    ) %>%
    ungroup()
}

# 16. Brain MRI Location Extraction
# 1) Grab ALL MRI location choice columns for ALL instances
mri_loc_cols_all <- grep(
  "^Location \\(choice=(Intraaxial|Extraaxial|Supratentorial|Infratentorial|Unknown)\\)_",
  names(df),
  value = TRUE
)

# 2) Make them 0/1 using your checkbox_to01 helper
df <- df %>%
  mutate(
    across(all_of(mri_loc_cols_all), checkbox_to01)
  )

# 3) Extract instance numbers (_1, _2, _3, ...)
instances_mri <- sort(unique(str_extract(mri_loc_cols_all, "(?<=_)\\d+$")))

# 4) Helper: turn column name into a clean label, e.g.
# "Location (choice=Intraaxial)_1" -> "Intraaxial"
mri_label <- function(colname) {
  base <- gsub("_\\d+$", "", colname)                # drop _1, _2, ...
  base <- sub("Location \\(choice=", "", base)       # remove prefix
  base <- sub("\\)$", "", base)                      # remove closing ")"
  base
}

# 5) Build MRI_Location_<instance> summary columns
for (inst in instances_mri) {
  # columns for this specific instance
  cols_inst <- grep(paste0("_", inst, "$"), mri_loc_cols_all, value = TRUE)
  if (length(cols_inst) == 0) next
  
  labels_inst <- setNames(mri_label(cols_inst), cols_inst)
  new_col <- paste0("MRI_Location_", inst)
  
  df <- df %>%
    rowwise() %>%
    mutate(
      !!new_col := {
        checked <- cols_inst[which(c_across(all_of(cols_inst)) == 1)]
        if (length(checked) == 0) {
          "Unknown"   # or NA_character_ if you prefer
        } else {
          paste(labels_inst[checked], collapse = ", ")
        }
      }
    ) %>%
    ungroup()
}

for (inst in 1:4) {
  df <- df %>%
    relocate_after_pattern(
      paste0("MRI_Location_", inst),
      pattern = paste0("^Location \\(choice=(Intraaxial|Extraaxial|Supratentorial|Infratentorial|Unknown)\\)_", inst, "$")
    )
}



# 17. Spine MRI Calibration

## 1. Spine Tumor MRI Location
spine_loc_cols_all <- grep(
  "^Location \\(choice=(Extradural|Intradural Extramedullary|Intramedullary|Unknown).*_",
  names(df),
  value = TRUE
)

# make them 0/1
df <- df %>%
  mutate(
    across(all_of(spine_loc_cols_all), checkbox_to01)
  )

# instance suffixes (_1, _2, _3, ...)
instances_spine <- sort(unique(str_extract(spine_loc_cols_all, "(?<=_)\\d+$")))

# helper: turn colname → clean label (Extradural, Intradural Extramedullary, Intramedullary, Unknown)
spine_label <- function(colname) {
  base <- gsub("_\\d+$", "", colname)
  base <- sub("Location \\(choice=", "", base)
  base <- sub("\\).*$", "", base)
  base
}

for (inst in instances_spine) {
  cols_inst <- grep(paste0("_", inst, "$"), spine_loc_cols_all, value = TRUE)
  if (length(cols_inst) == 0) next
  
  labels_inst <- setNames(spine_label(cols_inst), cols_inst)
  new_col <- paste0("Spine_Tumor_Location_", inst)
  
  df <- df %>%
    rowwise() %>%
    mutate(
      !!new_col := {
        checked <- cols_inst[which(c_across(all_of(cols_inst)) == 1)]
        if (length(checked) == 0) {
          NA_character_
        } else {
          paste(labels_inst[checked], collapse = ", ")
        }
      }
    ) %>%
    ungroup()
}

for (inst in 1:4) {
  df <- df %>%
    relocate_after_pattern(
      paste0("Spine_Tumor_Location_", inst),
      pattern = paste0("^Location \\(choice=(Extradural|Intradural Extramedullary|Intramedullary|Unknown)\\)_", inst, "$")
    )
}


## Compression Location
comp_cols_all <- grep("^(C[0-9]+|T[0-9]+|L[0-9]+|S1)_[0-9]+$", names(df), value = TRUE)

instances_comp <- sort(unique(str_extract(comp_cols_all, "(?<=_)\\d+$")))

for (inst in instances_comp) {
  cols_inst <- grep(paste0("_", inst, "$"), comp_cols_all, value = TRUE)
  if (length(cols_inst) == 0) next
  
  labels_inst <- setNames(gsub("_\\d+$", "", cols_inst), cols_inst)  # C7, T11, etc.
  new_col <- paste0("Compression_Location_", inst)
  
  df <- df %>%
    rowwise() %>%
    mutate(
      !!new_col := {
        vals <- c_across(all_of(cols_inst))
        checked <- cols_inst[which(vals %in% c("Yes", "YES", "yes", 1))]
        if (length(checked) == 0) {
          NA_character_  # or "No compression"
        } else {
          paste(labels_inst[checked], collapse = ", ")
        }
      }
    ) %>%
    ungroup()
}

for (inst in 1:4) {
  df <- df %>%
    relocate_after_pattern(
      paste0("Compression_Location_", inst),
      pattern = paste0("^(C[0-9]+|T[0-9]+|L[0-9]+|S1)_", inst, "$")
    )
}


## MESCC Grade Summary
mescc_cols_all <- grep("^(C[0-9]+|T[0-9]+|L[0-9]+|S1) Grade_[0-9]+$", 
                       names(df), value = TRUE)

df <- df %>%
  mutate(
    across(all_of(mescc_cols_all), as.character)
  )

instances_mescc <- sort(unique(stringr::str_extract(mescc_cols_all, "(?<=_)\\d+$")))

for (inst in instances_mescc) {
  cols_inst <- grep(paste0("_", inst, "$"), mescc_cols_all, value = TRUE)
  if (length(cols_inst) == 0) next
  
  # "C7 Grade_1" -> "C7"
  levels_inst <- setNames(sub(" Grade.*$", "", cols_inst), cols_inst)
  new_col <- paste0("MESCC_Grade_", inst)
  
  df <- df %>%
    rowwise() %>%
    mutate(
      !!new_col := {
        vals <- c_across(all_of(cols_inst))   # all character now
        non_empty <- which(!is.na(vals) & vals != "")
        if (length(non_empty) == 0) {
          NA_character_
        } else {
          paste(
            sprintf(
              "%s (Grade %s)",
              levels_inst[cols_inst[non_empty]],
              vals[non_empty]
            ),
            collapse = ", "
          )
        }
      }
    ) %>%
    ungroup()
}

for (inst in 1:4) {
  df <- df %>%
    relocate_after_pattern(
      paste0("MESCC_Grade_", inst),
      pattern = paste0("^(C[0-9]+|T[0-9]+|L[0-9]+|S1) Grade_", inst, "$")
    )
}


## Disc Degeneration DIV Summary
disc_cols_all <- grep("^Disc Degeneration DIV: Yes \\(choice=", names(df), value = TRUE)

# Make sure these are 0/1 first
df <- df %>%
  mutate(
    across(all_of(disc_cols_all), checkbox_to01)
  )

instances_disc <- sort(unique(str_extract(disc_cols_all, "(?<=_)\\d+$")))

# vectorised label helper
disc_label <- function(colnames) {
  base <- gsub("_\\d+$", "", colnames)
  base <- sub("Disc Degeneration DIV: Yes \\(choice=", "", base)
  base <- sub("\\)$", "", base)
  ifelse(base == "Unknown", "DIV Unknown", paste("DIV", base))
}

for (inst in instances_disc) {
  cols_inst <- grep(paste0("_", inst, "$"), disc_cols_all, value = TRUE)
  if (length(cols_inst) == 0) next
  
  labels_inst <- setNames(disc_label(cols_inst), cols_inst)
  new_col <- paste0("Disc_Degeneration_Location_", inst)
  
  df <- df %>%
    rowwise() %>%
    mutate(
      !!new_col := {
        checked <- cols_inst[which(c_across(all_of(cols_inst)) == 1)]
        if (length(checked) == 0) {
          NA_character_
        } else {
          paste(labels_inst[checked], collapse = ", ")
        }
      }
    ) %>%
    ungroup()
}

for (inst in 1:4) {
  df <- df %>%
    relocate_after_pattern(
      paste0("Disc_Degeneration_Location_", inst),
      pattern = paste0("^Disc Degeneration DIV: Yes \\(choice=.*_", inst, "$")
    )
}



# 18. Other Medications Calibration
## 1) Identify all checkbox columns for "Other Medicines Given During Hopitalization"
med_choice_cols_all <- grep(
  "^Other Medicines Given During Hopitalization \\(choice=",
  names(df),
  value = TRUE
)

## 2) Make sure these are coded as 0/1
df <- df %>%
  mutate(
    across(all_of(med_choice_cols_all), checkbox_to01)
  )

## 3) Get all instance suffixes (_1, _2, _3, ...)
instances_med <- sort(unique(str_extract(med_choice_cols_all, "(?<=_)\\d+$")))

## 4) Helper to extract medicine label
med_name_from_choice <- function(colname) {
  base <- gsub("_\\d+$", "", colname)
  base <- sub("Other Medicines Given During Hopitalization \\(choice=", "", base)
  base <- sub("\\)$", "", base)
  base
}

## 5) Loop over instances and build summary column
for (inst in instances_med) {
  
  cols_inst <- grep(paste0("_", inst, "$"), med_choice_cols_all, value = TRUE)
  if (length(cols_inst) == 0) next
  
  med_labels <- med_name_from_choice(cols_inst)
  
  dose_cols <- paste0("Dose ", med_labels, "_", inst)
  dur_cols  <- paste0("Duration ", med_labels, "_", inst)
  
  new_col <- paste0("Other_Meds_Summary_", inst)
  
  df <- df %>%
    rowwise() %>%
    mutate(
      !!new_col := {
        out <- character(0)
        
        for (j in seq_along(cols_inst)) {
          
          val <- .data[[cols_inst[j]]]
          if (!is.na(val) && val == 1) {
            
            med <- med_labels[j]
            if (med %in% c("Other", "Unknown")) next
            
            dose_val <- if (dose_cols[j] %in% names(df)) .data[[dose_cols[j]]] else NA
            dur_val  <- if (dur_cols[j]  %in% names(df)) .data[[dur_cols[j]]]  else NA
            
            label <- med
            extra <- c()
            if (!is.na(dose_val) && dose_val != "") extra <- c(extra, as.character(dose_val))
            if (!is.na(dur_val)  && dur_val  != "") extra <- c(extra, as.character(dur_val))
            if (length(extra) > 0) label <- paste0(label, " (", paste(extra, collapse=", "), ")")
            
            out <- c(out, label)
          }
        }
        
        free_text_col <- paste0("List of other medications_", inst)
        if (free_text_col %in% names(df)) {
          ft <- .data[[free_text_col]]
          if (!is.na(ft) && ft != "") out <- c(out, paste0("Other: ", ft))
        }
        
        if (length(out) == 0) NA_character_ else paste(out, collapse = "; ")
      }
    ) %>% 
    ungroup()
}


# 19. Pathology (PA) Calibration
#pa_checkbox_cols <- c(
#  grep("^Tumour Site \\(please select all that apply", names(df), value = TRUE),
#  grep("^Specimen Handling \\(please select all that apply", names(df), value = TRUE),
#  grep("^Pre-section Treatment \\(please select all that apply", names(df), value = TRUE),
#  grep("Analysis: Testing Method \\(select all that apply", names(df), value = TRUE)
#)

#df <- df %>%
#  mutate(across(all_of(pa_checkbox_cols), checkbox_to01))


###############################################################


# Save 
write_xlsx(df, "final19nov25_calib_new.xlsx")


################################ BRAIN / SPINE

## 1. Cari semua kolom Diagnosis
diag_cols <- grep("^Diagnosis_", names(df), value = TRUE)

## (opsional tapi aman) pastikan jadi character
df <- df %>%
  mutate(
    across(all_of(diag_cols), as.character)
  )

## 2. Gabungkan jadi satu kolom Diagnosis_text (ambil first non-NA)
df <- df %>%
  mutate(
    Diagnosis_text = if (length(diag_cols) == 1) {
      .data[[diag_cols]]
    } else {
      dplyr::coalesce(!!!rlang::syms(diag_cols))
    }
  )

table(is.na(df$Diagnosis_text))
head(df$Diagnosis_text, 20)

## Pattern BRAIN
brain_pattern <- regex(
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
)

## Pattern SPINE
spine_pattern <- regex(
  paste(
    "spine", "spinal", "vertebra", "vertebral",
    "cauda equina",
    "intramedullary", "extramedullary",
    "mescc", "escc",
    sep = "|"
  ),
  ignore_case = TRUE
)

## Brain-only
df_brain <- df %>%
  filter(!is.na(Diagnosis_text) & str_detect(Diagnosis_text, brain_pattern))

## Spine-only
df_spine <- df %>%
  filter(!is.na(Diagnosis_text) & str_detect(Diagnosis_text, spine_pattern))

writexl::write_xlsx(df_brain, "final19nov25_brain_only.xlsx")
writexl::write_xlsx(df_spine, "final19nov25_spine_only.xlsx")



################################ BRAIN CODING


df <- df %>%
  mutate(
    Brain_code = case_when(
      # 1. Glioma (includes GBM, LGG, HGG, astro, oligodendro)
      str_detect(Diagnosis_text,
                 regex("glioma|glioblastoma|astrocytoma|oligodendroglioma|hgg|lgg",
                       ignore_case = TRUE)
      ) ~ "Glioma",
      
      # 2. Ependymoma
      str_detect(Diagnosis_text,
                 regex("ependymoma|ependimoma", ignore_case = TRUE)
      ) ~ "Ependymoma",
      
      # 3. PCNSL / CNS Lymphoma
      str_detect(Diagnosis_text,
                 regex("pcnsl|primary cns lymphoma|cns lymphoma",
                       ignore_case = TRUE)
      ) ~ "PCNSL",
      
      # 4. Schwannoma
      str_detect(Diagnosis_text,
                 regex("schwan", ignore_case = TRUE)
      ) ~ "Schwannoma",
      
      # 5. Adenoma Hypophysis
      str_detect(Diagnosis_text,
                 regex("adenoma hipofis|pituitary adenoma|hipofisis",
                       ignore_case = TRUE)
      ) ~ "Adenoma Hypophysis",
      
      # 6. Craniopharyngioma
      str_detect(Diagnosis_text,
                 regex("craniopharyngioma|cranipharyngioma", ignore_case = TRUE)
      ) ~ "Craniopharyngioma",
      
      # 7. Cavernoma
      str_detect(Diagnosis_text,
                 regex("cavernoma", ignore_case = TRUE)
      ) ~ "Cavernoma",
      
      # 8. Hemangioblastoma
      str_detect(Diagnosis_text,
                 regex("hemangioblastoma", ignore_case = TRUE)
      ) ~ "Hemangioblastoma",
      
      # 9. Medulloblastoma
      str_detect(Diagnosis_text,
                 regex("medulloblastoma", ignore_case = TRUE)
      ) ~ "Medulloblastoma",
      
      # 10. Meningioma
      str_detect(Diagnosis_text,
                 regex("meningioma", ignore_case = TRUE)
      ) ~ "Meningioma",
      
      # 11. Brain Metastases
      str_detect(Diagnosis_text,
                 regex("brain metast", ignore_case = TRUE)
      ) ~ "Brain Metastases",
      
      # 12. Everything else (including non-brain tumors)
      TRUE ~ "Unknown"
    )
  )

save_brain_code <- function(code_label, filename) {
  sub_df <- df %>% filter(Brain_code == code_label)
  writexl::write_xlsx(sub_df, filename)
}

# Examples:
save_brain_code("Glioma", "final19nov25_glioma_only.xlsx")
save_brain_code("Meningioma", "final19nov25_meningioma_only.xlsx")
save_brain_code("Unknown", "final19nov25_brain_unknown.xlsx")

