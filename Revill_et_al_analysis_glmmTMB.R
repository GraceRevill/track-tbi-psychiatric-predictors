################################################################
#
# Analysis of TRACK-TBI dataset
#
# Revill et al
#
# This script performs all statistical analyses and saves results to files
# Results are then read by Revill_et_al_visualisation.R
#
# Components:
# - Unadjusted and adjusted models using glmmTMB (converges better)
# - Test for dose response relationships between severity measures
#   and outcomes
# - Comparison of severity measures in terms of predictive value
#
################################################################

# Load libraries
library(dplyr)
library(glmmTMB)
library(broom.mixed)
library(MuMIn)
library(Hmisc)
library(gt)
library(gtsummary)
library(caret)

# Remove everything from memory if needed to start afresh
rm(list = ls())

# Set data and output directories
data_dir <- "S:/Track_TBI/FlatCSV_2023-10-10T03-23-25/"
processed_data_dir <- "S:/Track_TBI/Grace/GraceCode/processed_data/"

setwd(data_dir)

# Set whether to use non-imputed data for analysis
# TRUE = use non-imputed data, FALSE = use imputed data (default)

imputed_analysis <- FALSE

###########################################################
#
# Load imputed data
#
###########################################################

# Load imputed data
imputed_wide_data <- readRDS(paste(processed_data_dir, "imputed_wide_data.rds", sep = ""))

# Load imputed data (TBI and Ortho only)
imputed_wide_data_oi <- readRDS(paste(processed_data_dir, "imputed_wide_data_oi.rds", sep = ""))

# Load non-imputed data for descriptive statistics
nonimputed_wide_data <- readRDS(paste(processed_data_dir, "nonimputed_wide_data.rds", sep = ""))

# Create working datasets based on analysis type
if (imputed_analysis) {
  cat("Using IMPUTED data\n")
  analysis_data <- imputed_wide_data
  analysis_data_oi <- imputed_wide_data_oi
  output_dir <- "S:/Track_TBI/Grace/GraceCode/output_imp/"
} else {
  cat("Using NON-IMPUTED data\n")
  analysis_data <- nonimputed_wide_data
  analysis_data_oi <- subset(nonimputed_wide_data, injury_type %in% c("TBI", "Ortho"))
  output_dir <- "S:/Track_TBI/Grace/GraceCode/output_nonimp/"
}

# Check this is correct
xtabs( ~ injury_type, data = analysis_data)
xtabs( ~ injury_type, data = analysis_data_oi)

###########################################################
#
# Descriptive statistics tables (using non-imputed data)
#
###########################################################

# Add variable labels for table output
label(nonimputed_wide_data$age) <- "Age"
label(nonimputed_wide_data$sex) <- "Sex"
label(nonimputed_wide_data$race) <- "Race and ethnicity"
label(nonimputed_wide_data$injury_type) <- "Injury type"
label(nonimputed_wide_data$family_income) <- "Household Income"
label(nonimputed_wide_data$previous_tbi) <- "Previous TBI"
label(nonimputed_wide_data$anxiety_history) <- "Anxiety diagnosis"
label(nonimputed_wide_data$depression_history) <- "Depression diagnosis"
label(nonimputed_wide_data$bipolar_history) <- "Bipolar diagnosis"
label(nonimputed_wide_data$schizophrenia_history) <- "Schizophrenia diagnosis"
label(nonimputed_wide_data$ptsd_history) <- "PTSD diagnosis"
label(nonimputed_wide_data$adhd_history) <- "ADHD diagnosis"
label(nonimputed_wide_data$any_mh_disorder) <- "Number of previous mental health diagnoses"
label(nonimputed_wide_data$psyc_hospital_history) <- "Psychiatric hospitalisation"
label(nonimputed_wide_data$psyc_med_history) <- "Psychiatric medication"
label(nonimputed_wide_data$loc_duration) <- "LOC duration"
label(nonimputed_wide_data$aoc_duration) <- "AOC duration"
label(nonimputed_wide_data$pta_duration) <- "PTA duration"

# Create mh_disorder variable if not present
if (!"mh_disorder" %in% names(nonimputed_wide_data)) {
  nonimputed_wide_data <- nonimputed_wide_data %>%
    mutate(mh_disorder = case_when(any_mh_disorder >= 1 ~ "One or more",
                                   any_mh_disorder < 1  ~ "None"))
  nonimputed_wide_data$mh_disorder <- as.factor(nonimputed_wide_data$mh_disorder)
}
label(nonimputed_wide_data$mh_disorder) <- "Previous mental health disorders"

# Create demographic and descriptive table for sample
descriptive_table <- nonimputed_wide_data %>%
  select(injury_type, age, sex, race, family_income, mh_disorder, previous_tbi) %>%
  tbl_summary(by = injury_type) %>%
  bold_labels() %>%
  italicize_levels() %>%
  modify_caption("**Table 1. Demographics of sample**")

# Save table
table_1_filename = paste(output_dir, "descriptive_table.html", sep="")
gt::gtsave(as_gt(descriptive_table), file = table_1_filename)

# Create table to see LOC, PTA and AOC descriptive statistics
duration_table <- nonimputed_wide_data %>%
  subset(injury_type == 'TBI') %>%
  select(loc_duration, pta_duration, aoc_duration) %>%
  tbl_summary() %>%
  italicize_levels() %>%
  modify_caption("**Table 2. Injury duration characteristics (TBI only)**")

# Save table
table_2_filename = paste(output_dir, "duration_injury_table.html", sep="")
gt::gtsave(as_gt(duration_table), file = table_2_filename)

# Create table to see percentage of prior mental health diagnoses
mh_history_table <- nonimputed_wide_data %>%
  select(injury_type, anxiety_history, depression_history, bipolar_history,
         schizophrenia_history, ptsd_history, adhd_history, psyc_med_history,
         psyc_hospital_history, any_mh_disorder) %>%
  tbl_summary(by = injury_type) %>%
  bold_labels() %>%
  italicize_levels() %>%
  modify_caption("**Table 3. Pre-injury mental health of sample**")

# Save table
table_3_filename = paste(output_dir, "previous_mh_table.html", sep="")
gt::gtsave(as_gt(mh_history_table), file = table_3_filename)

##############################################################################
#
# Mixed effects models
#
# Injury variables as predictors
# 12 month mental health variables as outcomes
# Study site as a random effect
# Adjusted model covariates are age, sex, ethnciity, SES, previous mental health score,
# previous alcohol and drug score, previous TBI
#
# Using glmmTMB for model fitting
#
#############################################################################

### Check variables stored and ordered correctly
# Set data type and level labels
analysis_data$injury_cause <- factor(analysis_data$injury_cause ,
                                         levels = c("Road traffic accident", "Accidental falls", "Violence/assault", "Other"))

# Set data type and level labels
analysis_data$admission_type[analysis_data$admission_type == "Other, specify"] <- "Other"

analysis_data$admission_type <- factor(analysis_data$admission_type,
                                           levels = c("Admission to hospital - ICU", "Admission to hospital - ward", "Admission to hospital - other (e.g., observation unit)", "Discharge to other hospital", "Discharge to home",
                                                      "Other"))

# Set data type and level labels
analysis_data$pta_duration <- factor(analysis_data$pta_duration,
                                         levels = c("None", "<1 minute", "1-29 minutes", "30-59 minutes", "1-24 hours", "1-7 days",
                                                    ">7 days"))

# Recode PTA duration variable
analysis_data <- analysis_data %>%
  mutate(pta_duration_ordered = case_when(pta_duration == "None" ~  0,
                                          pta_duration == "<1 minute" ~  1,
                                          pta_duration == "1-29 minutes" ~  2,
                                          pta_duration == "30-59 minutes" ~  3,
                                          pta_duration == "1-24 hours" ~  4,
                                          pta_duration == "1-7 days" ~  5,
                                          pta_duration == ">7 days" ~  6))

analysis_data$pta_duration_ordered <- as.numeric(analysis_data$pta_duration_ordered)


# Set data type and level labels
analysis_data$loc_duration <- factor(analysis_data$loc_duration,
                                         levels = c("None", "<1 minute", "1-29 minutes", "30-59 minutes", "1-24 hours", "1-7 days",
                                                    ">7 days"))

# Recode LOC duration variable
analysis_data <- analysis_data %>%
  mutate(loc_duration_ordered = case_when(loc_duration == "None" ~  0,
                                          loc_duration == "<1 minute" ~  1,
                                          loc_duration == "1-29 minutes" ~  2,
                                          loc_duration == "30-59 minutes" ~  3,
                                          loc_duration == "1-24 hours" ~  4,
                                          loc_duration == "1-7 days" ~  5,
                                          loc_duration == ">7 days" ~  6))

analysis_data$loc_duration_ordered <- as.numeric(analysis_data$loc_duration_ordered)

# Set data type and level labels
analysis_data$aoc_duration <- factor(analysis_data$aoc_duration,
                                         levels = c("None", "<1 minute", "1-29 minutes", "30-59 minutes", "1-24 hours", "1-7 days",
                                                    ">7 days"))


# Create new AOC duration variable
analysis_data <- analysis_data %>%
  mutate(aoc_duration_ordered = case_when(aoc_duration == "None" ~  0,
                                          aoc_duration == "<1 minute" ~  1,
                                          aoc_duration == "1-29 minutes" ~  2,
                                          aoc_duration == "30-59 minutes" ~  3,
                                          aoc_duration == "1-24 hours" ~  4,
                                          aoc_duration == "1-7 days" ~  5,
                                          aoc_duration == ">7 days" ~  6))

analysis_data$aoc_duration_ordered <- as.numeric(analysis_data$aoc_duration_ordered)


##############################################################################
#
# Fitting mixed effects models with glmmTMB
#
##############################################################################

##### Injury type (TBI or Ortho injury) as predictor ######

# Ensure variables stored correctly
analysis_data_oi$injury_type <- as.factor(analysis_data_oi$injury_type)

# Change reference categories
analysis_data_oi$injury_type <- relevel(analysis_data_oi$injury_type, ref = "Ortho")

# Run unadjusted model
injury_type_bsi_12_model_unadjusted <- glmmTMB(bsi_global_score_12_months ~ injury_type +
                                              (1|site),
                                            data = analysis_data_oi

)

# Run adjusted model
injury_type_bsi_12_model_adjusted <- glmmTMB(bsi_global_score_12_months ~ injury_type +
                                            age +
                                            sex +
                                            race +
                                            family_income +
                                            any_mh_disorder +
                                            audit_total_score +
                                            previous_tbi +
                                            drug_total_score +
                                            (1|site),
                                          data = analysis_data_oi,
                                          control = glmmTMBControl(optCtrl = list(iter.max = 10000, eval.max = 10000))
)

# Run unadjusted model
injury_type_phq_12_model_unadjusted <- glmmTMB(phq_global_score_12_months ~ injury_type +
                                              (1|site),
                                            data = analysis_data_oi
)

# Run adjusted model
injury_type_phq_12_model_adjusted <- glmmTMB(phq_global_score_12_months ~ injury_type +
                                            age +
                                            sex +
                                            race +
                                            family_income +
                                            any_mh_disorder +
                                            audit_total_score +
                                            previous_tbi +
                                            drug_total_score +
                                            (1|site),
                                          data = analysis_data_oi,
                                          control = glmmTMBControl(optCtrl = list(iter.max = 10000, eval.max = 10000))
)


##### Injury severity (mild, moderate, severe using GCS scoring system) as predictor ######

# Run unadjusted model
gcs_bsi_12_model_unadjusted <- glmmTMB(bsi_global_score_12_months ~ gcs_severity +
                                      (1|site),
                                    data = analysis_data
)



# Run adjusted model
gcs_bsi_12_model_adjusted <- glmmTMB(bsi_global_score_12_months ~ gcs_severity +
                                    age +
                                    sex +
                                    race +
                                    family_income +
                                    any_mh_disorder +
                                    audit_total_score +
                                    previous_tbi +
                                    drug_total_score +
                                    (1|site),
                                  data = analysis_data,
                                  control = glmmTMBControl(optCtrl = list(iter.max = 10000, eval.max = 10000))
)


# Run unadjusted model
gcs_phq_12_model_unadjusted <- glmmTMB(phq_global_score_12_months ~ gcs_severity +
                                      (1|site),
                                    data = analysis_data
)




# Run adjusted model
gcs_phq_12_model_adjusted <- glmmTMB(phq_global_score_12_months ~ gcs_severity +
                                    age +
                                    sex +
                                    race +
                                    family_income +
                                    any_mh_disorder +
                                    audit_total_score +
                                    previous_tbi +
                                    drug_total_score +
                                    (1|site),
                                  data = analysis_data,
                                  control = glmmTMBControl(optCtrl = list(iter.max = 10000, eval.max = 10000))
)


##### GCS score (continuous, reversed) as predictor ######

# Run unadjusted model
gcs_score_bsi_12_model_unadjusted <- glmmTMB(bsi_global_score_12_months ~ gcs_score_reversed +
                                            (1|site),
                                          data = analysis_data
)

# Run adjusted model
gcs_score_bsi_12_model_adjusted <- glmmTMB(bsi_global_score_12_months ~ gcs_score_reversed +
                                          age +
                                          sex +
                                          race +
                                          family_income +
                                          any_mh_disorder +
                                          audit_total_score +
                                          previous_tbi +
                                          drug_total_score +
                                          (1|site),
                                        data = analysis_data,
                                        control = glmmTMBControl(optCtrl = list(iter.max = 10000, eval.max = 10000))
)

# Run unadjusted model
gcs_score_phq_12_model_unadjusted <- glmmTMB(phq_global_score_12_months ~ gcs_score_reversed +
                                            (1|site),
                                          data = analysis_data
)

# Run adjusted model
gcs_score_phq_12_model_adjusted <- glmmTMB(phq_global_score_12_months ~ gcs_score_reversed +
                                          age +
                                          sex +
                                          race +
                                          family_income +
                                          any_mh_disorder +
                                          audit_total_score +
                                          previous_tbi +
                                          drug_total_score +
                                          (1|site),
                                        data = analysis_data,
                                        control = glmmTMBControl(optCtrl = list(iter.max = 10000, eval.max = 10000))
)


##### Injury cause (falls, RTA, violence etc) as predictor ######

# Make sure variable stored correctly
analysis_data$injury_cause <- as.factor(analysis_data$injury_cause)
analysis_data$injury_cause <- relevel(analysis_data$injury_cause, ref = "Accidental falls")



# Run unadjusted model
cause_bsi_12_model_unadjusted <- glmmTMB(bsi_global_score_12_months ~ injury_cause +
                                        (1|site),
                                      data = analysis_data
)


# Run adjusted model
cause_bsi_12_model_adjusted <- glmmTMB(bsi_global_score_12_months ~ injury_cause +
                                      age +
                                      sex +
                                      race +
                                      family_income +
                                      any_mh_disorder +
                                      audit_total_score +
                                      previous_tbi +
                                      drug_total_score +
                                      (1|site),
                                    data = analysis_data,
                                    control = glmmTMBControl(optCtrl = list(iter.max = 10000, eval.max = 10000))
)


# Run unadjusted model
cause_phq_12_model_unadjusted <- glmmTMB(phq_global_score_12_months ~ injury_cause +
                                        (1|site),
                                      data = analysis_data
)
# Run adjusted model
cause_phq_12_model_adjusted <- glmmTMB(phq_global_score_12_months ~ injury_cause +
                                      age +
                                      sex +
                                      race +
                                      family_income +
                                      any_mh_disorder +
                                      audit_total_score +
                                      previous_tbi +
                                      drug_total_score +
                                      (1|site),
                                    data = analysis_data,
                                    control = glmmTMBControl(optCtrl = list(iter.max = 10000, eval.max = 10000))
)


##### Hospital admission (IC, ED, discharged) as predictor ######

# Make sure variable stored correctly
analysis_data$admission <- as.factor(analysis_data$admission)
analysis_data$admission <- relevel(analysis_data$admission, ref = "ED Discharge")


# Run unadjusted model
admission_bsi_12_model_unadjusted <- glmmTMB(bsi_global_score_12_months ~ admission +
                                            (1|site),
                                          data = analysis_data
)

# Run adjusted model
admission_bsi_12_model_adjusted <- glmmTMB(bsi_global_score_12_months ~ admission +
                                          age +
                                          sex +
                                          race +
                                          family_income +
                                          any_mh_disorder +
                                          audit_total_score +
                                          previous_tbi +
                                          drug_total_score +
                                          (1|site),
                                        data = analysis_data,
                                        control = glmmTMBControl(optCtrl = list(iter.max = 10000, eval.max = 10000))
)


# Run unadjusted model
admission_phq_12_model_unadjusted <- glmmTMB(phq_global_score_12_months ~ admission +
                                            (1|site),
                                          data = analysis_data,
)



# Run adjusted model
admission_phq_12_model_adjusted <- glmmTMB(phq_global_score_12_months ~ admission +
                                          age +
                                          sex +
                                          race +
                                          family_income +
                                          any_mh_disorder +
                                          audit_total_score +
                                          previous_tbi +
                                          drug_total_score +
                                          (1|site),
                                        data = analysis_data,
                                        control = glmmTMBControl(optCtrl = list(iter.max = 10000, eval.max = 10000))
)

##### Hospital admission/discharge type as predictor ######

# Make sure variable stored correctly
analysis_data$admission_type <- as.factor(analysis_data$admission_type)
analysis_data$admission_type <- relevel(analysis_data$admission_type, ref = "Discharge to home")


# Run unadjusted model
admission_discharge_bsi_12_model_unadjusted <- glmmTMB(bsi_global_score_12_months ~ admission_type +
                                                      (1|site),
                                                    data = analysis_data
)


# Run adjusted model
admission_discharge_bsi_12_model_adjusted <- glmmTMB(bsi_global_score_12_months ~ admission_type +
                                                    age +
                                                    sex +
                                                    race +
                                                    family_income +
                                                    any_mh_disorder +
                                                    audit_total_score +
                                                    previous_tbi +
                                                    drug_total_score +
                                                    (1|site),
                                                  data = analysis_data,
                                                  control = glmmTMBControl(optCtrl = list(iter.max = 10000, eval.max = 10000))
)


# Run unadjusted model
admission_discharge_phq_12_model_unadjusted <- glmmTMB(phq_global_score_12_months ~ admission_type +
                                                      (1|site),
                                                    data = analysis_data
)


# Run adjusted model
admission_discharge_phq_12_model_adjusted <- glmmTMB(phq_global_score_12_months ~ admission_type +
                                                    age +
                                                    sex +
                                                    race +
                                                    family_income +
                                                    any_mh_disorder +
                                                    audit_total_score +
                                                    previous_tbi +
                                                    drug_total_score +
                                                    (1|site),
                                                  data = analysis_data,
                                                  control = glmmTMBControl(optCtrl = list(iter.max = 10000, eval.max = 10000))
)


##### PTA duration as predictor ######

# Make sure variable stored correctly
analysis_data$pta_duration <- as.factor(analysis_data$pta_duration)
analysis_data$pta_duration <- relevel(analysis_data$pta_duration, ref = "None")


# Run unadjusted model
pta_duration_bsi_12_model_unadjusted <- glmmTMB(bsi_global_score_12_months ~ pta_duration +
                                               (1|site),
                                             data = analysis_data
)


# Run adjusted model
pta_duration_bsi_12_model_adjusted <- glmmTMB(bsi_global_score_12_months ~ pta_duration +
                                             age +
                                             sex +
                                             race +
                                             family_income +
                                             any_mh_disorder +
                                             audit_total_score +
                                             previous_tbi +
                                             drug_total_score +
                                             (1|site),
                                           data = analysis_data,
                                           control = glmmTMBControl(optCtrl = list(iter.max = 10000, eval.max = 10000))
)


# Run unadjusted model
pta_duration_phq_12_model_unadjusted <- glmmTMB(phq_global_score_12_months ~ pta_duration +
                                               (1|site),
                                             data = analysis_data
)


# Run adjusted model
pta_duration_phq_12_model_adjusted <- glmmTMB(phq_global_score_12_months ~ pta_duration +
                                             age +
                                             sex +
                                             race +
                                             family_income +
                                             any_mh_disorder +
                                             audit_total_score +
                                             previous_tbi +
                                             drug_total_score +
                                             (1|site),
                                           data = analysis_data,
                                           control = glmmTMBControl(optCtrl = list(iter.max = 10000, eval.max = 10000))
)




##### Ordinal PTA duration as predictor ######


# Run unadjusted model
pta_duration_ordered_bsi_12_model_unadjusted <- glmmTMB(bsi_global_score_12_months ~ pta_duration_ordered +
                                                       (1|site),
                                                     data = analysis_data
)


# Run adjusted model
pta_duration_ordered_bsi_12_model_adjusted <- glmmTMB(bsi_global_score_12_months ~ pta_duration_ordered +
                                                     age +
                                                     sex +
                                                     race +
                                                     family_income +
                                                     any_mh_disorder +
                                                     audit_total_score +
                                                     previous_tbi +
                                                     drug_total_score +
                                                     (1|site),
                                                   data = analysis_data,
                                                   control = glmmTMBControl(optCtrl = list(iter.max = 10000, eval.max = 10000))
)


# Run unadjusted model
pta_duration_ordered_phq_12_model_unadjusted <- glmmTMB(phq_global_score_12_months ~ pta_duration_ordered +
                                                       (1|site),
                                                     data = analysis_data
)


# Run adjusted model
pta_duration_ordered_phq_12_model_adjusted <- glmmTMB(phq_global_score_12_months ~ pta_duration_ordered +
                                                     age +
                                                     sex +
                                                     race +
                                                     family_income +
                                                     any_mh_disorder +
                                                     audit_total_score +
                                                     previous_tbi +
                                                     drug_total_score +
                                                     (1|site),
                                                   data = analysis_data,
                                                   control = glmmTMBControl(optCtrl = list(iter.max = 10000, eval.max = 10000))
)


##### LOC duration as predictor ######

# Make sure variable stored correctly
analysis_data$loc_duration <- as.factor(analysis_data$loc_duration)
analysis_data$loc_duration <- relevel(analysis_data$loc_duration, ref = "None")


# Run unadjusted model
loc_duration_bsi_12_model_unadjusted <- glmmTMB(bsi_global_score_12_months ~ loc_duration +
                                               (1|site),
                                             data = analysis_data
)


# Run adjusted model
loc_duration_bsi_12_model_adjusted <- glmmTMB(bsi_global_score_12_months ~ loc_duration +
                                             age +
                                             sex +
                                             race +
                                             family_income +
                                             any_mh_disorder +
                                             audit_total_score +
                                             previous_tbi +
                                             drug_total_score +
                                             (1|site),
                                           data = analysis_data,
                                           control = glmmTMBControl(optCtrl = list(iter.max = 10000, eval.max = 10000))
)


# Run unadjusted model
loc_duration_phq_12_model_unadjusted <- glmmTMB(phq_global_score_12_months ~ loc_duration +
                                               (1|site),
                                             data = analysis_data
)


# Run adjusted model
loc_duration_phq_12_model_adjusted <- glmmTMB(phq_global_score_12_months ~ loc_duration +
                                             age +
                                             sex +
                                             race +
                                             family_income +
                                             any_mh_disorder +
                                             audit_total_score +
                                             previous_tbi +
                                             drug_total_score +
                                             (1|site),
                                           data = analysis_data,
                                           control = glmmTMBControl(optCtrl = list(iter.max = 10000, eval.max = 10000))
)




##### Ordinal LOC duration as predictor ######

# Run unadjusted model
loc_duration_ordered_bsi_12_model_unadjusted <- glmmTMB(bsi_global_score_12_months ~ loc_duration_ordered +
                                                       (1|site),
                                                     data = analysis_data
)


# Run adjusted model
loc_duration_ordered_bsi_12_model_adjusted <- glmmTMB(bsi_global_score_12_months ~ loc_duration_ordered +
                                                     age +
                                                     sex +
                                                     race +
                                                     family_income +
                                                     any_mh_disorder +
                                                     audit_total_score +
                                                     previous_tbi +
                                                     drug_total_score +
                                                     (1|site),
                                                   data = analysis_data,
                                                   control = glmmTMBControl(optCtrl = list(iter.max = 10000, eval.max = 10000))
)


# Run unadjusted model
loc_duration_ordered_phq_12_model_unadjusted <- glmmTMB(phq_global_score_12_months ~ loc_duration_ordered +
                                                       (1|site),
                                                     data = analysis_data
)


# Run adjusted model
loc_duration_ordered_phq_12_model_adjusted <- glmmTMB(phq_global_score_12_months ~ loc_duration_ordered +
                                                     age +
                                                     sex +
                                                     race +
                                                     family_income +
                                                     any_mh_disorder +
                                                     audit_total_score +
                                                     previous_tbi +
                                                     drug_total_score +
                                                     (1|site),
                                                   data = analysis_data,
                                                   control = glmmTMBControl(optCtrl = list(iter.max = 10000, eval.max = 10000))
)



##### AOC duration as predictor ######

# Make sure variable stored correctly
analysis_data$aoc_duration <- as.factor(analysis_data$aoc_duration)
analysis_data$aoc_duration <- relevel(analysis_data$aoc_duration, ref = "None")


# Run unadjusted model
aoc_duration_bsi_12_model_unadjusted <- glmmTMB(bsi_global_score_12_months ~ aoc_duration +
                                               (1|site),
                                             data = analysis_data
)


# Run adjusted model
aoc_duration_bsi_12_model_adjusted <- glmmTMB(bsi_global_score_12_months ~ aoc_duration +
                                             age +
                                             sex +
                                             race +
                                             family_income +
                                             any_mh_disorder +
                                             audit_total_score +
                                             previous_tbi +
                                             drug_total_score +
                                             (1|site),
                                           data = analysis_data,
                                           control = glmmTMBControl(optCtrl = list(iter.max = 10000, eval.max = 10000))
)


# Run unadjusted model
aoc_duration_phq_12_model_unadjusted <- glmmTMB(phq_global_score_12_months ~ aoc_duration +
                                               (1|site),
                                             data = analysis_data
)


# Run adjusted model
aoc_duration_phq_12_model_adjusted <- glmmTMB(phq_global_score_12_months ~ aoc_duration +
                                             age +
                                             sex +
                                             race +
                                             family_income +
                                             any_mh_disorder +
                                             audit_total_score +
                                             previous_tbi +
                                             drug_total_score +
                                             (1|site),
                                           data = analysis_data,
                                           control = glmmTMBControl(optCtrl = list(iter.max = 10000, eval.max = 10000))
)



##### Ordinal AOC duration as predictor ######


# Run unadjusted model
aoc_duration_ordered_bsi_12_model_unadjusted <- glmmTMB(bsi_global_score_12_months ~ aoc_duration_ordered +
                                                       (1|site),
                                                     data = analysis_data
)


# Run adjusted model
aoc_duration_ordered_bsi_12_model_adjusted <- glmmTMB(bsi_global_score_12_months ~ aoc_duration_ordered +
                                                     age +
                                                     sex +
                                                     race +
                                                     family_income +
                                                     any_mh_disorder +
                                                     audit_total_score +
                                                     previous_tbi +
                                                     drug_total_score +
                                                     (1|site),
                                                   data = analysis_data,
                                                   control = glmmTMBControl(optCtrl = list(iter.max = 10000, eval.max = 10000))
)


# Run unadjusted model
aoc_duration_ordered_phq_12_model_unadjusted <- glmmTMB(phq_global_score_12_months ~ aoc_duration_ordered +
                                                       (1|site),
                                                     data = analysis_data
)


# Run adjusted model
aoc_duration_ordered_phq_12_model_adjusted <- glmmTMB(phq_global_score_12_months ~ aoc_duration_ordered +
                                                     age +
                                                     sex +
                                                     race +
                                                     family_income +
                                                     any_mh_disorder +
                                                     audit_total_score +
                                                     previous_tbi +
                                                     drug_total_score +
                                                     (1|site),
                                                   data = analysis_data,
                                                   control = glmmTMBControl(optCtrl = list(iter.max = 10000, eval.max = 10000))
)


##### AIS score as predictor ######

# Run unadjusted model
ais_bsi_12_model_unadjusted <- glmmTMB(bsi_global_score_12_months ~ ais_injury_severity +
                                      (1|site),
                                    data = analysis_data
)


# Run adjusted model
ais_bsi_12_model_adjusted <- glmmTMB(bsi_global_score_12_months ~ ais_injury_severity +
                                    age +
                                    sex +
                                    race +
                                    family_income +
                                    any_mh_disorder +
                                    audit_total_score +
                                    previous_tbi +
                                    drug_total_score +
                                    (1|site),
                                  data = analysis_data,
                                  control = glmmTMBControl(optCtrl = list(iter.max = 10000, eval.max = 10000))
)


# Run unadjusted model
ais_phq_12_model_unadjusted <- glmmTMB(phq_global_score_12_months ~ ais_injury_severity +
                                      (1|site),
                                    data = analysis_data
)


# Run adjusted model
ais_phq_12_model_adjusted <- glmmTMB(phq_global_score_12_months ~ ais_injury_severity +
                                    age +
                                    sex +
                                    race +
                                    family_income +
                                    any_mh_disorder +
                                    audit_total_score +
                                    previous_tbi +
                                    drug_total_score +
                                    (1|site),
                                  data = analysis_data,
                                  control = glmmTMBControl(optCtrl = list(iter.max = 10000, eval.max = 10000))
)


##### CT result as predictor ######

# Make sure variable stored correctly
analysis_data$imaging_outcome <- as.factor(analysis_data$imaging_outcome)
analysis_data$imaging_outcome <- relevel(analysis_data$imaging_outcome, ref = "Normal")

# Run unadjusted model
imaging_bsi_12_model_unadjusted <- glmmTMB(bsi_global_score_12_months ~ imaging_outcome +
                                          (1|site),
                                        data = analysis_data
)


# Run adjusted model
imaging_bsi_12_model_adjusted <- glmmTMB(bsi_global_score_12_months ~ imaging_outcome +
                                        age +
                                        sex +
                                        race +
                                        family_income +
                                        any_mh_disorder +
                                        audit_total_score +
                                        previous_tbi +
                                        drug_total_score +
                                        (1|site),
                                      data = analysis_data,
                                      control = glmmTMBControl(optCtrl = list(iter.max = 10000, eval.max = 10000))
)


# Run unadjusted model
imaging_phq_12_model_unadjusted <- glmmTMB(phq_global_score_12_months ~ imaging_outcome +
                                          (1|site),
                                        data = analysis_data
)


# Run adjusted model
imaging_phq_12_model_adjusted <- glmmTMB(phq_global_score_12_months ~ imaging_outcome +
                                        age +
                                        sex +
                                        race +
                                        family_income +
                                        any_mh_disorder +
                                        audit_total_score +
                                        previous_tbi +
                                        drug_total_score +
                                        (1|site),
                                      data = analysis_data,
                                      control = glmmTMBControl(optCtrl = list(iter.max = 10000, eval.max = 10000))
)



##### Marshall score as predictor ######

# Run unadjusted model
marshall_bsi_12_model_unadjusted <- glmmTMB(bsi_global_score_12_months ~ marshall_score +
                                           (1|site),
                                         data = analysis_data
)


# Run adjusted model
marshall_bsi_12_model_adjusted <- glmmTMB(bsi_global_score_12_months ~ marshall_score +
                                         age +
                                         sex +
                                         race +
                                         family_income +
                                         any_mh_disorder +
                                         audit_total_score +
                                         previous_tbi +
                                         drug_total_score +
                                         (1|site),
                                       data = analysis_data,
                                       control = glmmTMBControl(optCtrl = list(iter.max = 10000, eval.max = 10000))
)


# Run unadjusted model
marshall_phq_12_model_unadjusted <- glmmTMB(phq_global_score_12_months ~  marshall_score +
                                           (1|site),
                                         data = analysis_data
)


# Run adjusted model
marshall_phq_12_model_adjusted <- glmmTMB(phq_global_score_12_months ~ marshall_score +
                                         age +
                                         sex +
                                         race +
                                         family_income +
                                         any_mh_disorder +
                                         audit_total_score +
                                         previous_tbi +
                                         drug_total_score +
                                         (1|site),
                                       data = analysis_data,
                                       control = glmmTMBControl(optCtrl = list(iter.max = 10000, eval.max = 10000))
)

#
# Save model objects
#

# Save all model objects needed for visualizations
model_list <- list(
  # Injury type models
  injury_type_bsi_12_model_adjusted = injury_type_bsi_12_model_adjusted,
  injury_type_phq_12_model_adjusted = injury_type_phq_12_model_adjusted,

  # GCS severity models
  gcs_bsi_12_model_adjusted = gcs_bsi_12_model_adjusted,
  gcs_phq_12_model_adjusted = gcs_phq_12_model_adjusted,

  # GCS score (continuous) models
  gcs_score_bsi_12_model_adjusted = gcs_score_bsi_12_model_adjusted,
  gcs_score_phq_12_model_adjusted = gcs_score_phq_12_model_adjusted,

  # Injury cause models
  cause_bsi_12_model_adjusted = cause_bsi_12_model_adjusted,
  cause_phq_12_model_adjusted = cause_phq_12_model_adjusted,

  # Admission models
  admission_bsi_12_model_adjusted = admission_bsi_12_model_adjusted,
  admission_phq_12_model_adjusted = admission_phq_12_model_adjusted,
  admission_discharge_bsi_12_model_adjusted = admission_discharge_bsi_12_model_adjusted,
  admission_discharge_phq_12_model_adjusted = admission_discharge_phq_12_model_adjusted,

  # PTA duration models
  pta_duration_bsi_12_model_adjusted = pta_duration_bsi_12_model_adjusted,
  pta_duration_phq_12_model_adjusted = pta_duration_phq_12_model_adjusted,
  pta_duration_ordered_bsi_12_model_adjusted = pta_duration_ordered_bsi_12_model_adjusted,
  pta_duration_ordered_phq_12_model_adjusted = pta_duration_ordered_phq_12_model_adjusted,
 
  # LOC duration models
  loc_duration_bsi_12_model_adjusted = loc_duration_bsi_12_model_adjusted,
  loc_duration_phq_12_model_adjusted = loc_duration_phq_12_model_adjusted,
  loc_duration_ordered_bsi_12_model_adjusted = loc_duration_ordered_bsi_12_model_adjusted,
  loc_duration_ordered_phq_12_model_adjusted = loc_duration_ordered_phq_12_model_adjusted,

  # AOC duration models
  aoc_duration_bsi_12_model_adjusted = aoc_duration_bsi_12_model_adjusted,
  aoc_duration_phq_12_model_adjusted = aoc_duration_phq_12_model_adjusted,
  aoc_duration_ordered_bsi_12_model_adjusted = aoc_duration_ordered_bsi_12_model_adjusted,
  aoc_duration_ordered_phq_12_model_adjusted = aoc_duration_ordered_phq_12_model_adjusted,

  # AIS models
  ais_bsi_12_model_adjusted = ais_bsi_12_model_adjusted,
  ais_phq_12_model_adjusted = ais_phq_12_model_adjusted,

  # Marshall score models
  marshall_bsi_12_model_adjusted = marshall_bsi_12_model_adjusted,
  marshall_phq_12_model_adjusted = marshall_phq_12_model_adjusted,

  # Imaging models
  imaging_bsi_12_model_adjusted = imaging_bsi_12_model_adjusted,
  imaging_phq_12_model_adjusted = imaging_phq_12_model_adjusted
)

# Save models as RDS file
saveRDS(model_list, paste0(output_dir, "all_models.rds"))

#
# Extract and save model estimates and confidence intervals
#

# Initialize empty data frame to store results
model_results <- data.frame()

# Define predictor name mappings
predictor_name_map <- list(
  injury_type_bsi_12_model_adjusted = "Injury Type",
  injury_type_phq_12_model_adjusted = "Injury Type",
  gcs_bsi_12_model_adjusted = "TBI Severity",
  gcs_phq_12_model_adjusted = "TBI Severity",
  gcs_score_bsi_12_model_adjusted = "GCS Score",
  gcs_score_phq_12_model_adjusted = "GCS Score",
  cause_bsi_12_model_adjusted = "Injury Cause",
  cause_phq_12_model_adjusted = "Injury Cause",
  admission_bsi_12_model_adjusted = "Admission",
  admission_phq_12_model_adjusted = "Admission",
  admission_discharge_bsi_12_model_adjusted = "Admission Type",
  admission_discharge_phq_12_model_adjusted = "Admission Type",
  pta_duration_bsi_12_model_adjusted = "PTA Duration",
  pta_duration_phq_12_model_adjusted = "PTA Duration",
  pta_duration_ordered_bsi_12_model_adjusted = "PTA Duration",
  pta_duration_ordered_phq_12_model_adjusted = "PTA Duration",
  loc_duration_bsi_12_model_adjusted = "LOC Duration",
  loc_duration_phq_12_model_adjusted = "LOC Duration",
  loc_duration_ordered_bsi_12_model_adjusted = "LOC Duration",
  loc_duration_ordered_phq_12_model_adjusted = "LOC Duration",
  aoc_duration_bsi_12_model_adjusted = "AOC Duration",
  aoc_duration_phq_12_model_adjusted = "AOC Duration",
  aoc_duration_ordered_bsi_12_model_adjusted = "AOC Duration",
  aoc_duration_ordered_phq_12_model_adjusted = "AOC Duration",
  ais_bsi_12_model_adjusted = "AIS Score",
  ais_phq_12_model_adjusted = "AIS Score",
  marshall_bsi_12_model_adjusted = "Marshall Score",
  marshall_phq_12_model_adjusted = "Marshall Score",
  imaging_bsi_12_model_adjusted = "Imaging Outcome",
  imaging_phq_12_model_adjusted = "Imaging Outcome"
)

# Loop through each model in the list
for (model_name in names(model_list)) {

  # Get the model object
  model <- model_list[[model_name]]

  # Extract fixed effects with confidence intervals and p-values using broom.mixed
  # Use conf.int = TRUE to get confidence intervals
  # broom.mixed calculates p-values automatically for glmmTMB models
  model_tidy <- tidy(model, conf.int = TRUE, conf.level = 0.95, effects = "fixed")

  # Check if p.value column exists, if not calculate from statistic
  if (!"p.value" %in% names(model_tidy)) {
    # Calculate p-values from z-statistics using normal approximation
    model_tidy$p.value <- 2 * (1 - pnorm(abs(model_tidy$statistic)))
  }

  # Ensure df column exists
  if (!"df" %in% names(model_tidy)) {
    model_tidy$df <- NA
  }

  # Filter out intercept
  predictor_rows <- model_tidy[model_tidy$term != "(Intercept)", ]

  # Determine outcome variable from model name
  outcome <- if (grepl("bsi", model_name, ignore.case = TRUE)) {
    "BSI-18"
  } else if (grepl("phq", model_name, ignore.case = TRUE)) {
    "PHQ-9"
  } else {
    "Unknown"
  }

  # Get predictor name
  predictor_name <- predictor_name_map[[model_name]]
  if (is.null(predictor_name)) {
    predictor_name <- NA
  }

  # Add columns to match combined_model_results.csv format
  predictor_rows$effect <- "fixed"
  predictor_rows$group <- NA
  predictor_rows$predictor_group <- NA
  predictor_rows$outcome <- outcome
  predictor_rows$predictor_name <- predictor_name

  # Calculate significance - ensure p.value is not NA
  predictor_rows$significant <- ifelse(is.na(predictor_rows$p.value), FALSE, predictor_rows$p.value < 0.05)

  predictor_rows$term_label <- predictor_rows$term
  predictor_rows$outcome_label <- outcome

  # Reorder columns to match combined_model_results.csv
  predictor_rows <- predictor_rows[, c("effect", "group", "term", "estimate", "std.error",
                                       "statistic", "df", "p.value", "conf.low", "conf.high",
                                       "predictor_group", "outcome", "predictor_name",
                                       "significant", "term_label", "outcome_label")]

  # Append to results data frame
  model_results <- rbind(model_results, predictor_rows)
}

# Save results to CSV file matching combined_model_results.csv format
write.csv(model_results, paste0(output_dir, "adjusted_model_estimates.csv"), row.names = FALSE)


##############################################################################
#
# Test for dose response relationships
#
##############################################################################

# Test whether ordinal consciousness measures show linear dose-response relationships
# by comparing models with ordinal (numeric) vs categorical coding

# Test PTA dose-response for BSI-18
pta_dose_response_bsi_test <- anova(pta_duration_bsi_12_model_adjusted,
                                    pta_duration_ordered_bsi_12_model_adjusted)
cat("PTA-BSI LRT p-value:", pta_dose_response_bsi_test$`Pr(>Chisq)`[2], "\n")

# Test PTA dose-response for PHQ-9
pta_dose_response_phq_test <- anova(pta_duration_phq_12_model_adjusted,
                                    pta_duration_ordered_phq_12_model_adjusted)
cat("PTA-PHQ LRT p-value:", pta_dose_response_phq_test$`Pr(>Chisq)`[2], "\n")

# Test LOC dose-response for BSI-18
loc_dose_response_bsi_test <- anova(loc_duration_bsi_12_model_adjusted,
                                    loc_duration_ordered_bsi_12_model_adjusted)
cat("LOC-BSI LRT p-value:", loc_dose_response_bsi_test$`Pr(>Chisq)`[2], "\n")

# Test LOC dose-response for PHQ-9
loc_dose_response_phq_test <- anova(loc_duration_phq_12_model_adjusted,
                                    loc_duration_ordered_phq_12_model_adjusted)
cat("LOC-PHQ LRT p-value:", loc_dose_response_phq_test$`Pr(>Chisq)`[2], "\n")


# Test AOC dose-response for BSI-18
aoc_dose_response_bsi_test <- anova(aoc_duration_bsi_12_model_adjusted,
                                    aoc_duration_ordered_bsi_12_model_adjusted)
cat("AOC-BSI LRT p-value:", aoc_dose_response_bsi_test$`Pr(>Chisq)`[2], "\n")

# Test AOC dose-response for PHQ-9
aoc_dose_response_phq_test <- anova(aoc_duration_phq_12_model_adjusted,
                                    aoc_duration_ordered_phq_12_model_adjusted)
cat("AOC-PHQ LRT p-value:", aoc_dose_response_phq_test$`Pr(>Chisq)`[2], "\n")


# Save dose-response test results
dose_response_tests <- data.frame(
  predictor = rep(c("PTA Duration", "LOC Duration", "AOC Duration"), each = 3),
  outcome = rep(c("BSI-18", "PHQ-9"), 3),
  chi_square = c(
    pta_dose_response_bsi_test$Chisq[2], pta_dose_response_phq_test$Chisq[2],
    loc_dose_response_bsi_test$Chisq[2], loc_dose_response_phq_test$Chisq[2],
    aoc_dose_response_bsi_test$Chisq[2], aoc_dose_response_phq_test$Chisq[2]
  ),
  df = c(
    pta_dose_response_bsi_test$Df[2], pta_dose_response_phq_test$Df[2],
    loc_dose_response_bsi_test$Df[2], loc_dose_response_phq_test$Df[2],
    aoc_dose_response_bsi_test$Df[2], aoc_dose_response_phq_test$Df[2]
  ),
  p_value = c(
    pta_dose_response_bsi_test$`Pr(>Chisq)`[2], pta_dose_response_phq_test$`Pr(>Chisq)`[2],
    loc_dose_response_bsi_test$`Pr(>Chisq)`[2], loc_dose_response_phq_test$`Pr(>Chisq)`[2],
    aoc_dose_response_bsi_test$`Pr(>Chisq)`[2], aoc_dose_response_phq_test$`Pr(>Chisq)`[2]
  ),
  interpretation = c(
    ifelse(pta_dose_response_bsi_test$`Pr(>Chisq)`[2] >= 0.05, "Linear adequate", "Non-linear"),
    ifelse(pta_dose_response_phq_test$`Pr(>Chisq)`[2] >= 0.05, "Linear adequate", "Non-linear"),
    ifelse(loc_dose_response_bsi_test$`Pr(>Chisq)`[2] >= 0.05, "Linear adequate", "Non-linear"),
    ifelse(loc_dose_response_phq_test$`Pr(>Chisq)`[2] >= 0.05, "Linear adequate", "Non-linear"),
    ifelse(aoc_dose_response_bsi_test$`Pr(>Chisq)`[2] >= 0.05, "Linear adequate", "Non-linear"),
    ifelse(aoc_dose_response_phq_test$`Pr(>Chisq)`[2] >= 0.05, "Linear adequate", "Non-linear")
  )
)

write.csv(dose_response_tests, paste0(output_dir, "dose_response_tests.csv"),
          row.names = FALSE)

# Save dose-response test objects
dose_response_test_objects <- list(
  pta_bsi = pta_dose_response_bsi_test,
  pta_phq = pta_dose_response_phq_test,
  loc_bsi = loc_dose_response_bsi_test,
  loc_phq = loc_dose_response_phq_test,
  aoc_bsi = aoc_dose_response_bsi_test,
  aoc_phq = aoc_dose_response_phq_test
)
saveRDS(dose_response_test_objects, paste0(output_dir, "dose_response_test_objects.rds"))


##############################################################################
#
# Comparison of severity measures: Which best predicts outcomes?
#
# Uses separate complete-case samples for each severity measure to maximize
# power and avoid unnecessary data loss. Compares predictive performance using:
# 1. Incremental R² (variance explained beyond covariates)
# 2. Cross-validated RMSE and R² (out-of-sample prediction accuracy)
# 3. Likelihood ratio tests
#
# Each severity measure is evaluated on all available data for that measure,
# so sample sizes (N) vary across comparisons.
#
##############################################################################

# Define function to compare a single severity measure
compare_severity_measure <- function(data, severity_var, outcome, covariates) {

  # Create formula strings
  baseline_formula <- paste(outcome, "~", paste(covariates, collapse = " + "))
  full_formula <- paste(baseline_formula, "+", severity_var)

  # Subset to cases with this severity measure (and outcome)
  complete_vars <- c(outcome, severity_var, covariates)
  complete_data <- data[complete.cases(data[, complete_vars]), ]

  # Check if we have enough data
  if (nrow(complete_data) < 50) {
    warning(paste("Only", nrow(complete_data), "complete cases for", severity_var))
    return(data.frame(
      severity_measure = severity_var,
      outcome = outcome,
      n = nrow(complete_data),
      incremental_r2 = NA,
      p_value = NA,
      cv_rmse = NA,
      cv_r2 = NA,
      cv_mae = NA,
      baseline_r2 = NA,
      full_r2 = NA
    ))
  }

  # Fit models
  baseline <- lm(as.formula(baseline_formula), data = complete_data)
  full <- lm(as.formula(full_formula), data = complete_data)

  # Compare models with likelihood ratio test
  lrt <- anova(baseline, full)

  # Calculate R² values
  baseline_r2 <- summary(baseline)$r.squared
  full_r2 <- summary(full)$r.squared
  inc_r2 <- full_r2 - baseline_r2

  # Cross-validate full model on this sample
  set.seed(123)  # For reproducibility
  cv_result <- train(as.formula(full_formula),
                     data = complete_data,
                     method = "lm",
                     trControl = trainControl(method = "cv", number = 10))

  return(data.frame(
    severity_measure = severity_var,
    outcome = outcome,
    n = nrow(complete_data),
    incremental_r2 = inc_r2,
    p_value = lrt$`Pr(>F)`[2],
    cv_rmse = cv_result$results$RMSE,
    cv_r2 = cv_result$results$Rsquared,
    cv_mae = cv_result$results$MAE,
    baseline_r2 = baseline_r2,
    full_r2 = full_r2
  ))
}

# Define covariates (same as in adjusted models)
covariates <- c("age", "sex", "race", "family_income", "any_mh_disorder",
                "audit_total_score", "previous_tbi", "drug_total_score")

# Define severity measures to compare
severity_measures <- c(
  "gcs_severity",
  "gcs_score_reversed",
  "loc_duration_ordered",
  "pta_duration_ordered",
  "aoc_duration_ordered",
  "marshall_score",
  "ais_injury_severity",
  "imaging_outcome",
  "injury_cause",
  "admission"
)

# Define outcomes
outcomes <- c("bsi_global_score_12_months",
              "phq_global_score_12_months")

# Run comparisons for all combinations
cat("\nComparing severity measures for predictive performance...\n")
cat("This may take a few minutes due to cross-validation...\n\n")

all_comparisons <- data.frame()

for (outcome in outcomes) {
  cat("Analyzing outcome:", outcome, "\n")

  for (severity_var in severity_measures) {
    cat("  - Severity measure:", severity_var, "\n")

    result <- compare_severity_measure(
      data = analysis_data,
      severity_var = severity_var,
      outcome = outcome,
      covariates = covariates
    )

    all_comparisons <- rbind(all_comparisons, result)
  }
  cat("\n")
}

# Add interpretive columns
all_comparisons$significant <- all_comparisons$p_value < 0.05
all_comparisons$significant[is.na(all_comparisons$p_value)] <- NA

# Sort by outcome and incremental R² (descending)
all_comparisons <- all_comparisons %>%
  arrange(outcome, desc(incremental_r2))

# Print summary
cat("\n=== SUMMARY: Top 3 Predictors by Incremental R² ===\n\n")
for (outcome_var in outcomes) {
  outcome_data <- all_comparisons[all_comparisons$outcome == outcome_var, ]
  cat(outcome_var, ":\n")
  top3 <- head(outcome_data[, c("severity_measure", "n", "incremental_r2", "p_value", "cv_rmse")], 3)
  print(top3, row.names = FALSE)
  cat("\n")
}

# Save full results
write.csv(
  all_comparisons,
  paste0(output_dir, "severity_measure_comparison.csv"),
  row.names = FALSE
)

cat("Results saved to:", paste0(output_dir, "severity_measure_comparison.csv"), "\n")

# Create summary table for each outcome showing ranking
for (outcome_var in outcomes) {
  outcome_data <- all_comparisons[all_comparisons$outcome == outcome_var & !is.na(all_comparisons$incremental_r2), ]

  # Rank by incremental R²
  outcome_data$rank_r2 <- rank(-outcome_data$incremental_r2, ties.method = "min")

  # Rank by CV RMSE (lower is better)
  outcome_data$rank_cv <- rank(outcome_data$cv_rmse, ties.method = "min")

  # Create summary
  summary_table <- outcome_data[, c("severity_measure", "n", "incremental_r2",
                                     "p_value", "cv_rmse", "cv_r2",
                                     "rank_r2", "rank_cv", "significant")]

  # Save outcome-specific summary
  outcome_clean <- gsub("_", "", outcome_var)
  write.csv(
    summary_table,
    paste0(output_dir, "severity_comparison_", outcome_clean, ".csv"),
    row.names = FALSE
  )
}

cat("\nSeverity measure comparison analysis complete.\n")
