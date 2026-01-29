################################################################
#
# Analysis of TRACK-TBI dataset
#
# Revill et al
#
# This script loads results from analysis_glmmTMB.R and creates all visualisations
#
################################################################

# Load libraries
library(dplyr)
library(ggplot2)
library(patchwork)
library(gt)
library(gtsummary)
library(sjPlot)
library(tibble)
library(tidyr)
library(viridis)
library(ggeffects)
library(gridExtra)
library(scales)

# Remove everything from memory if needed to start afresh
rm(list = ls())

imputed_analysis <- FALSE

# Set output directory
if (imputed_analysis) {
  cat("Using IMPUTED data output\n")
  output_dir <- "S:/Track_TBI/Grace/GraceCode/output_imp/"
} else {
  cat("Using NON-IMPUTED data output\n")
  output_dir <- "S:/Track_TBI/Grace/GraceCode/output_nonimp/"
}


##############################################################################
# LOAD SAVED RESULTS AND MODEL OBJECTS
##############################################################################

cat("Loading model objects and data...\n")

# Load all model objects
model_list <- readRDS(paste0(output_dir, "all_models.rds"))

# Extract individual models from list
list2env(model_list, envir = .GlobalEnv)

# Load combined results
# all_results <- read.csv(paste0(output_dir, "combined_model_results.csv"))


##############################################################################
# HELPER FUNCTIONS
##############################################################################

# Function to create clean predictor labels
create_predictor_labels <- function(term) {
  labels <- case_when(
    grepl("injury_type", term) ~ gsub("injury_type", "", term),
    grepl("gcs_severity|tbi_severity", term) ~ gsub("gcs_severity|tbi_severity", "", term),
    grepl("injury_cause", term) ~ gsub("injury_cause", "", term),
    grepl("ct_abnormal", term) ~ "CT Abnormal",
    grepl("admission_type", term) ~ gsub("Admission to hospital - ", "", 
                                         gsub("admission_type", "", term)),
    grepl("admission", term) ~ gsub("admission", "", term),
    grepl("pta_duration", term) & !grepl("ordered", term) ~ gsub("pta_duration", "PTA: ", term),
    grepl("loc_duration", term) & !grepl("ordered", term) ~ gsub("loc_duration", "LOC: ", term),
    grepl("aoc_duration", term) & !grepl("ordered", term) ~ gsub("aoc_duration", "AOC: ", term),
    term == "pta_duration_ordered" ~ "PTA Duration (ordinal)",
    term == "loc_duration_ordered" ~ "LOC Duration (ordinal)",
    term == "aoc_duration_ordered" ~ "AOC Duration (ordinal)",
    term == "ais_injury_severity" ~ "AIS Score",
    term == "marshall_score" ~ "Marshall Score",
    term == "gcs_score_reversed" ~ "GCS Score",
    TRUE ~ term
  )
  return(labels)
}

# Function to calculate axis limits from models
# Extracts confidence intervals from models and adds padding
calc_axis_lim <- function(..., padding = 0.1) {
  models <- list(...)

  # Extract confidence intervals from all models
  all_ci <- lapply(models, function(m) {
    ci <- confint(m, method = "Wald")
    # Remove intercept, covariates, and random effects
    # Only keep predictor terms (injury_type, gcs, imaging, admission, cause, duration, scores)
    keep_pattern <- "injury_type|gcs_severity|gcs_score_reversed|imaging_outcome|admission|injury_cause|pta_duration|loc_duration|aoc_duration|ais_injury|marshall_score"
    ci <- ci[grepl(keep_pattern, rownames(ci)), , drop = FALSE]
    ci
  })

  # Combine all CIs
  all_ci_combined <- do.call(rbind, all_ci)

  # Get min and max
  ci_min <- min(all_ci_combined[, 1], na.rm = TRUE)
  ci_max <- max(all_ci_combined[, 2], na.rm = TRUE)

  # Add padding
  range_size <- ci_max - ci_min
  lower <- ci_min - (range_size * padding)
  upper <- ci_max + (range_size * padding)

  # Ensure 0 is visible if close to the range
  if (lower > 0 && lower < range_size * 0.5) lower <- -0.1
  if (upper < 0 && upper > -range_size * 0.5) upper <- 0.1

  return(c(lower, upper))
}

##############################################################################
# BASIC VISUALIZATIONS USING TBLREGRESSION AND SJPLOT
##############################################################################

##### Injury type (TBI or Ortho injury) as predictor ######

# Create adjusted model table for BSI
injury_type_bsi_adjusted_table  <- tbl_regression(injury_type_bsi_12_model_adjusted,
                                                  estimate_fun = function(x) style_number(x, digits = 2),
                                                  include = injury_type, 
                                                  label = injury_type ~ "Injury type") %>% 
  italicize_levels() %>%
  bold_labels() %>%
  remove_row_type(variable = injury_type, type = 'reference') %>%
  modify_header(estimate = "**Adjusted Beta**") %>%
  modify_column_merge(pattern = "{estimate} ({ci})", rows = !is.na(estimate))


# Create adjusted model table for PHQ
injury_type_phq_adjusted_table  <- tbl_regression(injury_type_phq_12_model_adjusted, 
                                                  estimate_fun = function(x) style_number(x, digits = 2),
                                                  include = injury_type, 
                                                  label = injury_type ~ "Injury type") %>% 
  italicize_levels() %>%
  remove_row_type(variable = injury_type, type = 'reference') %>%
  bold_labels() %>%
  modify_header(estimate = "**Adjusted Beta**") %>%
  modify_column_merge(pattern = "{estimate} ({ci})", rows = !is.na(estimate))


##### Injury severity (mild, moderate, severe using GCS scoring system) as predictor ######

# Create adjusted model table for BSI
gcs_bsi_adjusted_table  <- tbl_regression(gcs_bsi_12_model_adjusted, 
                                          estimate_fun = function(x) style_number(x, digits = 2),
                                          include = gcs_severity, 
                                          label = gcs_severity ~ "TBI severity") %>% 
  italicize_levels() %>%
  bold_labels() %>%
  remove_row_type(variable = gcs_severity, type = 'reference') %>%
  modify_header(estimate = "**Adjusted Beta**") %>%
  modify_column_merge(pattern = "{estimate} ({ci})", rows = !is.na(estimate))


# Create adjusted model table for PHQ
gcs_phq_adjusted_table  <- tbl_regression(gcs_phq_12_model_adjusted, 
                                          estimate_fun = function(x) style_number(x, digits = 2),
                                          include = gcs_severity, 
                                          label = gcs_severity ~ "TBI severity") %>% 
  italicize_levels() %>%
  remove_row_type(variable = gcs_severity, type = 'reference') %>%
  bold_labels() %>%
  modify_header(estimate = "**Adjusted Beta**") %>%
  modify_column_merge(pattern = "{estimate} ({ci})", rows = !is.na(estimate))



##### Injury cause (falls, RTA, violence etc) as predictor ######

# Create adjusted model table for BSI
cause_bsi_adjusted_table  <- tbl_regression(cause_bsi_12_model_adjusted, 
                                            estimate_fun = function(x) style_number(x, digits = 2),
                                            include = injury_cause, 
                                            label = injury_cause ~ "Injury cause (reference = accidental falls)") %>% 
  italicize_levels() %>%
  bold_labels() %>%
  remove_row_type(variable = injury_cause, type = 'reference') %>%
  modify_header(estimate = "**Adjusted Beta**") %>%
  modify_column_merge(pattern = "{estimate} ({ci})]", rows = !is.na(estimate))


# Create adjusted model table for PHQ
cause_phq_adjusted_table  <- tbl_regression(cause_phq_12_model_adjusted, 
                                            estimate_fun = function(x) style_number(x, digits = 2),
                                            include = injury_cause, 
                                            label = injury_cause ~ "Injury cause (reference = accidental falls)") %>% 
  italicize_levels() %>%
  remove_row_type(variable = injury_cause, type = 'reference') %>%
  bold_labels() %>%
  modify_header(estimate = "**Adjusted Beta**") %>%
  modify_column_merge(pattern = "{estimate} ({ci})", rows = !is.na(estimate))



##### Hospital admission (IC, ED, discharged) as predictor ######

# Create adjusted model table for BSI
admission_bsi_adjusted_table  <- tbl_regression(admission_bsi_12_model_adjusted, 
                                                estimate_fun = function(x) style_number(x, digits = 2),
                                                include = admission, 
                                                label = admission ~ "Admission type (reference = ED discharge)") %>% 
  italicize_levels() %>%
  bold_labels() %>%
  remove_row_type(variable = admission, type = 'reference') %>%
  modify_header(estimate = "**Adjusted Beta**") %>%
  modify_column_merge(pattern = "{estimate} ({ci})", rows = !is.na(estimate))


# Create adjusted model table for PHQ
admission_phq_adjusted_table  <- tbl_regression(admission_phq_12_model_adjusted, 
                                                estimate_fun = function(x) style_number(x, digits = 2),
                                                include = admission, 
                                                label = admission ~ "Admission type (reference = ED discharge)") %>% 
  italicize_levels() %>%
  remove_row_type(variable = admission, type = 'reference') %>%
  bold_labels() %>%
  modify_header(estimate = "**Adjusted Beta**") %>%
  modify_column_merge(pattern = "{estimate} ({ci})", rows = !is.na(estimate))



##### Hospital discharge/admission as predictor ######

# Create adjusted model table for BSI
admission_discharge_bsi_adjusted_table  <- tbl_regression(admission_discharge_bsi_12_model_adjusted, 
                                                          estimate_fun = function(x) style_number(x, digits = 2),
                                                          include = admission_type, 
                                                          label = admission_type ~ "Admission type (reference = Discharge to home)") %>% 
  italicize_levels() %>%
  bold_labels() %>%
  remove_row_type(variable = admission_type, type = 'reference') %>%
  modify_header(estimate = "**Adjusted Beta**") %>%
  modify_column_merge(pattern = "{estimate} ({ci})", rows = !is.na(estimate))


# Create adjusted model table for PHQ
admission_discharge_phq_adjusted_table  <- tbl_regression(admission_discharge_phq_12_model_adjusted, 
                                                          estimate_fun = function(x) style_number(x, digits = 2),
                                                          include = admission_type, 
                                                          label = admission_type ~ "Admission type (reference = Discharge to home)") %>% 
  italicize_levels() %>%
  remove_row_type(variable = admission_type, type = 'reference') %>%
  bold_labels() %>%
  modify_header(estimate = "**Adjusted Beta**") %>%
  modify_column_merge(pattern = "{estimate} ({ci})", rows = !is.na(estimate))


# Plot 1: Categorical injury variables and mental health symptoms at 12 months

cat("Creating categorical injury variables plot...\n")

# Calculate axis limits automatically
bsi_cat_axis_lim <- calc_axis_lim(injury_type_bsi_12_model_adjusted, gcs_bsi_12_model_adjusted,
                                   cause_bsi_12_model_adjusted, admission_bsi_12_model_adjusted,
                                   imaging_bsi_12_model_adjusted)
phq_cat_axis_lim <- calc_axis_lim(injury_type_phq_12_model_adjusted, gcs_phq_12_model_adjusted,
                                   cause_phq_12_model_adjusted, admission_phq_12_model_adjusted,
                                   imaging_phq_12_model_adjusted)

# BSI
# Terms appear in order: imaging_outcome, admission (2 levels), injury_cause (3 levels), gcs_severity (2 levels), injury_type
plot1 <- plot_models(imaging_bsi_12_model_adjusted, admission_bsi_12_model_adjusted, cause_bsi_12_model_adjusted, gcs_bsi_12_model_adjusted, injury_type_bsi_12_model_adjusted,
                     axis.lim = bsi_cat_axis_lim,
                     rm.terms = c("age", "sexMale", "raceBlack", "raceAsian", "raceOther",
                                  "family_income<50K", "family_income>=50K & <100K", "family_income>=100K",
                                  "any_mh_disorder", "audit_total_score", "previous_tbiNo", "drug_total_score",
                                  "sd__(Intercept)", "sd__Observation"),
                     colors = "steelblue", vline.color = "black",
                     show.values = TRUE, value.size = 4, spacing = 0.6, show.legend = FALSE,
                     axis.labels = c("TBI (vs Ortho)",
                                     "Severe TBI (vs Mild)", "Moderate TBI (vs Mild)",
                                     "Other cause (vs Falls)", "Violence/assault (vs Falls)", "Road traffic accident (vs Falls)",
                                     "ICU admission (vs ED discharge)", "Hospital admission (vs ED discharge)",
                                     "Abnormal CT (vs Normal)"
                     ))

# PHQ
# Terms appear in order: imaging_outcome, admission (2 levels), injury_cause (3 levels), gcs_severity (2 levels), injury_type
plot2 <- plot_models(imaging_phq_12_model_adjusted, admission_phq_12_model_adjusted, cause_phq_12_model_adjusted, gcs_phq_12_model_adjusted, injury_type_phq_12_model_adjusted,
                     axis.lim = phq_cat_axis_lim,
                     rm.terms = c("age", "sexMale", "raceBlack", "raceAsian", "raceOther",
                                  "family_income<50K", "family_income>=50K & <100K", "family_income>=100K",
                                  "any_mh_disorder", "audit_total_score", "previous_tbiNo", "drug_total_score",
                                  "sd__(Intercept)", "sd__Observation"),
                     colors = "indianred", vline.color = "black",
                     show.values = TRUE, value.size = 4, spacing = 0.6, show.legend = FALSE,
                     axis.labels = c("TBI (vs Ortho)",
                                     "Severe TBI (vs Mild)", "Moderate TBI (vs Mild)",
                                     "Other cause (vs Falls)", "Violence/assault (vs Falls)", "Road traffic accident (vs Falls)",
                                     "ICU admission (vs ED discharge)", "Hospital admission (vs ED discharge)",
                                     "Abnormal CT (vs Normal)"
                     ))

# Combine BSI and PHQ plots 
plot_total <- plot1 + plot2

plot_total + plot_annotation(
  title = 'Association between injury variables and A) BSI-18 global score B) PHQ-9 total score at 12 months following TBI', tag_levels = 'A'
)

# Save 
png(file=paste(output_dir, '/', 'psyc_symptoms_categorical.png', sep=''), width = 800, height = 600)
plot_total + ggtitle('Association between injury variables and A) BSI-18 global score B) PHQ-9 total score at 12 months following TBI') + theme(plot.title = element_text(hjust = 1))
dev.off()

# Save as PDF
pdf(file=paste(output_dir, '/', 'psyc_symptoms_categorical.pdf', sep=''), width = 14, height = 8.5)
plot_total  + ggtitle('Association between injury variables and A) BSI-18 global score B) PHQ-9 total score at 12 months following TBI') + theme(plot.title = element_text(size =5 , hjust = 1))
dev.off()


# Plot 2: Continuous injury variables and mental health symptoms at 12 months

# Calculate axis limits automatically
bsi_cont_axis_lim <- calc_axis_lim(pta_duration_ordered_bsi_12_model_adjusted, loc_duration_ordered_bsi_12_model_adjusted,
                                    aoc_duration_ordered_bsi_12_model_adjusted, ais_bsi_12_model_adjusted,
                                    marshall_bsi_12_model_adjusted)
phq_cont_axis_lim <- calc_axis_lim(pta_duration_ordered_phq_12_model_adjusted, loc_duration_ordered_phq_12_model_adjusted,
                                    aoc_duration_ordered_phq_12_model_adjusted, ais_phq_12_model_adjusted,
                                    marshall_phq_12_model_adjusted)

# BSI
# Terms appear in order: marshall_score, ais_injury_severity, aoc_duration_ordered, loc_duration_ordered, pta_duration_ordered
plot1 <- plot_models(marshall_bsi_12_model_adjusted, ais_bsi_12_model_adjusted, aoc_duration_ordered_bsi_12_model_adjusted,
                     loc_duration_ordered_bsi_12_model_adjusted, pta_duration_ordered_bsi_12_model_adjusted,
                     axis.lim = bsi_cont_axis_lim,
                     rm.terms = c("age", "sexMale", "raceBlack", "raceAsian", "raceOther",
                                  "family_income<50K", "family_income>=50K & <100K", "family_income>=100K",
                                  "any_mh_disorder", "audit_total_score", "previous_tbiNo", "drug_total_score",
                                  "sd__(Intercept)", "sd__Observation"),
                     colors = "steelblue", vline.color = "black",
                     show.values = TRUE, value.size = 4, spacing = 0.6, show.legend = FALSE,
                     axis.labels = c("PTA duration (ordinal)", "LOC duration (ordinal)",
                                     "AOC duration (ordinal)", "AIS score", "Marshall score"
                     ))

# PHQ
# Terms appear in order: marshall_score, ais_injury_severity, aoc_duration_ordered, loc_duration_ordered, pta_duration_ordered
plot2 <- plot_models(marshall_phq_12_model_adjusted, ais_phq_12_model_adjusted, aoc_duration_ordered_phq_12_model_adjusted,
                     loc_duration_ordered_phq_12_model_adjusted, pta_duration_ordered_phq_12_model_adjusted,
                     axis.lim = phq_cont_axis_lim,
                     rm.terms = c("age", "sexMale", "raceBlack", "raceAsian", "raceOther",
                                  "family_income<50K", "family_income>=50K & <100K", "family_income>=100K",
                                  "any_mh_disorder", "audit_total_score", "previous_tbiNo", "drug_total_score",
                                  "sd__(Intercept)", "sd__Observation"),
                     colors = "indianred", vline.color = "black",
                     show.values = TRUE, value.size = 4, spacing = 0.6, show.legend = FALSE,
                     axis.labels = c("PTA duration (ordinal)", "LOC duration (ordinal)",
                                     "AOC duration (ordinal)", "AIS score", "Marshall score"
                     ))

# Combine BSI and PHQ plots 
plot_total <- plot1 + plot2

plot_total + plot_annotation(
  title = 'Association between injury variables and A) BSI-18 global score B) PHQ-9 total score at 12 months following TBI', tag_levels = 'A'
)

# Save 
png(file=paste(output_dir, '/', 'psyc_symptoms_continuous.png', sep=''), width = 800, height = 600)
plot_total + ggtitle('Association between injury variables and A) BSI-18 global score B) PHQ-9 total score at 12 months following TBI') + theme(plot.title = element_text(hjust = 1))
dev.off()

# Save as PDF
pdf(file=paste(output_dir, '/', 'psyc_symptoms_continuous.pdf', sep=''), width = 14, height = 8.5)
plot_total  + ggtitle('Association between injury variables and A) BSI-18 global score B) PHQ-9 total score at 12 months following TBI') + theme(plot.title = element_text(size =5 , hjust = 1))
dev.off()


# Plot 3: PTA, LOC and AOC variables and mental health symptoms at 12 months

# Calculate axis limits automatically
bsi_duration_axis_lim <- calc_axis_lim(pta_duration_bsi_12_model_adjusted, loc_duration_bsi_12_model_adjusted,
                                        aoc_duration_bsi_12_model_adjusted)
phq_duration_axis_lim <- calc_axis_lim(pta_duration_phq_12_model_adjusted, loc_duration_phq_12_model_adjusted,
                                        aoc_duration_phq_12_model_adjusted)

# BSI
plot1 <- plot_models(pta_duration_bsi_12_model_adjusted, loc_duration_bsi_12_model_adjusted, aoc_duration_bsi_12_model_adjusted,
                     axis.lim = bsi_duration_axis_lim,
                     rm.terms = c("age", "sexMale", "raceBlack", "raceAsian", "raceOther",
                                  "family_income<50K", "family_income>=50K & <100K", "family_income>=100K",
                                  "any_mh_disorder", "audit_total_score", "previous_tbiNo", "drug_total_score",
                                  "sd__(Intercept)", "sd__Observation"),
                     vline.color = "black", colors = "steelblue",
                     axis.labels = c("1-7days", "1-24 hrs", "30-59 mins", "1-29 mins", "AOC: < 1 min",
                                     "1-7days", "1-24-hrs", "30-59 mins", "1-29 mins", "LOC: <1 min",
                                     "1-7days", "1-24 hrs", "30-59 mins", "1-29 mins", "PTA: < 1 min"),
                     show.values = TRUE, line.size = 0.5, dot.size = 2, value.size = 3.8, spacing = -0.8, show.legend = FALSE
)

# PHQ
plot2 <- plot_models(pta_duration_phq_12_model_adjusted, loc_duration_phq_12_model_adjusted, aoc_duration_phq_12_model_adjusted,
                     axis.lim = phq_duration_axis_lim,
                     rm.terms = c("age", "sexMale", "raceBlack", "raceAsian", "raceOther",
                                  "family_income<50K", "family_income>=50K & <100K", "family_income>=100K",
                                  "any_mh_disorder", "audit_total_score", "previous_tbiNo", "drug_total_score",
                                  "sd__(Intercept)", "sd__Observation"),
                     vline.color = "black", colors = "indianred",
                     axis.labels = c("1-7days", "1-24 hrs", "30-59 mins", "1-29 mins", "AOC: < 1 min",
                                     "1-7days", "1-24-hrs", "30-59 mins", "1-29 mins", "LOC: < 1 min",
                                     "1-7days", "1-24 hrs", "30-59 mins", "1-29 mins", "PTA: < 1 min"),
                     show.values = TRUE, line.size = 0.5, dot.size = 2, value.size = 3.8, spacing = -0.8, show.legend = FALSE
)

# Combine BSI and PHQ plots 
plot_total <- plot1 + plot2

plot_total + plot_annotation(
  title = 'Association between LOC, PTA and AOC duration and A) BSI-18 global score B) PHQ-9 total score at 12 months following TBI', tag_levels = 'A'
)

# Save 
png(file=paste(output_dir, '/', 'psyc_symptoms_pta_loc_aoc_duration.png', sep=''), width = 800, height = 600)
plot_total  + ggtitle('Association between PTA, LOC and AOC duration and A) BSI-18 global score B) PHQ-9 total score at 12 months following TBI') + theme(plot.title = element_text(hjust = 1))
dev.off()

# Save as PDF
pdf(file=paste(output_dir, '/', 'psyc_symptoms_pta_loc_aoc_duration.pdf', sep=''), width = 14, height = 8.5)
plot_total  + ggtitle('Association between PTA, LOC and AOC duration and A) BSI-18 global score B) PHQ-9 total score at 12 months following TBI') + theme(plot.title = element_text(size =5 , hjust = 1))
dev.off()


##############################################################################
# CUSTOM GGPLOT2 FOREST PLOT - CATEGORICAL PREDICTORS
##############################################################################

cat("Creating custom ggplot2 forest plot for categorical predictors...\n")

# Extract coefficients and confidence intervals from models
library(broom.mixed)

# Function to extract estimates for specific terms
extract_estimates <- function(model, model_name, outcome_name) {
  # Get the tidy output
  model_tidy <- tidy(model, conf.int = TRUE)

  # Print all terms for debugging
  cat("\nTerms in", model_name, "model for", outcome_name, ":\n")
  print(unique(model_tidy$term))

  # Keep only injury-related terms based on their names
  # Filter for terms that contain injury-related keywords (categorical or continuous)
  model_tidy %>%
    filter(grepl("injury_type|gcs_severity|gcs_score_reversed|injury_cause|admission|imaging_outcome|pta_duration|loc_duration|aoc_duration|ais_injury|marshall_score", term)) %>%
    mutate(
      model = model_name,
      outcome = outcome_name
    ) %>%
    select(term, estimate, conf.low, conf.high, model, outcome)
}

# Extract estimates from all relevant models for BSI
bsi_estimates <- bind_rows(
  extract_estimates(injury_type_bsi_12_model_adjusted, "Injury Type", "BSI-18"),
  extract_estimates(gcs_bsi_12_model_adjusted, "TBI Severity", "BSI-18"),
  extract_estimates(cause_bsi_12_model_adjusted, "Injury Cause", "BSI-18"),
  extract_estimates(admission_bsi_12_model_adjusted, "Admission", "BSI-18"),
  extract_estimates(imaging_bsi_12_model_adjusted, "Imaging", "BSI-18")
)

# Extract estimates from all relevant models for PHQ
phq_estimates <- bind_rows(
  extract_estimates(injury_type_phq_12_model_adjusted, "Injury Type", "PHQ-9"),
  extract_estimates(gcs_phq_12_model_adjusted, "TBI Severity", "PHQ-9"),
  extract_estimates(cause_phq_12_model_adjusted, "Injury Cause", "PHQ-9"),
  extract_estimates(admission_phq_12_model_adjusted, "Admission", "PHQ-9"),
  extract_estimates(imaging_phq_12_model_adjusted, "Imaging", "PHQ-9")
)

# Combine BSI and PHQ
all_estimates <- bind_rows(bsi_estimates, phq_estimates)

# Save estimates to CSV files
write.csv(bsi_estimates, paste0(output_dir, "bsi_estimates.csv"), row.names = FALSE)
write.csv(phq_estimates, paste0(output_dir, "phq_estimates.csv"), row.names = FALSE)
write.csv(all_estimates, paste0(output_dir, "all_estimates.csv"), row.names = FALSE)

cat("\nSaved estimates to CSV files:\n")
cat("  -", paste0(output_dir, "bsi_estimates.csv"), "\n")
cat("  -", paste0(output_dir, "phq_estimates.csv"), "\n")
cat("  -", paste0(output_dir, "all_estimates.csv"), "\n")

# Print unique terms to diagnose any issues
cat("Unique terms extracted:\n")
print(unique(all_estimates$term))

# Print admission-related terms specifically for debugging
cat("\nAdmission-related terms:\n")
admission_terms <- all_estimates %>% filter(grepl("admission", term, ignore.case = TRUE))
print(admission_terms$term)

# Create clean labels based on actual term names from the CSV files
all_estimates <- all_estimates %>%
  mutate(
    label = case_when(
      grepl("injury_typeTBI", term) ~ "TBI (vs Ortho)",
      grepl("gcs_severityModerate", term) ~ "Moderate TBI (vs mild)",
      grepl("gcs_severitySevere", term) ~ "Severe TBI (vs mild)",
      grepl("injury_causeRoad", term) ~ "Road traffic accident (vs falls)",
      grepl("injury_causeViolence", term) ~ "Violence/assault (vs falls)",
      grepl("injury_causeOther", term) ~ "Other cause (vs falls)",
      # Match based on actual term names from CSV
      term == "admissionHospital admit with ICU" ~ "ICU admission (vs ED discharge)",
      term == "admissionHospital admit no ICU" ~ "Hospital admission (vs ED discharge)",
      grepl("imaging_outcomeAbnormal", term) ~ "Abnormal CT (vs normal)",
      TRUE ~ term
    )
  )

# Print all labels to verify
cat("\nAll labels extracted:\n")
print(unique(all_estimates$label))

# Filter to only the terms we want
all_estimates <- all_estimates %>%
  filter(label %in% c("Abnormal CT (vs normal)", "ICU admission (vs ED discharge)",
                      "Hospital admission (vs ED discharge)", "Other cause (vs falls)",
                      "Violence/assault (vs falls)", "Road traffic accident (vs falls)",
                      "Severe TBI (vs mild)", "Moderate TBI (vs mild)", "TBI (vs Ortho)"))

# Create forest plot
forest_plot_custom <- ggplot(all_estimates, aes(x = estimate, y = label, color = outcome)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.8) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.3, linewidth = 1, position = position_dodge(width = 0.5)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("BSI-18" = "steelblue", "PHQ-9" = "indianred"),
                     name = "Outcome") +
  scale_y_discrete(limits = c("TBI (vs Ortho)", "Moderate TBI (vs mild)", "Severe TBI (vs mild)",
                              "Road traffic accident (vs falls)", "Violence/assault (vs falls)",
                              "Other cause (vs falls)", "Hospital admission (vs ED discharge)",
                              "ICU admission (vs ED discharge)", "Abnormal CT (vs normal)")) +
  labs(
    title = "Association Between Categorical Injury Variables and Mental Health Outcomes at 12 Months",
    subtitle = "Adjusted model estimates with 95% confidence intervals",
    x = "Effect Estimate (β coefficient)",
    y = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    axis.text.y = element_text(size = 11)
  )

# Display plot
print(forest_plot_custom)

# Save plot
ggsave(paste0(output_dir, "forest_plot_categorical_custom.png"),
       forest_plot_custom, width = 10, height = 8, dpi = 300)
ggsave(paste0(output_dir, "forest_plot_categorical_custom.pdf"),
       forest_plot_custom, width = 10, height = 8)

cat("Saved custom categorical forest plot\n")


##############################################################################
# CUSTOM GGPLOT2 FOREST PLOT - CONTINUOUS PREDICTORS
##############################################################################

cat("Creating custom ggplot2 forest plot for continuous predictors...\n")

# Extract estimates from continuous variable models for BSI
bsi_continuous_estimates <- bind_rows(
  extract_estimates(gcs_score_bsi_12_model_adjusted, "GCS Score", "BSI-18"),
  extract_estimates(pta_duration_ordered_bsi_12_model_adjusted, "PTA Duration", "BSI-18"),
  extract_estimates(loc_duration_ordered_bsi_12_model_adjusted, "LOC Duration", "BSI-18"),
  extract_estimates(aoc_duration_ordered_bsi_12_model_adjusted, "AOC Duration", "BSI-18"),
  extract_estimates(ais_bsi_12_model_adjusted, "AIS Score", "BSI-18"),
  extract_estimates(marshall_bsi_12_model_adjusted, "Marshall Score", "BSI-18")
)

# Extract estimates from continuous variable models for PHQ
phq_continuous_estimates <- bind_rows(
  extract_estimates(gcs_score_phq_12_model_adjusted, "GCS Score", "PHQ-9"),
  extract_estimates(pta_duration_ordered_phq_12_model_adjusted, "PTA Duration", "PHQ-9"),
  extract_estimates(loc_duration_ordered_phq_12_model_adjusted, "LOC Duration", "PHQ-9"),
  extract_estimates(aoc_duration_ordered_phq_12_model_adjusted, "AOC Duration", "PHQ-9"),
  extract_estimates(ais_phq_12_model_adjusted, "AIS Score", "PHQ-9"),
  extract_estimates(marshall_phq_12_model_adjusted, "Marshall Score", "PHQ-9")
)

# Combine BSI and PHQ continuous estimates
all_continuous_estimates <- bind_rows(bsi_continuous_estimates, phq_continuous_estimates)

# Save continuous estimates to CSV files
write.csv(bsi_continuous_estimates, paste0(output_dir, "bsi_continuous_estimates.csv"), row.names = FALSE)
write.csv(phq_continuous_estimates, paste0(output_dir, "phq_continuous_estimates.csv"), row.names = FALSE)
write.csv(all_continuous_estimates, paste0(output_dir, "all_continuous_estimates.csv"), row.names = FALSE)

cat("\nSaved continuous estimates to CSV files:\n")
cat("  -", paste0(output_dir, "bsi_continuous_estimates.csv"), "\n")
cat("  -", paste0(output_dir, "phq_continuous_estimates.csv"), "\n")
cat("  -", paste0(output_dir, "all_continuous_estimates.csv"), "\n")

# Print unique terms to diagnose
cat("\nUnique continuous terms extracted:\n")
print(unique(all_continuous_estimates$term))

# Create clean labels for continuous variables
all_continuous_estimates <- all_continuous_estimates %>%
  mutate(
    label = case_when(
      grepl("gcs_score_reversed", term) ~ "GCS score (reversed)",
      grepl("pta_duration_ordered", term) ~ "PTA duration (ordinal)",
      grepl("loc_duration_ordered", term) ~ "LOC duration (ordinal)",
      grepl("aoc_duration_ordered", term) ~ "AOC duration (ordinal)",
      grepl("ais_injury_severity", term) ~ "AIS score",
      grepl("marshall_score", term) ~ "Marshall score",
      TRUE ~ term
    )
  )

# Print all continuous labels to verify
cat("\nAll continuous labels extracted:\n")
print(unique(all_continuous_estimates$label))

# Filter to only the continuous terms we want
all_continuous_estimates <- all_continuous_estimates %>%
  filter(label %in% c("GCS score (reversed)", "PTA duration (ordinal)", "LOC duration (ordinal)",
                      "AOC duration (ordinal)", "AIS score", "Marshall score"))

# Create forest plot for continuous variables
forest_plot_continuous <- ggplot(all_continuous_estimates, aes(x = estimate, y = label, color = outcome)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.8) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.3, linewidth = 1, position = position_dodge(width = 0.5)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("BSI-18" = "steelblue", "PHQ-9" = "indianred"),
                     name = "Outcome") +
  scale_y_discrete(limits = c("Marshall score", "AIS score", "AOC duration (ordinal)",
                              "LOC duration (ordinal)", "PTA duration (ordinal)", "GCS score (reversed)")) +
  labs(
    title = "Association Between Continuous Injury Variables and Mental Health Outcomes at 12 Months",
    subtitle = "Adjusted model estimates with 95% confidence intervals",
    x = "Effect Estimate (β coefficient)",
    y = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    axis.text.y = element_text(size = 11)
  )

# Display plot
print(forest_plot_continuous)

# Save plot
ggsave(paste0(output_dir, "forest_plot_continuous_custom.png"),
       forest_plot_continuous, width = 10, height = 6, dpi = 300)
ggsave(paste0(output_dir, "forest_plot_continuous_custom.pdf"),
       forest_plot_continuous, width = 10, height = 6)

cat("Saved custom continuous forest plot\n")


##############################################################################
# CUSTOM GGPLOT2 FOREST PLOT - PTA, LOC, AOC DURATION (CATEGORICAL)
##############################################################################

cat("Creating custom ggplot2 forest plot for PTA, LOC, AOC duration...\n")

# Extract estimates from PTA, LOC, AOC duration models for BSI
bsi_duration_estimates <- bind_rows(
  extract_estimates(pta_duration_bsi_12_model_adjusted, "PTA Duration", "BSI-18"),
  extract_estimates(loc_duration_bsi_12_model_adjusted, "LOC Duration", "BSI-18"),
  extract_estimates(aoc_duration_bsi_12_model_adjusted, "AOC Duration", "BSI-18")
)

# Extract estimates from PTA, LOC, AOC duration models for PHQ
phq_duration_estimates <- bind_rows(
  extract_estimates(pta_duration_phq_12_model_adjusted, "PTA Duration", "PHQ-9"),
  extract_estimates(loc_duration_phq_12_model_adjusted, "LOC Duration", "PHQ-9"),
  extract_estimates(aoc_duration_phq_12_model_adjusted, "AOC Duration", "PHQ-9")
)

# Combine BSI and PHQ duration estimates
all_duration_estimates <- bind_rows(bsi_duration_estimates, phq_duration_estimates)

# Save duration estimates to CSV files
write.csv(bsi_duration_estimates, paste0(output_dir, "bsi_duration_estimates.csv"), row.names = FALSE)
write.csv(phq_duration_estimates, paste0(output_dir, "phq_duration_estimates.csv"), row.names = FALSE)
write.csv(all_duration_estimates, paste0(output_dir, "all_duration_estimates.csv"), row.names = FALSE)

cat("\nSaved duration estimates to CSV files:\n")
cat("  -", paste0(output_dir, "bsi_duration_estimates.csv"), "\n")
cat("  -", paste0(output_dir, "phq_duration_estimates.csv"), "\n")
cat("  -", paste0(output_dir, "all_duration_estimates.csv"), "\n")

# Print unique terms to diagnose
cat("\nUnique duration terms extracted:\n")
print(unique(all_duration_estimates$term))

# Note: After adding relevel() for pta_duration in 01_analysis.R, all three duration variables
# should now have proper categorical levels with "None" as the reference category

# Create clean labels for duration variables
all_duration_estimates <- all_duration_estimates %>%
  mutate(
    # Extract the duration category and type
    duration_type = case_when(
      grepl("pta_duration", term) ~ "PTA",
      grepl("loc_duration", term) ~ "LOC",
      grepl("aoc_duration", term) ~ "AOC",
      TRUE ~ "Unknown"
    ),
    duration_category = case_when(
      grepl("<1 minute|<1min", term, ignore.case = TRUE) ~ "<1 minute",
      grepl("1-29 minutes|1-29min", term, ignore.case = TRUE) ~ "1-29 minutes",
      grepl("30-59 minutes|30-59min", term, ignore.case = TRUE) ~ "30-59 minutes",
      grepl("1-24 hours|1-24hrs|1-24 hrs", term, ignore.case = TRUE) ~ "1-24 hours",
      grepl("1-7 days", term, ignore.case = TRUE) ~ "1-7 days",
      grepl(">7 days|>7days", term, ignore.case = TRUE) ~ ">7 days",
      TRUE ~ term
    ),
    label = paste0(duration_type, ": ", duration_category)
  )

# Print all duration labels to verify
cat("\nAll duration labels extracted:\n")
print(unique(all_duration_estimates$label))

# Define the order of duration categories (from shortest to longest)
duration_order <- c(
  "PTA: <1 minute", "PTA: 1-29 minutes", "PTA: 30-59 minutes", "PTA: 1-24 hours", "PTA: 1-7 days",
  "LOC: <1 minute", "LOC: 1-29 minutes", "LOC: 30-59 minutes", "LOC: 1-24 hours", "LOC: 1-7 days",
  "AOC: <1 minute", "AOC: 1-29 minutes", "AOC: 30-59 minutes", "AOC: 1-24 hours", "AOC: 1-7 days"
)

# Filter to only valid duration labels
all_duration_estimates <- all_duration_estimates %>%
  filter(label %in% duration_order)

cat("\nFinal number of duration estimates for plot:", nrow(all_duration_estimates), "\n")

# Create separate forest plots for BSI-18 and PHQ-9

# BSI-18 plot
forest_plot_duration_bsi <- ggplot(bsi_duration_estimates %>%
                                     mutate(
                                       duration_type = case_when(
                                         grepl("pta_duration", term) ~ "PTA",
                                         grepl("loc_duration", term) ~ "LOC",
                                         grepl("aoc_duration", term) ~ "AOC",
                                         TRUE ~ "Unknown"
                                       ),
                                       duration_category = case_when(
                                         grepl("<1 minute|<1min", term, ignore.case = TRUE) ~ "<1 minute",
                                         grepl("1-29 minutes|1-29min", term, ignore.case = TRUE) ~ "1-29 minutes",
                                         grepl("30-59 minutes|30-59min", term, ignore.case = TRUE) ~ "30-59 minutes",
                                         grepl("1-24 hours|1-24hrs|1-24 hrs", term, ignore.case = TRUE) ~ "1-24 hours",
                                         grepl("1-7 days", term, ignore.case = TRUE) ~ "1-7 days",
                                         grepl(">7 days|>7days", term, ignore.case = TRUE) ~ ">7 days",
                                         TRUE ~ term
                                       ),
                                       label = paste0(duration_type, ": ", duration_category)
                                     ) %>%
                                     filter(label %in% duration_order),
                                   aes(x = estimate, y = label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.8) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.3, linewidth = 0.8, color = "steelblue") +
  geom_point(size = 2.5, color = "steelblue") +
  scale_y_discrete(limits = rev(duration_order)) +
  labs(
    title = "Association Between PTA, LOC, and AOC Duration and BSI-18 at 12 Months",
    subtitle = "Adjusted model estimates with 95% confidence intervals (reference: None)",
    x = "Effect Estimate (β coefficient)",
    y = ""
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    axis.text.y = element_text(size = 10)
  )

# PHQ-9 plot
forest_plot_duration_phq <- ggplot(phq_duration_estimates %>%
                                     mutate(
                                       duration_type = case_when(
                                         grepl("pta_duration", term) ~ "PTA",
                                         grepl("loc_duration", term) ~ "LOC",
                                         grepl("aoc_duration", term) ~ "AOC",
                                         TRUE ~ "Unknown"
                                       ),
                                       duration_category = case_when(
                                         grepl("<1 minute|<1min", term, ignore.case = TRUE) ~ "<1 minute",
                                         grepl("1-29 minutes|1-29min", term, ignore.case = TRUE) ~ "1-29 minutes",
                                         grepl("30-59 minutes|30-59min", term, ignore.case = TRUE) ~ "30-59 minutes",
                                         grepl("1-24 hours|1-24hrs|1-24 hrs", term, ignore.case = TRUE) ~ "1-24 hours",
                                         grepl("1-7 days", term, ignore.case = TRUE) ~ "1-7 days",
                                         grepl(">7 days|>7days", term, ignore.case = TRUE) ~ ">7 days",
                                         TRUE ~ term
                                       ),
                                       label = paste0(duration_type, ": ", duration_category)
                                     ) %>%
                                     filter(label %in% duration_order),
                                   aes(x = estimate, y = label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.8) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.3, linewidth = 0.8, color = "indianred") +
  geom_point(size = 2.5, color = "indianred") +
  scale_y_discrete(limits = rev(duration_order)) +
  labs(
    title = "Association Between PTA, LOC, and AOC Duration and PHQ-9 at 12 Months",
    subtitle = "Adjusted model estimates with 95% confidence intervals (reference: None)",
    x = "Effect Estimate (β coefficient)",
    y = ""
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    axis.text.y = element_text(size = 10)
  )

# Display plots
print(forest_plot_duration_bsi)
print(forest_plot_duration_phq)

# Save BSI-18 plot
ggsave(paste0(output_dir, "forest_plot_duration_bsi_custom.png"),
       forest_plot_duration_bsi, width = 10, height = 12, dpi = 300)
ggsave(paste0(output_dir, "forest_plot_duration_bsi_custom.pdf"),
       forest_plot_duration_bsi, width = 10, height = 12)

# Save PHQ-9 plot
ggsave(paste0(output_dir, "forest_plot_duration_phq_custom.png"),
       forest_plot_duration_phq, width = 10, height = 12, dpi = 300)
ggsave(paste0(output_dir, "forest_plot_duration_phq_custom.pdf"),
       forest_plot_duration_phq, width = 10, height = 12)

cat("Saved custom duration forest plots (BSI-18 and PHQ-9 separately)\n")

##############################################################################
# DOSE-RESPONSE CURVES
##############################################################################

# Get predicted values for BSI
pred_pta_bsi <- ggpredict(pta_duration_bsi_12_model_adjusted, terms = "pta_duration")
pred_loc_bsi <- ggpredict(loc_duration_bsi_12_model_adjusted, terms = "loc_duration")
pred_aoc_bsi <- ggpredict(aoc_duration_bsi_12_model_adjusted, terms = "aoc_duration")

# Combine for BSI
dose_response_bsi <- bind_rows(
  pred_pta_bsi %>% as.data.frame() %>% mutate(Type = "PTA Duration"),
  pred_loc_bsi %>% as.data.frame() %>% mutate(Type = "LOC Duration"),
  pred_aoc_bsi %>% as.data.frame() %>% mutate(Type = "AOC Duration")
) %>%
  mutate(
    x = factor(x, levels = c("None", "<1 minute", "1-29 minutes", "30-59 minutes", "1-24 hours", "1-7 days", 
                             ">7 days")),
    Type = factor(Type, levels = c("PTA Duration", "LOC Duration", "AOC Duration"))
  )

dose_response_plot_bsi <- ggplot(dose_response_bsi, 
                                  aes(x = x, y = predicted, group = Type, color = Type)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Type), 
              alpha = 0.2, color = NA) +
  scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +
  scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +
  scale_x_discrete(labels = c("None" = "None", 
                               "<1 minute" = "<1min", 
                               "1-29 minutes" = "1-29mins", 
                               "30-59 minutes" = "30-59mins", 
                               "1-24 hours" = "1-24hrs", 
                               "1-7 days" = "1-7days", 
                               ">7 days" = ">7days")) +
  facet_wrap(~Type, ncol = 1, scales = "free_x") +
  labs(
    title = "Dose-Response: Consciousness Disturbance Duration and BSI-18",
    subtitle = "Predicted values from adjusted models with 95% CI",
    x = "Duration Category",
    y = "Predicted BSI-18 Global Score at 12 Months"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    strip.text = element_text(face = "bold", size = 11),
    strip.background = element_rect(fill = "gray90", color = NA),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 13)
  )

ggsave(paste0(output_dir, "dose_response_consciousness_BSI.png"), 
       dose_response_plot_bsi, width = 10, height = 12, dpi = 300)
ggsave(paste0(output_dir, "dose_response_consciousness_BSI.pdf"), 
       dose_response_plot_bsi, width = 10, height = 12)



# PHQ-9 dose response
pred_pta_phq <- ggpredict(pta_duration_phq_12_model_adjusted, terms = "pta_duration")
pred_loc_phq <- ggpredict(loc_duration_phq_12_model_adjusted, terms = "loc_duration")
pred_aoc_phq <- ggpredict(aoc_duration_phq_12_model_adjusted, terms = "aoc_duration")

dose_response_phq <- bind_rows(
  pred_pta_phq %>% as.data.frame() %>% mutate(Type = "PTA Duration"),
  pred_loc_phq %>% as.data.frame() %>% mutate(Type = "LOC Duration"),
  pred_aoc_phq %>% as.data.frame() %>% mutate(Type = "AOC Duration")
) %>%
  mutate(
    x = factor(x, levels = c("None", "<1 minute", "1-29 minutes", "30-59 minutes", "1-24 hours", "1-7 days", 
                             ">7 days")),
    Type = factor(Type, levels = c("PTA Duration", "LOC Duration", "AOC Duration"))
  )

dose_response_plot_phq <- ggplot(dose_response_phq, 
                                  aes(x = x, y = predicted, group = Type, color = Type)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Type), 
              alpha = 0.2, color = NA) +
  scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +
  scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +
  scale_x_discrete(labels = c("None" = "None", 
                               "<1 minute" = "<1min", 
                               "1-29 minutes" = "1-29mins", 
                               "30-59 minutes" = "30-59mins", 
                               "1-24 hours" = "1-24hrs", 
                               "1-7 days" = "1-7days", 
                               ">7 days" = ">7days")) +
  facet_wrap(~Type, ncol = 1, scales = "free_x") +
  labs(
    title = "Dose-Response: Consciousness Disturbance Duration and PHQ-9",
    subtitle = "Predicted values from adjusted models with 95% CI",
    x = "Duration Category",
    y = "Predicted PHQ-9 Global Score at 12 Months"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    strip.text = element_text(face = "bold", size = 11),
    strip.background = element_rect(fill = "gray90", color = NA),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 13)
  )

ggsave(paste0(output_dir, "dose_response_consciousness_PHQ.png"), 
       dose_response_plot_phq, width = 10, height = 12, dpi = 300)
ggsave(paste0(output_dir, "dose_response_consciousness_PHQ.pdf"), 
       dose_response_plot_phq, width = 10, height = 12)

