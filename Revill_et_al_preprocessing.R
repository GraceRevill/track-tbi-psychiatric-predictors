################################################################
#
# Preprocessing of TRACK-TBI dataset
#
# Revill et al
#
# Components:
# - Recode data
# - Test for missingness at random
# - Test for association between acute severity measures
#   and missingness of outcome measure
# - Create imputed dataset
#
################################################################

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(Hmisc)
library(patchwork)
library(visdat)
library(mice)
library(gt)
library(gtsummary)
library(naniar)

# Remove everything from memory if needed to start afresh
rm(list = ls())

# Set data and output directories
data_dir <- "S:/Track_TBI/FlatCSV_2023-10-10T03-23-25/"
processed_data_dir <- "S:/Track_TBI/Grace/GraceCode/processed_data/"
output_dir <- "S:/Track_TBI/Grace/GraceCode/output/"

setwd(data_dir)


###########################################################
#
# Load data
#
###########################################################


# Demographics
demographics <- read.csv("S:/Track_TBI/FlatCSV_2023-10-10T03-23-25/query_result_DemogrFITBIR_2023-10-10T03-10-444343538283458394252.csv")
med_history <- read.csv("S:/Track_TBI/FlatCSV_2023-10-10T03-23-25/query_result_MedHx_Appdx_TRACKTBI_2023-10-10T03-18-106585029037693363492.csv")
ohio <- read.csv("S:/Track_TBI/FlatCSV_2023-10-10T03-23-25/query_result_OSUTBIMSF_2023-10-10T03-09-482303788971698144126.csv")

# TBI severity variables
gcs <- read.csv("S:/Track_TBI/FlatCSV_2023-10-10T03-23-25/query_result_GCS_2023-10-10T03-19-391763169094238428624.csv")
injury <- read.csv("S:/Track_TBI/FlatCSV_2023-10-10T03-23-25/query_result_InjHx_FITBIR_2023-10-10T03-11-434335266523938982300.csv")
ais <- read.csv("S:/Track_TBI/FlatCSV_2023-10-10T03-23-25/query_result_AIS_2023-10-10T03-18-1970204024289368364.csv")
imaging <- read.csv("S:/Track_TBI/FlatCSV_2023-10-10T03-23-25/query_result_ImagingRead_FITBIR_2023-10-10T03-11-015430718155238106161.csv")
imaging_appdx <- read.csv("S:/Track_TBI/FlatCSV_2023-10-10T03-23-25/query_result_ImgngRd_Appdx_TRACKTBI_2023-10-10T03-18-391773578634190930423.csv")

# Mental health
bsi <- read.csv("S:/Track_TBI/FlatCSV_2023-10-10T03-23-25/query_result_BSI18_2023-10-10T03-19-053714155967854501741.csv")
phq <- read.csv("S:/Track_TBI/FlatCSV_2023-10-10T03-23-25/query_result_PHQ9_2023-10-10T03-11-508228439344401046069.csv")
pcl <- read.csv("S:/Track_TBI/FlatCSV_2023-10-10T03-23-25/query_result_PCL5_2023-10-10T03-10-031793473900812372875.csv")
interview <- read.csv("S:/Track_TBI/FlatCSV_2023-10-10T03-23-25/query_result_TRACKTBIInterviewMod_2023-10-10T03-09-371957878521648697796.csv")

# Alcohol use (AUDIT-C)
audit <- read.csv("S:/Track_TBI/FlatCSV_2023-10-10T03-23-25/query_result_AUDITC_2023-10-10T03-09-561557536921711864894.csv")

###########################################################
#
# Rename and recode variables
#
###########################################################

# Rename and select demographic variables
demographics <- demographics %>%
  rename(age = DemogrFITBIRV1.1.MainGroup.AgeYrs) %>%
  rename(subjectkey = DemogrFITBIRV1.1.MainGroup.GUID)%>%
  rename(site = DemogrFITBIRV1.1.MainGroup.SiteName) %>%
  rename(injury_type = DemogrFITBIRV1.1.MainGroup.CaseContrlInd)%>%
  rename(admission = DemogrFITBIRV1.1.FormAdministration.ContextTypeOTH) %>%
  rename(sex = DemogrFITBIRV1.1.SubjectDemographics.GenderTyp) %>%
  rename(race = DemogrFITBIRV1.1.SubjectDemographics.RaceUSACat) %>%
  rename(family_income = DemogrFITBIRV1.1.SubjectFamilyIncome.FmlyIncRange) %>%
  rename(highest_education = DemogrFITBIRV1.1.SubjectEducation.EduLvlUSATyp) %>%
  rename(employment_status = DemogrFITBIRV1.1.SubjectOccupationandEmployment.OccupationPrimaryStatus) %>%
  rename(death_dropout = DemogrFITBIRV1.1.MainGroup.GeneralNotesTxt) %>%
  select(subjectkey, age, site, injury_type, admission, sex, race, family_income, highest_education, death_dropout)


# Set data type and level labels for injury type variable
demographics$injury_type <- factor(demographics$injury_type,
                                   levels = c("Case", "Control"),
                                   labels = c("TBI", "Ortho"))

# Make sure missing data is coded properly
demographics[demographics == ""] <- NA
demographics[demographics == "Unknown"] <- NA

# Recode ethnicity
demographics$race[demographics$race == 'American Indian or Alaska Native'] <- 'Other'
demographics$race[demographics$race == 'Native Hawaiian or Other Pacific Islander'] <- 'Other'
demographics$race[demographics$race == 'Black or African-American'] <- 'Black'

# Set data type and level labels
demographics$race <- factor(demographics$race,
                            levels = c("White", "Black", "Asian", "Other"))

# Make sure missing data is coded properly
demographics$family_income[demographics$family_income == ''] <- NA
demographics$family_income[demographics$family_income == 'Refused'] <- NA

# Recode household income
demographics$family_income[demographics$family_income == '$100,000 and over'] <- '>=100K'
demographics$family_income[demographics$family_income == '$75,000 to $99,999'] <- '>=50K & <100K'
demographics$family_income[demographics$family_income == '$50,000 to $74,999'] <- '>=50K & <100K'
demographics$family_income[demographics$family_income == '$35,000 to $49,999'] <- '<50K'
demographics$family_income[demographics$family_income == '$25,000 to $34,999'] <- '<50K'
demographics$family_income[demographics$family_income == '$15,000 to $24,999'] <- '<25K'
demographics$family_income[demographics$family_income == 'Under $15,000'] <- '<25K'

# Set data type and level labels
demographics$family_income <- factor(demographics$family_income,
                                     levels = c("<25K", "<50K", ">=50K & <100K", ">=100K"))

xtabs( ~ family_income, data = demographics)

# Remove rows where patient died/dropped out
demographics <- filter(demographics , death_dropout != "Cohort2wk is blank when subject died or withdrew before 2 wk visit completed")

# Rename and select pre-injury mental health variables
med_history <- med_history %>%
  rename(subjectkey = MedHx_Appdx_TRACKTBIV1.0.Main.GUID)%>%
  rename(anxiety_history = MedHx_Appdx_TRACKTBIV1.0.Psychiatric.MedHxPsychiatricAnx) %>%
  rename(depression_history = MedHx_Appdx_TRACKTBIV1.0.Psychiatric.MedHxPsychiatricDep) %>%
  rename(bipolar_history = MedHx_Appdx_TRACKTBIV1.0.Psychiatric.MedHxPsychiatricBipol) %>%
  rename(schizophrenia_history = MedHx_Appdx_TRACKTBIV1.0.Psychiatric.MedHxPsychiatricSchiz) %>%
  rename(ptsd_history = MedHx_Appdx_TRACKTBIV1.0.Psychiatric.MedHxPsychiatricPtsd) %>%
  rename(adhd_history = MedHx_Appdx_TRACKTBIV1.0.Developmental.MedHxDevelopmentalADHD) %>%
  rename(psyc_hospital_history = MedHx_Appdx_TRACKTBIV1.0.PreInjuryQuestions.MedHxHospitalPsych) %>%
  rename(psyc_med_history = MedHx_Appdx_TRACKTBIV1.0.PreInjuryQuestions.MedHxMedsPsych) %>%
  select(subjectkey, anxiety_history, depression_history, bipolar_history, schizophrenia_history, ptsd_history, adhd_history,
         psyc_hospital_history, psyc_med_history)

# Make sure missing data is coded properly
med_history[med_history == ""] <- NA
med_history[med_history == 'Unknown'] <- NA

# Make sure variables stored as factors
med_history$anxiety_history <- as.factor(med_history$anxiety_history)
med_history$depression_history <- as.factor(med_history$depression_history)
med_history$schizophrenia_history <- as.factor(med_history$schizophrenia_history)
med_history$ptsd_history <- as.factor(med_history$ptsd_history)
med_history$adhd_history <- as.factor(med_history$adhd_history)
med_history$bipolar_history <- as.factor(med_history$bipolar_history)
med_history$psyc_hospital_history <- as.factor(med_history$psyc_hospital_history)
med_history$psyc_med_history <- as.factor(med_history$psyc_med_history)

# Create any mental health disorder variable
med_history$any_mh_disorder <- rowSums(med_history[, c("anxiety_history", "depression_history","ptsd_history",
                                                       "schizophrenia_history", "bipolar_history", "adhd_history")] == "Yes")

# Recode
med_history <- med_history %>%
  mutate(mh_disorder= case_when(any_mh_disorder >= 1 ~  "One or more",
                                any_mh_disorder < 1  ~ "None")
  )

med_history$mh_disorder <- as.factor(med_history$mh_disorder)

# Rename and select AUDIT-C scores
audit <- audit %>%
  rename(subjectkey = AUDITCV1.2.Main.GUID) %>%
  rename(audit_timepoint = AUDITCV1.2.Main.DaysSinceBaseline) %>%
  rename(audit_total_score = AUDITCV1.2.AUDITCScoring.AUDITCTotalScore) %>%
  select(subjectkey, audit_timepoint, audit_total_score)

# Remove AUDIT scores that are not related to pre-injury
audit <- audit %>%
  mutate(audit_timepoint = case_when(audit_timepoint < 5 ~  "Admission",
                                     audit_timepoint >= 5  ~ "Not applicable")
  )

audit <- filter(audit, audit_timepoint != "Not applicable")
audit <- subset(audit, select = -c(audit_timepoint))

# Rename and select ohio variables
ohio <- ohio %>%
  rename(subjectkey = OSUTBIMSFV1.0.Main.GUID) %>%
  rename(previous_loc_tbi = OSUTBIMSFV1.0.Scoring.LOCTBICt) %>%
  rename(worst_tbi = OSUTBIMSFV1.0.Scoring.OSUTBIMSWorstInjReslt) %>%
  select(subjectkey, previous_loc_tbi, worst_tbi)

# Remove incomplete rows (rows are duplicated with empty cells)
ohio <- ohio[complete.cases(ohio$worst_tbi),]

# Recode worst previous TBI
ohio <- ohio %>%
  mutate(previous_tbi = case_when(worst_tbi == 1 ~  "No",
                                  worst_tbi == 2  ~ "Yes",
                                  worst_tbi == 3  ~ "Yes",
                                  worst_tbi == 4  ~ "Yes",
                                  worst_tbi == 5  ~ "Yes")
  )

# Make sure variable stored as factors
ohio$previous_tbi <- factor(ohio$previous_tbi,
                            levels = c("Yes", "No"))

# Recode worst previous TBI
ohio <- ohio %>%
  mutate(worst_tbi = case_when(worst_tbi == 1 ~  "None",
                               worst_tbi == 2  ~ "Mild TBI",
                               worst_tbi == 3  ~ "Mild TBI",
                               worst_tbi == 4  ~ "Moderate TBI",
                               worst_tbi == 5  ~ "Severe TBI")
  )

# Set data type and level labels
ohio$worst_tbi <- factor(ohio$worst_tbi,
                         levels = c("Mild TBI", "Moderate TBI", "Severe TBI", "None"))


###########################################################
#
# Rename and recode injury related variables
#
###########################################################

# Rename and select injury severity variables
injury <- injury %>%
  rename(subjectkey = InjHx_FITBIRV1.1.Main.GUID) %>%
  rename(injury_cause = InjHx_FITBIRV1.1.InjuryGeneralInfo.InjCauseTyp) %>%
  rename(injury_mechanism = InjHx_FITBIRV1.1.InjuryGeneralInfo.TBITyp) %>%
  rename(loc_duration = InjHx_FITBIRV1.1.LOCAOCandPTA.LOCDurRang) %>%
  rename(pta_duration = InjHx_FITBIRV1.1.LOCAOCandPTA.PstTraumAmnsDurRang) %>%
  rename(aoc_duration = InjHx_FITBIRV1.1.LOCAOCandPTA.AOCDurRang) %>%
  rename(admission_type = InjHx_FITBIRV1.1.EDDischarge.EmrgyRmDischrgDestTyp) %>%
  rename(loc_indicator = InjHx_FITBIRV1.1.LOCAOCandPTA.LOCInd) %>%
  rename(pta_indicator = InjHx_FITBIRV1.1.LOCAOCandPTA.PstTraumtcAmnsInd) %>%
  rename(aoc_indicator = InjHx_FITBIRV1.1.LOCAOCandPTA.AOCInd) %>%
  select(subjectkey, injury_cause, injury_mechanism, loc_duration, pta_duration, aoc_duration, admission_type,
         loc_indicator, pta_indicator, aoc_indicator)

# Make sure missing data is coded properly
injury[injury == ""] <- NA
injury[injury == "Unknown"] <- NA

# Remove incomplete rows (rows are duplicated with empty cells)
injury <- injury[complete.cases(injury$injury_cause, injury$injury_mechanism),]

# Set data type and level labels
injury$loc_indicator[injury$loc_indicator == 'Suspected'] <- "Yes"

injury$loc_indicator <- factor(injury$loc_indicator,
                               levels = c("Yes", "No"))

# Set data type and level labels
injury$pta_indicator[injury$pta_indicator == 'Suspected'] <- "Yes"

injury$pta_indicator <- factor(injury$pta_indicator,
                               levels = c("Yes", "No"))

# Set data type and level labels
injury$aoc_indicator[injury$aoc_indicator == 'Suspected'] <- "Yes"

injury$aoc_indicator <- factor(injury$aoc_indicator,
                               levels = c("Yes", "No"))

# Set data type and level labels
injury$loc_duration <- factor(injury$loc_duration,
                              levels = c("<1 minute", "1-29 minutes", "30-59 minutes", "1-24 hours","1-7 days",
                                         ">7 days", "None"))

# Set data type and level labels
injury$pta_duration <- factor(injury$pta_duration,
                              levels = c("<1 minute", "1-29 minutes", "30-59 minutes", "1-24 hours","1-7 days",
                                         ">7 days", "None"))

# Set data type and level labels
injury$aoc_duration <- factor(injury$aoc_duration,
                              levels = c("<1 minute", "1-29 minutes", "30-59 minutes", "1-24 hours","1-7 days",
                                         ">7 days", "None"))

# Recode injury cause variable (categories are Falls, Road traffic accidents, Violence/Assault and Other)
injury <- injury %>%
  mutate(injury_cause = case_when(injury_cause == 'Accidental falls' ~ "Accidental falls",
                                  injury_cause == 'Accidental falls;Accidents due to natural and environmental factors (e900-e909)' ~ "Accidental falls",
                                  injury_cause == 'Accidental falls;Other accidents' ~ "Accidental falls",
                                  injury_cause == 'Accidental falls;Homicide and injury purposely inflicted by other persons' ~ "Violence/assault",
                                  injury_cause == 'Accidental falls;Homicide and injury purposely inflicted by other persons;Injury undetermined whether accidentally or purposely inflicted' ~ "Violence/assault",
                                  injury_cause == 'Homicide and injury purposely inflicted by other persons' ~ "Violence/assault",
                                  injury_cause == 'Motor vehicle nontraffic accidents;Homicide and injury purposely inflicted by other persons' ~ "Violence/assault",
                                  injury_cause == 'Motor vehicle traffic accidents;Homicide and injury purposely inflicted by other persons' ~ "Violence/assault",
                                  injury_cause == 'Vehicle accidents not elsewhere classifiable;Homicide and injury purposely inflicted by other persons' ~ "Violence/assault",
                                  injury_cause == 'Motor vehicle nontraffic accidents' ~ "Road traffic accident",
                                  injury_cause == 'Motor vehicle nontraffic accidents;Accidental falls' ~ "Road traffic accident",
                                  injury_cause == 'Motor vehicle nontraffic accidents;Injury undetermined whether accidentally or purposely inflicted' ~ "Road traffic accident",
                                  injury_cause == 'Motor vehicle nontraffic accidents;Other accidents' ~ "Road traffic accident",
                                  injury_cause == 'Motor vehicle nontraffic accidents;Other road vehicle accidents' ~ "Road traffic accident",
                                  injury_cause == 'Motor vehicle nontraffic accidents;Other road vehicle accidents;Accidental falls' ~ "Road traffic accident",
                                  injury_cause == 'Motor vehicle nontraffic accidents;Other road vehicle accidents;Vehicle accidents not elsewhere classifiable;Other accidents' ~ "Road traffic accident",
                                  injury_cause == 'Motor vehicle nontraffic accidents;Vehicle accidents not elsewhere classifiable;Accidental falls' ~ "Road traffic accident",
                                  injury_cause == 'Motor vehicle nontraffic accidents;Vehicle accidents not elsewhere classifiable;Other accidents' ~ "Road traffic accident",
                                  injury_cause == 'Motor vehicle traffic accidents' ~ "Road traffic accident",
                                  injury_cause == 'Motor vehicle traffic accidents;Accidental falls' ~ "Road traffic accident",
                                  injury_cause == 'Motor vehicle traffic accidents;Motor vehicle nontraffic accidents' ~ "Road traffic accident",
                                  injury_cause == 'Motor vehicle traffic accidents;Motor vehicle nontraffic accidents;Other road vehicle accidents' ~ "Road traffic accident",
                                  injury_cause == 'Motor vehicle traffic accidents;Motor vehicle nontraffic accidents;Other road vehicle accidents;Vehicle accidents not elsewhere classifiable' ~ "Road traffic accident",
                                  injury_cause == 'Motor vehicle traffic accidents;Motor vehicle nontraffic accidents;Other road vehicle accidents;Vehicle accidents not elsewhere classifiable;Other accidents' ~ "Road traffic accident",
                                  injury_cause == 'Motor vehicle traffic accidents;Other accidents' ~ "Road traffic accident",
                                  injury_cause == 'Motor vehicle traffic accidents;Other road vehicle accidents' ~ "Road traffic accident",
                                  injury_cause == 'Motor vehicle traffic accidents;Other road vehicle accidents;Vehicle accidents not elsewhere classifiable' ~ "Road traffic accident",
                                  injury_cause == 'Motor vehicle traffic accidents;Other road vehicle accidents;Vehicle accidents not elsewhere classifiable;Other accidents' ~ "Road traffic accident",
                                  injury_cause == 'Motor vehicle traffic accidents;Vehicle accidents not elsewhere classifiable' ~ "Road traffic accident",
                                  injury_cause == 'Other road vehicle accidents' ~ "Road traffic accident",
                                  injury_cause == 'Other road vehicle accidents;Accidental falls' ~ "Road traffic accident",
                                  injury_cause == 'Other road vehicle accidents;Accidental falls;Injury undetermined whether accidentally or purposely inflicted' ~ "Road traffic accident",
                                  injury_cause == 'Other road vehicle accidents;Accidental falls;Other accidents' ~ "Road traffic accident",
                                  injury_cause == 'Other road vehicle accidents;Other accidents' ~ "Road traffic accident",
                                  injury_cause == 'Other road vehicle accidents;Vehicle accidents not elsewhere classifiable' ~ "Road traffic accident",
                                  injury_cause == 'Other road vehicle accidents;Vehicle accidents not elsewhere classifiable;Accidental falls' ~ "Road traffic accident",
                                  injury_cause == 'Railway accidents;Motor vehicle traffic accidents' ~ "Road traffic accident",
                                  injury_cause == 'Railway accidents;Motor vehicle traffic accidents;Motor vehicle nontraffic accidents' ~ "Road traffic accident",
                                  injury_cause == 'Vehicle accidents not elsewhere classifiable' ~ "Road traffic accident",
                                  injury_cause == 'Vehicle accidents not elsewhere classifiable;Other accidents' ~ "Road traffic accident",
                                  injury_cause == 'Suicide and self-inflicted injury' ~ "Other",
                                  injury_cause == 'Accidents due to natural and environmental factors (e900-e909)' ~ "Other",
                                  injury_cause == 'Accidents due to natural and environmental factors (e900-e909);Other accidents' ~ "Other",
                                  injury_cause == 'Injury undetermined whether accidentally or purposely inflicted' ~ "Other",
                                  injury_cause == 'Water transport accidents' ~ "Other",
                                  injury_cause == 'Vehicle accidents not elsewhere classifiable;Other accidents' ~ "Road traffic accident",
                                  injury_cause == 'Vehicle accidents not elsewhere classifiable;Accidental falls' ~ "Road traffic accident",
                                  injury_cause == 'Railway accidents' ~ "Other"
  ))

# Set data type and level labels
injury$injury_cause <- factor(injury$injury_cause,
                              levels = c("Road traffic accident", "Accidental falls", "Violence/assault", "Other"))

xtabs(~ injury_cause, data =injury)

# Rename and select AIS variables
ais <- ais %>%
  rename(subjectkey = AISV1.0.Main.GUID) %>%
  rename(ais_injury_severity = AISV1.0.ISS.InjSeverScore) %>%
  select(subjectkey, ais_injury_severity)

# Make sure missing data is coded properly
ais$ais_injury_severity[ais$ais_injury_severity == 'Unknown'] <- NA

# Rename and select GCS variables
gcs <- gcs %>%
  rename(subjectkey = GCSV1.2.Main.GUID) %>%
  rename(gcs_timepoint = GCSV1.2.FormAdministration.ContextTypeOTH) %>%
  rename(gcs_score = GCSV1.2.GlasgowComaScale.GCSTotalScore) %>%
  select(subjectkey, gcs_timepoint, gcs_score)

#
# Check
#

# Remove GCS scores that are not on ED arrival
gcs <- gcs %>%
  mutate(gcs_timepoint = case_when(gcs_timepoint == 'ED Admission' ~ "ED Admission",
                                   TRUE  ~ "Not applicable")
  )

gcs <- filter(gcs, gcs_timepoint != "Not applicable")
gcs <- subset(gcs, select = -c(gcs_timepoint))


# Make sure missing data is coded properly
gcs$gcs_score[gcs$gcs_score == 'Unknown'] <- NA

# Set data type and level labels
gcs$gcs_score <- as.numeric(as.character(gcs$gcs_score))

# Create severity variable based on GCS score
gcs <- gcs %>%
  mutate(gcs_score_reversed = case_when(gcs_score == 3 ~ 15,
                                        gcs_score == 4 ~ 14,
                                        gcs_score == 5 ~ 13,
                                        gcs_score == 6 ~ 12,
                                        gcs_score == 7 ~ 11,
                                        gcs_score == 8 ~ 10,
                                        gcs_score == 9 ~ 9,
                                        gcs_score == 10 ~ 8,
                                        gcs_score == 11 ~ 7,
                                        gcs_score == 12 ~ 6,
                                        gcs_score == 13 ~ 5,
                                        gcs_score == 14 ~ 4,
                                        gcs_score == 15 ~ 3))

# Create severity variable based on GCS score
gcs <- gcs %>%
  mutate(gcs_grouped = case_when(gcs_score < 9 ~ "3-8",
                                 gcs_score >= 9 & gcs_score < 13 ~ "9-12",
                                 gcs_score >= 13 ~ "13-15"))

# Create severity variable based on GCS score
gcs <- gcs %>%
  mutate(gcs_severity = case_when(gcs_score < 9 ~ "Severe",
                                  gcs_score >= 9 & gcs_score < 13 ~ "Moderate",
                                  gcs_score >= 13 ~ "Mild"))

xtabs(~ gcs_severity, data = gcs)

# Rename and select imaging variables
imaging <- imaging %>%
  rename(subjectkey = ImagingRead_FITBIRV1.0.Main.GUID) %>%
  rename(imaging_timepoint = ImagingRead_FITBIRV1.0.Main.DaysSinceBaseline) %>%
  rename(imaging_outcome = ImagingRead_FITBIRV1.0.Findings.ImgBrainAssessmtReslt) %>%
  rename(marshall_score = ImagingRead_FITBIRV1.0.Findings.MarshallCTScore) %>%

  select(subjectkey, imaging_timepoint, imaging_outcome, marshall_score)


# Remove rows that are not taken at admission
imaging <- imaging %>%
  mutate(imaging_timepoint = case_when(imaging_timepoint < 10 ~ "Admission",
                                       imaging_timepoint >= 10 ~ "Not applicable"))

imaging <- filter(imaging, imaging_timepoint != "Not applicable")
imaging <- subset(imaging, select = -c(imaging_timepoint))

# Make sure missing data is coded properly
imaging$imaging_outcome[imaging$imaging_outcome == 'Unknown'] <- NA

xtabs( ~ marshall_score, data = imaging)


###########################################################
#
# Rename and recode mental health variables
#
###########################################################

# Rename and select BSI scores
bsi <- bsi %>%
  rename(subjectkey = BSI18V1.4.Main.GUID) %>%
  rename(bsi_timepoint = BSI18V1.4.Main.DaysSinceBaseline) %>%
  rename(somatisation_score = BSI18V1.4.FormCompletion.BSI18SomScoreRaw) %>%
  rename(bsi_depression_score = BSI18V1.4.FormCompletion.BSI18DeprScoreRaw) %>%
  rename(anxiety_score = BSI18V1.4.FormCompletion.BSI18AnxScoreRaw) %>%
  rename(bsi_global_score = BSI18V1.4.FormCompletion.BSI18GSIScoreRaw) %>%
  select(subjectkey, bsi_timepoint, somatisation_score, bsi_depression_score, anxiety_score, bsi_global_score)


# Recode 'days since baseline' to timepoint - 2 weeks, 3 months, 6 months, 12 months
bsi <- bsi %>%
  mutate(bsi_timepoint = case_when(bsi_timepoint < 24 ~ "2 weeks",
                                   bsi_timepoint >= 50 & bsi_timepoint < 122 ~ "3 months",
                                   bsi_timepoint >= 122 & bsi_timepoint < 227 ~ "6 months",
                                   bsi_timepoint >= 227 ~ "12 months")
  )


# Set data type and level labels
bsi$bsi_timepoint <- factor(bsi$bsi_timepoint,
                            levels = c("2 weeks", "3 months", "6 months", "12 months"))


# Ensure BSI variables stored as factors
bsi$bsi_global_score <- as.numeric(bsi$bsi_global_score)
bsi$somatisation_score <- as.numeric(bsi$somatisation_score)

# Change to wide format
bsi_wide <- reshape(bsi, idvar = "subjectkey", timevar = "bsi_timepoint", direction = "wide")


# Rename and select PHQ9 scores
phq <- phq %>%
  rename(subjectkey = PHQ9V1.3.Main.GUID) %>%
  rename(phq_timepoint = PHQ9V1.3.Main.DaysSinceBaseline) %>%
  rename(pleasure_score = PHQ9V1.3.PatientHealthQuestionnaire.PHQ9IntrstPleasrActScore) %>%
  rename(phq_depression_score = PHQ9V1.3.PatientHealthQuestionnaire.PHQ9DwnDeprssnHopelssScore) %>%
  rename(sleep_score = PHQ9V1.3.PatientHealthQuestionnaire.PHQ9SleepImpairScore) %>%
  rename(energy_score = PHQ9V1.3.PatientHealthQuestionnaire.PHQ9TirdLittleEnrgyScore) %>%
  rename(diet_score = PHQ9V1.3.PatientHealthQuestionnaire.PHQ9AbnrmlDietScore) %>%
  rename(failure_score = PHQ9V1.3.PatientHealthQuestionnaire.PHQ9FlngFailrScore) %>%
  rename(concentration_score = PHQ9V1.3.PatientHealthQuestionnaire.PHQ9ConcntrtnImprmntScore) %>%
  rename(movement_speech_score = PHQ9V1.3.PatientHealthQuestionnaire.PHQ9MovmntSpchImprmntScore) %>%
  rename(suicidal_thoughts_score = PHQ9V1.3.PatientHealthQuestionnaire.PHQ9BttrDdThghtScore) %>%
  rename(phq_global_score = PHQ9V1.3.PatientHealthQuestionnaire.PHQ9TotalScore) %>%
  select(subjectkey, phq_timepoint, pleasure_score, phq_depression_score, sleep_score, energy_score,
         diet_score, failure_score, concentration_score, movement_speech_score, suicidal_thoughts_score,
         phq_global_score)


# Recode 'days since baseline' to timepoint - 2 weeks, 3 months, 6 months, 12 months
phq <- phq %>%
  mutate(phq_timepoint = case_when(phq_timepoint < 24 ~ "2 weeks",
                                   phq_timepoint >= 50 & phq_timepoint < 122 ~ "3 months",
                                   phq_timepoint > 159 & phq_timepoint < 227 ~ "6 months",
                                   phq_timepoint >= 227 ~ "12 months")
  )


# Set data type and level labels
phq$phq_timepoint <- factor(phq$phq_timepoint,
                            levels = c("2 weeks", "3 months", "6 months", "12 months"))



# Create depression categories from phq global score
phq <- phq %>%
  mutate(depression_categories = case_when(phq_global_score < 5 ~ "None",
                                           phq_global_score >= 5 & phq_global_score <= 9 ~ "Mild",
                                           phq_global_score >= 10 & phq_global_score <= 14 ~ "Moderate",
                                           phq_global_score >= 15 & phq_global_score <= 19 ~ "Moderately severe",
                                           phq_global_score >= 20 & phq_global_score <= 27 ~ "Severe"))

# Ensure variable stored as factor
phq$depression_categories <- as.factor(phq$depression_categories)


# Create possible depression variable from phq global score
phq <- phq %>%
  mutate(possible_depression = case_when(phq_global_score <= 4 ~ "No",
                                         phq_global_score >= 5 ~ "Yes"))

# Ensure variable stored as factor
phq$possible_depression <- as.factor(phq$possible_depression)

# Change to wide format
phq_wide <- reshape(phq, idvar = "subjectkey", timevar = "phq_timepoint", direction = "wide")


# Rename and select PCL5 scores
pcl <- pcl %>%
  rename(subjectkey = PCL5V1.2.Main.GUID) %>%
  rename(pcl_timepoint = PCL5V1.2.Main.DaysSinceBaseline) %>%
  rename(pcl_total_score = PCL5V1.2.PCL5Scoring.PCL5SymptmSeverityScore) %>%
  select(subjectkey, pcl_timepoint, pcl_total_score)

# Recode 'days since baseline' to timepoint - 2 weeks, 3 months, 6 months, 12 months
pcl <- pcl %>%
  mutate(pcl_timepoint = case_when(pcl_timepoint < 24 ~ "2 weeks",
                                   pcl_timepoint >= 50 & pcl_timepoint < 122 ~ "3 months",
                                   pcl_timepoint > 159 & pcl_timepoint < 227 ~ "6 months",
                                   pcl_timepoint >= 227 ~ "12 months")
  )


# Set data type and level labels
pcl$pcl_timepoint <- factor(pcl$pcl_timepoint,
                            levels = c("2 weeks", "3 months", "6 months", "12 months"))



# Create possible depression variable from phq global score
pcl <- pcl %>%
  mutate(possible_ptsd = case_when(pcl_total_score <= 30 ~ "No",
                                   pcl_total_score >= 31 ~ "Yes"))

# Ensure variable stored as factor
pcl$possible_ptsd <- as.factor(pcl$possible_ptsd)

# Change to wide format
pcl_wide <- reshape(pcl, idvar = "subjectkey", timevar = "pcl_timepoint", direction = "wide")


# Rename and select interview variables
interview <- interview %>%
  rename(subjectkey = TRACKTBIInterviewModV1.0.Main.GUID)%>%
  rename(interview_timepoint = TRACKTBIInterviewModV1.0.FormAdministration.LangCRFAdministratISOCodeOTH) %>%
  rename(new_tbi = TRACKTBIInterviewModV1.0.SustainedInjury.SustOthInjTBIInd) %>%
  rename(psychiatrist = TRACKTBIInterviewModV1.0.ClinicalCare.ClincnCarePsychiatristInd) %>%
  rename(psychologist = TRACKTBIInterviewModV1.0.ClinicalCare.ClincnCarePsychologistInd) %>%
  rename(inpatient = TRACKTBIInterviewModV1.0.InpatientCare.InpatientTherPsychologicalInd) %>%
  rename(outpatient = TRACKTBIInterviewModV1.0.OutpatientCare.OutpatientTherPsychologicalInd) %>%
  rename(psyc_hospital = TRACKTBIInterviewModV1.0.Psychotherapy.HospitalEmotPsychProbInd) %>%
  rename(psychotherapy = TRACKTBIInterviewModV1.0.Psychotherapy.OutPatCounselInd) %>%
  rename(psyc_med = TRACKTBIInterviewModV1.0.Psychotherapy.TakePsychMedsInd) %>%
  rename(any_drug = TRACKTBIInterviewModV1.0.IllicitorNonPrescriptionDrugs.TrackIllicitDrugUseInd) %>%
  rename(sedative = TRACKTBIInterviewModV1.0.IllicitorNonPrescriptionDrugs.DrugSedativesInd) %>%
  rename(tranquiliser = TRACKTBIInterviewModV1.0.IllicitorNonPrescriptionDrugs.DrugTranquilizersInd) %>%
  rename(painkiller = TRACKTBIInterviewModV1.0.IllicitorNonPrescriptionDrugs.DrugPainkillersInd) %>%
  rename(stimulant = TRACKTBIInterviewModV1.0.IllicitorNonPrescriptionDrugs.DrugStimulantsInd) %>%
  rename(marijuana = TRACKTBIInterviewModV1.0.IllicitorNonPrescriptionDrugs.DrugMarijuanaInd) %>%
  rename(cocaine = TRACKTBIInterviewModV1.0.IllicitorNonPrescriptionDrugs.DrugCocaineInd) %>%
  rename(hallucinogen = TRACKTBIInterviewModV1.0.IllicitorNonPrescriptionDrugs.DrugHallucinogensInd) %>%
  rename(inhalant = TRACKTBIInterviewModV1.0.IllicitorNonPrescriptionDrugs.DrugInhalentsInds) %>%
  rename(heroin = TRACKTBIInterviewModV1.0.IllicitorNonPrescriptionDrugs.DrugHeroinInd) %>%
  rename(synthetics = TRACKTBIInterviewModV1.0.IllicitorNonPrescriptionDrugs.DrugSyntheticInd) %>%
  rename(drug_other = TRACKTBIInterviewModV1.0.IllicitorNonPrescriptionDrugs.DrugOtherInd) %>%
  rename(drug_trouble = TRACKTBIInterviewModV1.0.IllicitorNonPrescriptionDrugs.DrugTroubleInd) %>%
  select(subjectkey, interview_timepoint, new_tbi, psychiatrist, psychologist, inpatient, outpatient,
         psychotherapy, psyc_med, psyc_hospital, any_drug, sedative, tranquiliser, painkiller, stimulant,
         marijuana, cocaine, hallucinogen, inhalant, heroin, synthetics, drug_other, drug_trouble)


# Recode timepoint so that it matches other dataframes
interview <- interview %>%
  mutate(interview_timepoint = case_when(interview_timepoint == '2 Week' ~ "2 weeks",
                                         interview_timepoint == '3 Month' ~ "3 months",
                                         interview_timepoint == '6 Month' ~ "6 months",
                                         interview_timepoint == '12 Month' ~ "12 months"))

# Change all empty rows to NA

interview[interview == ""] <- NA
interview[interview == "N/A"] <- NA
interview[interview == "Unknown"] <- NA

# Recode any psychiatrist as yes (1) or no (0)
interview$psychiatrist[interview$psychiatrist == 0] <- 0
interview$psychiatrist[interview$psychiatrist == 1] <- 1

# Set data type and level labels
interview$psychiatrist <- factor(interview$psychiatrist, levels = c(1,0), labels = c("Yes", "No"))

# Recode any psychologist as yes (1) or no (0)
interview$psychologist[interview$psychologist == 0] <- 0
interview$psychologist[interview$psychologist == 1] <- 1

# Set data type and level labels
interview$psychologist <- factor(interview$psychologist, levels = c(1,0), labels = c("Yes", "No"))


# Recode inpatient services as yes (1) or no (0)
interview$inpatient[interview$inpatient == 0] <- 0
interview$inpatient[interview$inpatient == 1] <- 1

# Set data type and level labels
interview$inpatient <- factor(interview$inpatient, levels = c(1,0), labels = c("Yes", "No"))


# Recode outpatient services as yes (1) or no (0)
interview$outpatient[interview$outpatient == 0] <- 0
interview$outpatient[interview$outpatient == 1] <- 1

# Set data type and level labels
interview$outpatient <- factor(interview$outpatient, levels = c(1,0), labels = c("Yes", "No"))


# Recode psyc hospitalisation variable
interview$psyc_hospital <- as.factor(interview$psyc_hospital)


# Recode drug use variables

# Recode sedative
interview$sedative[interview$sedative == 'Yes'] <- 1
interview$sedative[interview$sedative == 'No'] <- 0
interview$sedative[interview$sedative == 'N/A (Not applicable (have not used any drugs including Marijuana))'] <- 0


interview$sedative <- as.numeric(interview$sedative)

# Recode tranquiliser
interview$tranquiliser[interview$tranquiliser == 'Yes'] <- 1
interview$tranquiliser[interview$tranquiliser == 'No'] <- 0
interview$tranquiliser[interview$tranquiliser == 'N/A (Not applicable (have not used any drugs including Marijuana))'] <- 0

interview$tranquiliser <- as.numeric(interview$tranquiliser)

# Recode stimulant
interview$stimulant[interview$stimulant == 'Yes'] <- 1
interview$stimulant[interview$stimulant == 'No'] <- 0
interview$stimulant[interview$stimulant == 'N/A (Not applicable (have not used any drugs including Marijuana))'] <- 0


interview$stimulant <- as.numeric(interview$stimulant)

# Recode painkiller
interview$painkiller[interview$painkiller == 'Yes'] <- 1
interview$painkiller[interview$painkiller == 'No'] <- 0
interview$painkiller[interview$painkiller == 'N/A (Not applicable (have not used any drugs including Marijuana))'] <- 0


interview$painkiller <- as.numeric(interview$painkiller)

# Recode marijuana
interview$marijuana[interview$marijuana == 'Yes'] <- 1
interview$marijuana[interview$marijuana == 'No'] <- 0
interview$marijuana[interview$marijuana == 'N/A (Not applicable (have not used any drugs including Marijuana))'] <- 0


interview$marijuana <- as.numeric(interview$marijuana)

# Recode cocaine
interview$cocaine[interview$cocaine == 'Yes'] <- 1
interview$cocaine[interview$cocaine == 'No'] <- 0
interview$cocaine[interview$cocaine == 'N/A (Not applicable (have not used any drugs including Marijuana))'] <- 0


interview$cocaine <- as.numeric(interview$cocaine)

# Recode hallucinogen
interview$hallucinogen[interview$hallucinogen == 'Yes'] <- 1
interview$hallucinogen[interview$hallucinogen == 'No'] <- 0
interview$hallucinogen[interview$hallucinogen == 'N/A (Not applicable (have not used any drugs including Marijuana))'] <- 0


interview$hallucinogen <- as.numeric(interview$hallucinogen)

# Recode inhalant
interview$inhalant[interview$inhalant == 'Yes'] <- 1
interview$inhalant[interview$inhalant == 'No'] <- 0
interview$inhalant[interview$inhalant == 'N/A (Not applicable (have not used any drugs including Marijuana))'] <- 0

interview$inhalant <- as.numeric(interview$inhalant)

# Recode heroin
interview$heroin[interview$heroin == 'Yes'] <- 1
interview$heroin[interview$heroin == 'No'] <- 0
interview$heroin[interview$heroin == 'N/A (Not applicable (have not used any drugs including Marijuana))'] <- 0


interview$heroin <- as.numeric(interview$heroin)


# Recode synthetics
interview$synthetics[interview$synthetics == 'Yes'] <- 1
interview$synthetics[interview$synthetics == 'No'] <- 0
interview$synthetics[interview$synthetics == 'N/A (Not applicable (have not used any drugs including Marijuana))'] <- 0

interview$synthetics <- as.numeric(interview$synthetics)

# Recode other drug
interview$drug_other[interview$drug_other == 'Yes'] <- 1
interview$drug_other[interview$drug_other == 'No'] <- 0
interview$drug_other[interview$drug_other == 'N/A (Not applicable (have not used any drugs including Marijuana))'] <- 0


interview$drug_other <- as.numeric(interview$drug_other)

# Recode other drug
interview$drug_trouble[interview$drug_trouble == 'Yes'] <- 1
interview$drug_trouble[interview$drug_trouble == 'No'] <- 0
interview$drug_trouble[interview$drug_trouble == 'N/A (Not applicable (have not used any drugs including Marijuana))'] <- 0

interview$drug_trouble <- as.numeric(interview$drug_trouble)


# Change to wide format
interview_wide <- reshape(interview, idvar = "subjectkey", timevar = "interview_timepoint", direction = "wide")



##########################################################
#
# Merge datasets
#
###########################################################

# Merge datasets
data <- demographics %>%
  left_join(med_history, by = "subjectkey") %>%
  left_join(ohio, by = "subjectkey") %>%
  left_join(gcs, by = "subjectkey") %>%
  left_join(injury, by = "subjectkey") %>%
  left_join(ais, by = "subjectkey") %>%
  left_join(imaging, by = "subjectkey") %>%
  left_join(bsi_wide, by = "subjectkey") %>%
  left_join(phq_wide, by = "subjectkey") %>%
  left_join(pcl_wide, by = "subjectkey") %>%
  left_join(interview_wide, by = "subjectkey") %>%
  left_join(audit, by = "subjectkey")


##############################################################################
#
# Rename variables for better readability
# Create tables for demographic and descriptive statistics of sample
#
#############################################################################

# Remove NA columns
data <- data %>%
  select(-matches("NA$"))


# Remove spaces in column names
colnames(data) <- gsub(" ", "_", colnames(data))

# Remove full stops in column names
colnames(data) <- gsub("\\.", "_", colnames(data))


# Make sure all empty rows are labelled NA
data[data == ""] <- NA
data[data == "Unknown"] <- NA

# Remove unnecessary columns (variables aren't measured at these timepoints)
data <- subset(data, select = -c(psychotherapy_2_weeks, psyc_med_2_weeks,
                                 psychotherapy_3_months, psyc_med_3_months,
                                 psyc_med_6_months, psychotherapy_6_months, outpatient_6_months, inpatient_6_months
))

# Change age to numeric
data$age <- as.numeric(data$age)


# Create psychologist total variable
data <- data %>%
  mutate(psychologist_total = ifelse(psychologist_2_weeks == 'Yes' | psychologist_3_months == 'Yes'
                                     | psychologist_6_months == 'Yes'  | psychologist_12_months == 'Yes',
                                     'Yes', 'No'))



# Create psychiatrist total variable
data <- data %>%
  mutate(psychiatrist_total = ifelse(psychiatrist_2_weeks == 'Yes' | psychiatrist_3_months == 'Yes'
                                     | psychiatrist_6_months == 'Yes'  | psychiatrist_12_months == 'Yes',
                                     'Yes', 'No'))


# Create inpatient total variable
data <- data %>%
  mutate(inpatient_total = ifelse(inpatient_12_months == 'Yes',
                                  'Yes', 'No'))


# Create outpatient total variable
data <- data %>%
  mutate(outpatient_total = ifelse(outpatient_12_months == 'Yes',
                                   'Yes', 'No'))


# Create psyc hospital total variable
data <- data %>%
  mutate(psyc_hospital_total = ifelse(psyc_hospital_12_months == 'Yes',
                                      'Yes', 'No'))


# Create any mental health total variable
data <- data %>%
  mutate(any_mh_service = ifelse(psychologist_total == 'Yes' | psychiatrist_total == 'Yes' | psychotherapy_12_months == 'Yes' |
                                   psyc_hospital_total == 'Yes' | psyc_med_12_months == 'Yes',
                                 'Yes', 'No'))

# Create total drug score variable by summing all drug use
data$drug_total_score <- rowSums(data[,c("sedative_2_weeks", "tranquiliser_2_weeks", "painkiller_2_weeks", "stimulant_2_weeks",
                                         "marijuana_2_weeks", "cocaine_2_weeks", "hallucinogen_2_weeks", "inhalant_2_weeks",
                                         "heroin_2_weeks", "synthetics_2_weeks", "drug_other_2_weeks")])


# Check variables are coded correctly

xtabs( ~ psyc_med_12_months, data = data)
xtabs( ~ drug_total_score, data = data)
xtabs( ~ any_mh_service, data = data)
xtabs( ~ outpatient_total, data = data)
xtabs( ~ outpatient_2_weeks, data = data)
xtabs( ~ psychologist_total, data = data)
xtabs( ~ psychiatrist_total, data = data)
xtabs( ~ psyc_hospital_total, data = data)
xtabs( ~ outpatient_3_months, data = data)
xtabs( ~ outpatient_12_months, data = data)
xtabs( ~ drug_total_score, data = data)
typeof(data$psyc_hospital_total)
typeof(data$psychologist_total)
typeof(data$inpatient_total)
typeof(data$outpatient_total)
typeof(data$outpatient_total)


# Rename variables for better readability
label(data$age) <- "Age"
label(data$sex) <- "Sex"
label(data$race) <- "Race and ethnicity"
label(data$injury_type) <- "Injury type"
label(data$family_income) <- "Household Income"
label(data$admission) <- "Admission"
label(data$highest_education) <- "Highest Education"
label(data$previous_loc_tbi) <- "Number of previous TBI with LOC"
label(data$worst_tbi) <- "Previous TBI"
label(data$anxiety_history) <- "Anxiety diagnosis"
label(data$depression_history) <- "Depression diagnosis"
label(data$bipolar_history) <- "Bipolar diagnosis"
label(data$schizophrenia_history) <- "Schizophrenia diagnosis"
label(data$ptsd_history) <- "PTSD diagnosis"
label(data$adhd_history) <- "ADHD diagnosis"
label(data$any_mh_disorder) <- "Number of previous mental health diagnoses"
label(data$psyc_hospital_history) <- "Psychiatric hospitalisation"
label(data$psyc_med_history) <- "Psychiatric medication"
label(data$gcs_score) <- "GCS score"
label(data$gcs_severity) <- "GCS severity"
label(data$injury_cause) <- "Injury cause"
label(data$injury_mechanism) <- "Injury mechanism"
label(data$loc_duration) <- "LOC duration"
label(data$aoc_duration) <- "AOC duration"
label(data$pta_duration) <- "PTA duration"
label(data$admission_type) <- "Admission type"
label(data$ais_injury_severity) <- "AIS injury severity"
label(data$imaging_outcome) <- "Imaging outcome"
label(data$loc_indicator) <- "Loss of consciousness"
label(data$pta_indicator) <- "Post-traumatic amnesia"
label(data$aoc_indicator) <- "Alteration of consciousness"
label(data$marshall_score) <- "Marshall score"
label(data$psychologist_total) <- "Psychologist"
label(data$psychiatrist_total) <- "Psychiatrist"
label(data$any_mh_service) <- "Any mental health service use"
label(data$gcs_score_reversed) <- "TBI severity (GCS)"
label(data$previous_tbi) <- "Previous TBI"
label(data$mh_disorder) <- "Previous mental health disorders"




##############################################################################
#
# Multiple imputation using mice package
#
# DATA FLOW AND NAMING CONVENTION:
#   1. 'data' = Full dataset with all variables (before imputation)
#   2. 'data_for_imputation' = Selected variables BEFORE imputation (has missing values)
#   3. 'imputed_result' = MICE object containing multiple imputed datasets
#   4. 'imputed_data' = First completed dataset AFTER imputation (no missing values)
#   5. 'imputed_wide_data' = Final dataset with imputed predictors + outcomes
#
#############################################################################

# Set seed for reproducibility
set.seed(777)

##############################################################################
# STEP 1: Ensure variables stored correctly BEFORE selecting for imputation
##############################################################################

# Convert all categorical variables to factors
data$site <- as.factor(data$site)
data$sex <- as.factor(data$sex)
data$race <- as.factor(data$race)
data$family_income <- as.factor(data$family_income)
data$injury_type <- as.factor(data$injury_type)
data$highest_education <- as.factor(data$highest_education)
data$admission <- as.factor(data$admission)

# TBI severity and injury characteristics
data$gcs_score <- as.factor(data$gcs_score)
data$gcs_severity <- as.factor(data$gcs_severity)
data$injury_cause <- as.factor(data$injury_cause)
data$injury_mechanism <- as.factor(data$injury_mechanism)
data$loc_duration <- as.factor(data$loc_duration)
data$pta_duration <- as.factor(data$pta_duration)
data$aoc_duration <- as.factor(data$aoc_duration)
data$loc_indicator <- as.factor(data$loc_indicator)
data$pta_indicator <- as.factor(data$pta_indicator)
data$aoc_indicator <- as.factor(data$aoc_indicator)
data$admission_type <- as.factor(data$admission_type)
data$imaging_outcome <- as.factor(data$imaging_outcome)

# Pre-injury history
data$anxiety_history <- as.factor(data$anxiety_history)
data$depression_history <- as.factor(data$depression_history)
data$bipolar_history <- as.factor(data$bipolar_history)
data$schizophrenia_history <- as.factor(data$schizophrenia_history)
data$ptsd_history <- as.factor(data$ptsd_history)
data$adhd_history <- as.factor(data$adhd_history)
data$psyc_hospital_history <- as.factor(data$psyc_hospital_history)
data$psyc_med_history <- as.factor(data$psyc_med_history)
data$worst_tbi <- as.factor(data$worst_tbi)
data$previous_tbi <- as.factor(data$previous_tbi)

# Post-injury mental health service use - NOT included in imputation
# These will be kept from original data
# data$psychologist_2_weeks <- as.factor(data$psychologist_2_weeks)
# data$psychologist_3_months <- as.factor(data$psychologist_3_months)
# data$psychologist_6_months <- as.factor(data$psychologist_6_months)
# data$psychologist_12_months <- as.factor(data$psychologist_12_months)
# data$psychiatrist_2_weeks <- as.factor(data$psychiatrist_2_weeks)
# data$psychiatrist_3_months <- as.factor(data$psychiatrist_3_months)
# data$psychiatrist_6_months <- as.factor(data$psychiatrist_6_months)
# data$psychiatrist_12_months <- as.factor(data$psychiatrist_12_months)
# data$inpatient_12_months <- as.factor(data$inpatient_12_months)
# data$outpatient_12_months <- as.factor(data$outpatient_12_months)
# data$psychotherapy_12_months <- as.factor(data$psychotherapy_12_months)
# data$psyc_med_12_months <- as.factor(data$psyc_med_12_months)
# data$psyc_hospital_12_months <- as.factor(data$psyc_hospital_12_months)

# Numeric variables - ensure proper type
data$age <- as.numeric(data$age)
data$gcs_score_reversed <- as.numeric(data$gcs_score_reversed)
data$ais_injury_severity <- as.numeric(data$ais_injury_severity)
data$marshall_score <- as.numeric(data$marshall_score)
data$audit_total_score <- as.numeric(data$audit_total_score)
data$any_mh_disorder <- as.numeric(data$any_mh_disorder)

# Individual drug use variables (2 weeks timepoint) - NOT included in imputation
# These will be kept from original data
# data$sedative_2_weeks <- as.numeric(data$sedative_2_weeks)
# data$tranquiliser_2_weeks <- as.numeric(data$tranquiliser_2_weeks)
# data$painkiller_2_weeks <- as.numeric(data$painkiller_2_weeks)
# data$stimulant_2_weeks <- as.numeric(data$stimulant_2_weeks)
# data$marijuana_2_weeks <- as.numeric(data$marijuana_2_weeks)
# data$cocaine_2_weeks <- as.numeric(data$cocaine_2_weeks)
# data$hallucinogen_2_weeks <- as.numeric(data$hallucinogen_2_weeks)
# data$inhalant_2_weeks <- as.numeric(data$inhalant_2_weeks)
# data$heroin_2_weeks <- as.numeric(data$heroin_2_weeks)
# data$synthetics_2_weeks <- as.numeric(data$synthetics_2_weeks)
# data$drug_other_2_weeks <- as.numeric(data$drug_other_2_weeks)

# Mental health scores at earlier timepoints (2 weeks, 3 months, 6 months)
# These can be used as predictors but NOT 12-month outcomes
data$bsi_global_score_2_weeks <- as.numeric(data$bsi_global_score_2_weeks)
data$bsi_global_score_3_months <- as.numeric(data$bsi_global_score_3_months)
data$bsi_global_score_6_months <- as.numeric(data$bsi_global_score_6_months)
data$phq_global_score_2_weeks <- as.numeric(data$phq_global_score_2_weeks)
data$phq_global_score_3_months <- as.numeric(data$phq_global_score_3_months)
data$phq_global_score_6_months <- as.numeric(data$phq_global_score_6_months)
data$pcl_total_score_2_weeks <- as.numeric(data$pcl_total_score_2_weeks)
data$pcl_total_score_3_months <- as.numeric(data$pcl_total_score_3_months)
data$pcl_total_score_6_months <- as.numeric(data$pcl_total_score_6_months)


##############################################################################
# STEP 2: Select ONLY base variables for imputation
# DO NOT include:
#   - Derived variables (totals, composites)
#   - Duplicate variables
#   - Acute severity measures (primary predictors - use complete case analysis)
##############################################################################

data_for_imputation <- select(data,
                       # Subject identifier (will be removed before imputation)
                       subjectkey,

                       # Demographics and baseline characteristics
                       site,
                       age,
                       sex,
                       race,
                       family_income,
                       injury_type,

                       # Pre-injury mental health
                       anxiety_history,
                       depression_history,
                       bipolar_history,
                       schizophrenia_history,
                       ptsd_history,
                       adhd_history,
                       psyc_hospital_history,
                       psyc_med_history,

                       # Pre-injury substance use
                       audit_total_score,

                       # Mental health scores at all timepoints (including 12 months)
                       # 12-month PHQ and BSI will be imputed ONLY when early data exists
                       bsi_global_score_2_weeks,
                       bsi_global_score_3_months,
                       bsi_global_score_6_months,
                       bsi_global_score_12_months,
                       phq_global_score_2_weeks,
                       phq_global_score_3_months,
                       phq_global_score_6_months,
                       phq_global_score_12_months,
                       pcl_total_score_2_weeks,
                       pcl_total_score_3_months,
                       pcl_total_score_6_months,
                       pcl_total_score_12_months

                       # NOT IMPUTED (kept from original data for complete case analysis):
                       # - Previous TBI: previous_tbi, worst_tbi
                       # - Acute TBI severity: gcs_score, loc/pta/aoc duration/indicators,
                       #   ais_injury_severity, imaging_outcome, marshall_score
                       # - Injury characteristics: injury_cause, injury_mechanism, admission, admission_type
)

##############################################################################
# STEP 3: Store outcome variables separately (NOT for imputation)
# NOTE: phq_global_score_12_months, bsi_global_score_12_months is now included in imputation
#       (with post-imputation filtering)
##############################################################################

outcomes_12_months <- select(data,
                             subjectkey,
                             # Include any other 12-month outcomes not being imputed
                             somatisation_score_12_months,
                             bsi_depression_score_12_months,
                             anxiety_score_12_months
)

##############################################################################
# STEP 4: Visualize missing data and test for missing completely at random
##############################################################################

# Calculate missingness for each variable
miss_summary <- data_for_imputation %>%
  summarise(across(everything(), ~sum(is.na(.))/n()*100)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "percent_missing") %>%
  arrange(desc(percent_missing))

print(head(miss_summary, 20))

# Visualize missing data pattern
options(repr.plot.width = 10, repr.plot.height = 10, repr.plot.res = 200)
vis_miss(data_for_imputation, sort_miss = TRUE, warn_large_data = FALSE)
ggsave(paste(output_dir, "missing_data_before_imputation.png", sep = ""))

# Test for Missing Completely At Random (MCAR) using Little's test
cat("\n=== Testing for MCAR (Missing Completely At Random) ===\n")
cat("Little's MCAR test:\n")
cat("  - Null hypothesis: Data is MCAR\n")
cat("  - If p < 0.05: Reject MCAR (data is likely MAR or MNAR)\n")
cat("  - If p >= 0.05: Fail to reject MCAR (data may be MCAR)\n\n")

mcar_result <- mcar_test(data_for_imputation)
print(mcar_result)

# Save MCAR test result to file
sink(paste(output_dir, "mcar_test_result.txt", sep = ""))
cat("Little's MCAR test for missing data pattern\n\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
print(mcar_result)
cat("\n\nInterpretation:\n")
cat("  - Null hypothesis: Data is Missing Completely At Random (MCAR)\n")
cat("  - Alternative hypothesis: Data is NOT MCAR (may be MAR or MNAR)\n")
if(mcar_result$p.value < 0.05) {
  cat("\n  Result: p < 0.05 - REJECT null hypothesis\n")
  cat("  The missingness pattern is likely NOT completely random.\n")
  cat("  Missing data may depend on observed or unobserved values (MAR or MNAR).\n")
  cat("  Multiple imputation is appropriate for handling this missing data.\n")
} else {
  cat("\n  Result: p >= 0.05 - FAIL TO REJECT null hypothesis\n")
  cat("  The missingness pattern appears to be random (MCAR).\n")
  cat("  Both complete case analysis and multiple imputation are reasonable.\n")
}
sink()

cat("\nMCAR test result saved to:", paste(output_dir, "mcar_test_result.txt", sep = ""), "\n")




##############################################################################
# STEP 4.5: Test whether acute severity measures predict missingness
#            in 12-month mental health outcomes
#
# This analysis tests whether data is Missing Not At Random (MNAR)
# by examining if acute TBI severity predicts missingness in outcomes
##############################################################################

cat("\n=== Testing Association Between Acute Severity and Missingness ===\n\n")

# Create binary missingness indicators
cat("Creating missingness indicators for 12-month outcomes...\n")

data$missing_phq_12m <- ifelse(is.na(data$phq_global_score_12_months), 1, 0)
data$missing_bsi_12m <- ifelse(is.na(data$bsi_global_score_12_months), 1, 0)

# Summary of missingness
cat("\n12-Month Outcome Missingness Summary:\n")
cat("  PHQ-9:  ", sum(data$missing_phq_12m), " missing (",
    round(mean(data$missing_phq_12m, na.rm=TRUE)*100, 1), "%)\n", sep="")
cat("  BSI-18: ", sum(data$missing_bsi_12m), " missing (",
    round(mean(data$missing_bsi_12m, na.rm=TRUE)*100, 1), "%)\n", sep="")

# Descriptive comparison - Missingness by severity groups
cat("Comparing missingness rates across severity groups...\n\n")

# Function to compare missingness across groups
compare_missingness <- function(data, outcome_var, severity_var, severity_name) {
  cross_tab <- table(data[[outcome_var]], data[[severity_var]], useNA = "no")
  pct_missing <- prop.table(cross_tab, margin = 2)[2, ] * 100
  
  test_result <- tryCatch({
    if(min(cross_tab) < 5) {
      fisher.test(cross_tab)
    } else {
      chisq.test(cross_tab)
    }
  }, error = function(e) NULL)
  
  list(severity_name = severity_name, outcome_name = outcome_var,
       percentages = pct_missing, test = test_result)
}

# Test severity measures
severity_vars <- list(gcs_severity = "GCS Severity",
                      imaging_outcome = "CT Imaging Outcome",
                      admission = "Admission Type",
                      injury_type = "Injury Type",
                      loc_duration = "LOC Duration",
                      pta_duration = "PTA Duration",
                      aoc_duration = "AOC Duration")

descriptive_results <- list()

for(severity_var in names(severity_vars)) {
  if(severity_var %in% names(data)) {
    cat("\n--- ", severity_vars[[severity_var]], " ---\n", sep="")
    
    for(outcome_var in c("missing_phq_12m", "missing_bsi_12m")) {
      result <- compare_missingness(data, outcome_var, severity_var, severity_vars[[severity_var]])
      descriptive_results[[paste(severity_var, outcome_var, sep="_")]] <- result
      
      outcome_label <- switch(outcome_var, missing_phq_12m = "PHQ-9",
                              missing_bsi_12m = "BSI-18")
      
      cat("\n", outcome_label, " missingness by ", severity_vars[[severity_var]], ":\n", sep="")
      print(round(result$percentages, 1))
      
      if(!is.null(result$test)) {
        cat("  Test p-value: ", format.pval(result$test$p.value, digits=3), "\n", sep="")
      }
    }
  }
}

# Univariate logistic regression models
cat("\n\n=== Univariate Logistic Regression Models ===\n")
cat("Testing each severity measure separately\n\n")

# Define severity predictors
severity_predictors <- list(
  gcs_severity = "GCS Severity (vs Mild)",
  imaging_outcome = "CT Abnormal (vs Normal)",
  admission = "Admission Type (vs ED discharge)",
  injury_type = "Injury Type (vs Ortho)",
  loc_duration = "LOC Duration",
  pta_duration = "PTA Duration",
  aoc_duration = "AOC Duration",
  gcs_score_reversed = "GCS Score (reversed, continuous)",
  ais_injury_severity = "AIS Injury Severity Score",
  marshall_score = "Marshall CT Score"
)

# Function to run univariate logistic regression
run_univariate_model <- function(data, outcome, predictor, predictor_name) {
  if(!predictor %in% names(data) || sum(!is.na(data[[predictor]])) < 10) {
    return(NULL)
  }
  
  formula_str <- paste(outcome, "~", predictor)
  model <- tryCatch({
    glm(as.formula(formula_str), family = binomial, data = data)
  }, error = function(e) NULL)
  
  if(is.null(model)) return(NULL)
  
  tidy_result <- broom::tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(term != "(Intercept)") %>%
    mutate(predictor_name = predictor_name, outcome = outcome, predictor_var = predictor)
  
  return(tidy_result)
}

# Run all univariate models
univariate_results <- list()

for(outcome in c("missing_phq_12m", "missing_bsi_12m")) {
  outcome_label <- switch(outcome, missing_phq_12m = "PHQ-9",
                          missing_bsi_12m = "BSI-18")
  
  cat("\n--- ", outcome_label, " 12-month Missingness ---\n", sep="")
  
  for(predictor in names(severity_predictors)) {
    result <- run_univariate_model(data, outcome, predictor, severity_predictors[[predictor]])
    
    if(!is.null(result)) {
      univariate_results[[paste(outcome, predictor, sep="_")]] <- result
      
      cat("\n  ", severity_predictors[[predictor]], ":\n", sep="")
      for(i in 1:nrow(result)) {
        cat("    ", result$term[i], ": OR = ", round(result$estimate[i], 2),
            " (95% CI: ", round(result$conf.low[i], 2), "-", round(result$conf.high[i], 2),
            "), p = ", format.pval(result$p.value[i], digits=3), "\n", sep="")
      }
    }
  }
}

all_univariate_results <- bind_rows(univariate_results)

# Multivariate logistic regression models
cat("\n\n=== Multivariate Logistic Regression Models ===\n")
cat("Testing multiple severity measures together, controlling for confounders\n\n")

multivariate_results <- list()

for(outcome in c("missing_phq_12m", "missing_bsi_12m")) {
  outcome_label <- switch(outcome, missing_phq_12m = "PHQ-9",
                          missing_bsi_12m = "BSI-18")
  
  cat("\n========== ", outcome_label, " 12-month Missingness ==========\n", sep="")
  
  # Full model with multiple severity measures + confounders
  model_formula <- paste(outcome, "~ gcs_severity + imaging_outcome + admission +",
                         "age + sex + race + family_income + site")
  
  model <- tryCatch({
    glm(as.formula(model_formula), family = binomial, data = data)
  }, error = function(e) NULL)
  
  if(!is.null(model)) {
    cat("  N = ", nobs(model), ", AIC = ", round(AIC(model), 1), "\n", sep="")
    
    tidy_result <- broom::tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
      filter(term != "(Intercept)") %>%
      mutate(model_name = paste(outcome_label, "Full Model"), outcome = outcome)
    
    multivariate_results[[outcome]] <- tidy_result
    
    # Print severity variable results
    severity_terms <- tidy_result %>%
      filter(grepl("gcs|imaging|admission|injury_type|loc_duration|pta_duration|aoc_duration", term))
    
    if(nrow(severity_terms) > 0) {
      cat("\n  Severity measure results:\n")
      for(i in 1:nrow(severity_terms)) {
        sig_marker <- ifelse(severity_terms$p.value[i] < 0.05, "*", " ")
        cat("    ", severity_terms$term[i], ": OR = ", round(severity_terms$estimate[i], 2),
            " (95% CI: ", round(severity_terms$conf.low[i], 2), "-",
            round(severity_terms$conf.high[i], 2),
            "), p = ", format.pval(severity_terms$p.value[i], digits=3), sig_marker, "\n", sep="")
      }
    }
  }
}

all_multivariate_results <- bind_rows(multivariate_results)

# Create summary tables
cat("\n=== Creating Summary Tables ===\n")

for(outcome in c("missing_phq_12m", "missing_bsi_12m")) {
  outcome_label <- switch(outcome, missing_phq_12m = "PHQ-9",
                          missing_bsi_12m = "BSI-18")
  
  formula_str <- paste(outcome, "~ gcs_severity + imaging_outcome + admission + injury_type +",
                       "age + sex + race + family_income")
  
  model <- tryCatch({
    glm(as.formula(formula_str), family = binomial, data = data)
  }, error = function(e) NULL)
  
  if(!is.null(model)) {
    tbl <- tbl_regression(model, exponentiate = TRUE,
                          label = list(gcs_severity ~ "GCS Severity",
                                       imaging_outcome ~ "CT Imaging",
                                       admission ~ "Admission Type",
                                       injury_type ~ "Injury Type",
                                       age ~ "Age", sex ~ "Sex",
                                       race ~ "Race", family_income ~ "Family Income")) %>%
      bold_p(t = 0.05) %>%
      bold_labels() %>%
      modify_header(estimate ~ "**OR (95% CI)**") %>%
      modify_caption(paste0("**Predictors of ", outcome_label, " 12-month Missingness**"))
    
    tbl %>%
      as_gt() %>%
      gtsave(filename = paste0(output_dir, "missingness_",
                               gsub("missing_|_12m", "", outcome), "_table.html"))
  }
}

cat("  Saved HTML tables to output directory\n")

# Create forest plots
cat("\n=== Creating Forest Plots ===\n")

for(outcome in c("missing_phq_12m", "missing_bsi_12m")) {
  outcome_label <- switch(outcome, missing_phq_12m = "PHQ-9",
                          missing_bsi_12m = "BSI-18")
  
  plot_data <- all_multivariate_results %>%
    filter(outcome == !!outcome) %>%
    filter(grepl("gcs|imaging|admission|injury_type|loc_duration|pta_duration|aoc_duration", term)) %>%
    mutate(term_clean = case_when(
      term == "gcs_severityModerate" ~ "Moderate TBI (vs Mild)",
      term == "gcs_severitySevere" ~ "Severe TBI (vs Mild)",
      term == "imaging_outcomeAbnormal" ~ "Abnormal CT (vs Normal)",
      grepl("admission.*ICU", term) ~ "ICU Admission (vs ED)",
      grepl("admission.*no ICU", term) ~ "Hospital Admission (vs ED)",
      term == "injury_typeTBI" ~ "TBI (vs Ortho)",
      grepl("loc_duration", term) ~ gsub("loc_duration", "LOC: ", term),
      grepl("pta_duration", term) ~ gsub("pta_duration", "PTA: ", term),
      grepl("aoc_duration", term) ~ gsub("aoc_duration", "AOC: ", term),
      TRUE ~ term
    ), significant = p.value < 0.05)
  
  if(nrow(plot_data) > 0) {
    forest_plot <- ggplot(plot_data, aes(x = estimate, y = term_clean, color = significant)) +
      geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
      geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, linewidth = 0.8) +
      geom_point(size = 3) +
      scale_color_manual(values = c("FALSE" = "gray60", "TRUE" = "red"),
                         labels = c("Not significant", "p < 0.05"), name = "") +
      scale_x_log10(breaks = c(0.25, 0.5, 1, 2, 4)) +
      labs(title = paste("Predictors of", outcome_label, "12-Month Missingness"),
           subtitle = "Odds ratios from multivariate logistic regression",
           x = "Odds Ratio (log scale)", y = "") +
      theme_minimal(base_size = 11) +
      theme(legend.position = "bottom", panel.grid.minor = element_blank(),
            plot.title = element_text(face = "bold"))
    
    ggsave(paste0(output_dir, "missingness_forest_",
                  gsub("missing_|_12m", "", outcome), ".png"),
           forest_plot, width = 10, height = 6, dpi = 300)
    
    ggsave(paste0(output_dir, "missingness_forest_",
                  gsub("missing_|_12m", "", outcome), ".pdf"),
           forest_plot, width = 10, height = 6)
  }
}

cat("  Saved forest plots to output directory\n")

# Generate comprehensive report
cat("\n=== Generating Comprehensive Report ===\n")

sink(paste0(output_dir, "missingness_severity_analysis_report.txt"))

cat("MISSINGNESS AND SEVERITY ANALYSIS REPORT\n")
cat("=========================================\n\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

cat("RESEARCH QUESTION:\n")
cat("Does acute TBI severity predict missingness in 12-month mental health outcomes?\n\n")

cat("SEVERITY PREDICTORS TESTED:\n")
cat("---------------------------\n")
cat("  - GCS severity (mild/moderate/severe)\n")
cat("  - CT imaging outcome (normal/abnormal)\n")
cat("  - Admission type (ED discharge/hospital/ICU)\n")
cat("  - Injury type (TBI/Ortho)\n")
cat("  - LOC duration (categorical)\n")
cat("  - PTA duration (categorical)\n")
cat("  - AOC duration (categorical)\n")
cat("  - GCS score (continuous, reversed)\n")
cat("  - AIS injury severity score\n")
cat("  - Marshall CT score\n\n")

cat("MISSINGNESS SUMMARY\n")
cat("-------------------\n")
cat("PHQ-9 12-month:  ", sum(data$missing_phq_12m, na.rm=TRUE), " missing (",
    round(mean(data$missing_phq_12m, na.rm=TRUE)*100, 1), "%)\n", sep="")
cat("BSI-18 12-month: ", sum(data$missing_bsi_12m, na.rm=TRUE), " missing (",
    round(mean(data$missing_bsi_12m, na.rm=TRUE)*100, 1), "%)\n", sep="")

##############################################################################
# CHANGED BLOCK #1: UNIVARIATE REPORT NOW PRINTS ALL (SIG + NON-SIG)
##############################################################################
cat("KEY FINDINGS - UNIVARIATE MODELS\n")
cat("---------------------------------\n\n")

all_univ_report <- all_univariate_results %>%
  arrange(outcome, p.value) %>%
  mutate(sig_marker = ifelse(p.value < 0.05, "*", ""))

if(nrow(all_univ_report) > 0) {
  cat("All univariate associations (sorted by outcome, then p-value). '*' = p < 0.05\n\n")
  
  for(outcome in c("missing_phq_12m", "missing_bsi_12m")) {
    outcome_label <- switch(outcome,
                            missing_phq_12m = "PHQ-9",
                            missing_bsi_12m = "BSI-18")
    
    cat("\n", outcome_label, " Missingness:\n", sep="")
    out_rows <- all_univ_report %>% filter(outcome == !!outcome)
    
    if(nrow(out_rows) == 0) {
      cat("  No models estimated.\n")
      next
    }
    
    for(i in 1:nrow(out_rows)) {
      cat("  ", out_rows$predictor_name[i], " - ", out_rows$term[i], "\n", sep="")
      cat("    OR = ", round(out_rows$estimate[i], 2),
          " (95% CI: ", round(out_rows$conf.low[i], 2), "-",
          round(out_rows$conf.high[i], 2), "), p = ",
          format.pval(out_rows$p.value[i], digits=3),
          " ", out_rows$sig_marker[i], "\n\n", sep="")
    }
  }
} else {
  cat("No univariate models were estimated (all_univariate_results is empty).\n\n")
}

##############################################################################
# CHANGED BLOCK #2: MULTIVARIATE REPORT NOW PRINTS ALL (SIG + NON-SIG)
##############################################################################
cat("KEY FINDINGS - MULTIVARIATE MODELS\n")
cat("-----------------------------------\n\n")

all_multi_report <- all_multivariate_results %>%
  filter(grepl("gcs|imaging|admission|injury_type|loc_duration|pta_duration|aoc_duration", term)) %>%
  arrange(outcome, p.value) %>%
  mutate(sig_marker = ifelse(p.value < 0.05, "*", ""))

if(nrow(all_multi_report) > 0) {
  cat("All multivariate severity-term associations (sorted by outcome, then p-value). '*' = p < 0.05\n\n")
  
  for(outcome in c("missing_phq_12m", "missing_bsi_12m")) {
    outcome_label <- switch(outcome,
                            missing_phq_12m = "PHQ-9",
                            missing_bsi_12m = "BSI-18")
    
    cat("\n", outcome_label, " Missingness:\n", sep="")
    out_rows <- all_multi_report %>% filter(outcome == !!outcome)
    
    if(nrow(out_rows) == 0) {
      cat("  No multivariate severity terms available.\n")
      next
    }
    
    for(i in 1:nrow(out_rows)) {
      cat("  ", out_rows$term[i], ": OR = ", round(out_rows$estimate[i], 2),
          " (95% CI: ", round(out_rows$conf.low[i], 2), "-",
          round(out_rows$conf.high[i], 2), "), p = ",
          format.pval(out_rows$p.value[i], digits=3),
          " ", out_rows$sig_marker[i], "\n", sep="")
    }
    cat("\n")
  }
} else {
  cat("No multivariate severity-term results available.\n\n")
}

cat("\nINTERPRETATION AND IMPLICATIONS\n")
cat("--------------------------------\n")

n_sig_findings <- nrow(all_multivariate_results %>% filter(p.value < 0.05))

if(n_sig_findings == 0) {
  cat("\nRESULT: No significant associations found.\n")
  cat("DATA: Likely Missing At Random (MAR) with respect to severity\n")
  cat("IMPLICATIONS: Multiple imputation assumptions are supported\n")
} else {
  cat("\nRESULT: Evidence for severity-related missingness found.\n")
  cat("DATA: May be Missing Not At Random (MNAR)\n")
  cat("IMPLICATIONS: Consider sensitivity analyses\n")
}

sink()

cat("Report saved to:", paste0(output_dir, "missingness_severity_analysis_report.txt"), "\n")

# Save results as R objects
missingness_analysis_results <- list(
  univariate_results = all_univariate_results,
  multivariate_results = all_multivariate_results,
  descriptive_comparisons = descriptive_results
)

saveRDS(missingness_analysis_results,
        paste0(output_dir, "missingness_severity_analysis_results.rds"))

cat("Results saved to:", paste0(output_dir, "missingness_severity_analysis_results.rds"), "\n")
cat("\n=== MISSINGNESS-SEVERITY ANALYSIS COMPLETE ===\n\n")



##############################################################################
# STEP 4.5: Test whether acute severity measures predict missingness
#            in 12-month mental health outcomes
#
# This analysis tests whether data is Missing Not At Random (MNAR)
# by examining if acute TBI severity predicts missingness in outcomes
##############################################################################

cat("\n=== Testing Association Between Acute Severity and Missingness ===\n\n")

# Create binary missingness indicators
cat("Creating missingness indicators for 12-month outcomes...\n")

data$missing_phq_12m <- ifelse(is.na(data$phq_global_score_12_months), 1, 0)
data$missing_bsi_12m <- ifelse(is.na(data$bsi_global_score_12_months), 1, 0)

# Summary of missingness
cat("\n12-Month Outcome Missingness Summary:\n")
cat("  PHQ-9:  ", sum(data$missing_phq_12m), " missing (",
    round(mean(data$missing_phq_12m, na.rm=TRUE)*100, 1), "%)\n", sep="")
cat("  BSI-18: ", sum(data$missing_bsi_12m), " missing (",
    round(mean(data$missing_bsi_12m, na.rm=TRUE)*100, 1), "%)\n", sep="")

# Descriptive comparison - Missingness by severity groups
cat("Comparing missingness rates across severity groups...\n\n")

# Function to compare missingness across groups
compare_missingness <- function(data, outcome_var, severity_var, severity_name) {
  cross_tab <- table(data[[outcome_var]], data[[severity_var]], useNA = "no")
  pct_missing <- prop.table(cross_tab, margin = 2)[2, ] * 100

  test_result <- tryCatch({
    if(min(cross_tab) < 5) {
      fisher.test(cross_tab)
    } else {
      chisq.test(cross_tab)
    }
  }, error = function(e) NULL)

  list(severity_name = severity_name, outcome_name = outcome_var,
       percentages = pct_missing, test = test_result)
}

# Test severity measures
severity_vars <- list(gcs_severity = "GCS Severity",
                      imaging_outcome = "CT Imaging Outcome",
                      admission = "Admission Type",
                      injury_type = "Injury Type",
                      loc_duration = "LOC Duration",
                      pta_duration = "PTA Duration",
                      aoc_duration = "AOC Duration")

descriptive_results <- list()

for(severity_var in names(severity_vars)) {
  if(severity_var %in% names(data)) {
    cat("\n--- ", severity_vars[[severity_var]], " ---\n", sep="")

    for(outcome_var in c("missing_phq_12m", "missing_bsi_12m")) {
      result <- compare_missingness(data, outcome_var, severity_var, severity_vars[[severity_var]])
      descriptive_results[[paste(severity_var, outcome_var, sep="_")]] <- result

      outcome_label <- switch(outcome_var, missing_phq_12m = "PHQ-9",
                              missing_bsi_12m = "BSI-18")

      cat("\n", outcome_label, " missingness by ", severity_vars[[severity_var]], ":\n", sep="")
      print(round(result$percentages, 1))

      if(!is.null(result$test)) {
        cat("  Test p-value: ", format.pval(result$test$p.value, digits=3), "\n", sep="")
      }
    }
  }
}

# Univariate logistic regression models
cat("\n\n=== Univariate Logistic Regression Models ===\n")
cat("Testing each severity measure separately\n\n")

# Define severity predictors
severity_predictors <- list(
  gcs_severity = "GCS Severity (vs Mild)",
  imaging_outcome = "CT Abnormal (vs Normal)",
  admission = "Admission Type (vs ED discharge)",
  injury_type = "Injury Type (vs Ortho)",
  loc_duration = "LOC Duration",
  pta_duration = "PTA Duration",
  aoc_duration = "AOC Duration",
  gcs_score_reversed = "GCS Score (reversed, continuous)",
  ais_injury_severity = "AIS Injury Severity Score",
  marshall_score = "Marshall CT Score"
)

# Function to run univariate logistic regression
run_univariate_model <- function(data, outcome, predictor, predictor_name) {
  if(!predictor %in% names(data) || sum(!is.na(data[[predictor]])) < 10) {
    return(NULL)
  }

  formula_str <- paste(outcome, "~", predictor)
  model <- tryCatch({
    glm(as.formula(formula_str), family = binomial, data = data)
  }, error = function(e) NULL)

  if(is.null(model)) return(NULL)

  tidy_result <- broom::tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(term != "(Intercept)") %>%
    mutate(predictor_name = predictor_name, outcome = outcome, predictor_var = predictor)

  return(tidy_result)
}

# Run all univariate models
univariate_results <- list()

for(outcome in c("missing_phq_12m", "missing_bsi_12m")) {
  outcome_label <- switch(outcome, missing_phq_12m = "PHQ-9",
                          missing_bsi_12m = "BSI-18")

  cat("\n--- ", outcome_label, " 12-month Missingness ---\n", sep="")

  for(predictor in names(severity_predictors)) {
    result <- run_univariate_model(data, outcome, predictor, severity_predictors[[predictor]])

    if(!is.null(result)) {
      univariate_results[[paste(outcome, predictor, sep="_")]] <- result

      cat("\n  ", severity_predictors[[predictor]], ":\n", sep="")
      for(i in 1:nrow(result)) {
        cat("    ", result$term[i], ": OR = ", round(result$estimate[i], 2),
            " (95% CI: ", round(result$conf.low[i], 2), "-", round(result$conf.high[i], 2),
            "), p = ", format.pval(result$p.value[i], digits=3), "\n", sep="")
      }
    }
  }
}

all_univariate_results <- bind_rows(univariate_results)

# Multivariate logistic regression models
cat("\n\n=== Multivariate Logistic Regression Models ===\n")
cat("Testing multiple severity measures together, controlling for confounders\n\n")

multivariate_results <- list()

for(outcome in c("missing_phq_12m", "missing_bsi_12m")) {
  outcome_label <- switch(outcome, missing_phq_12m = "PHQ-9",
                          missing_bsi_12m = "BSI-18")

  cat("\n========== ", outcome_label, " 12-month Missingness ==========\n", sep="")

  # Full model with multiple severity measures + confounders
  model_formula <- paste(outcome, "~ gcs_severity + imaging_outcome + admission +",
                         "age + sex + race + family_income + site")

  model <- tryCatch({
    glm(as.formula(model_formula), family = binomial, data = data)
  }, error = function(e) NULL)

  if(!is.null(model)) {
    cat("  N = ", nobs(model), ", AIC = ", round(AIC(model), 1), "\n", sep="")

    tidy_result <- broom::tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
      filter(term != "(Intercept)") %>%
      mutate(model_name = paste(outcome_label, "Full Model"), outcome = outcome)

    multivariate_results[[outcome]] <- tidy_result

    # Print severity variable results
    severity_terms <- tidy_result %>%
      filter(grepl("gcs|imaging|admission|injury_type|loc_duration|pta_duration|aoc_duration", term))

    if(nrow(severity_terms) > 0) {
      cat("\n  Severity measure results:\n")
      for(i in 1:nrow(severity_terms)) {
        sig_marker <- ifelse(severity_terms$p.value[i] < 0.05, "*", " ")
        cat("    ", severity_terms$term[i], ": OR = ", round(severity_terms$estimate[i], 2),
            " (95% CI: ", round(severity_terms$conf.low[i], 2), "-",
            round(severity_terms$conf.high[i], 2),
            "), p = ", format.pval(severity_terms$p.value[i], digits=3), sig_marker, "\n", sep="")
      }
    }
  }
}

all_multivariate_results <- bind_rows(multivariate_results)

# Create summary tables
cat("\n=== Creating Summary Tables ===\n")

for(outcome in c("missing_phq_12m", "missing_bsi_12m")) {
  outcome_label <- switch(outcome, missing_phq_12m = "PHQ-9",
                          missing_bsi_12m = "BSI-18")

  formula_str <- paste(outcome, "~ gcs_severity + imaging_outcome + admission + injury_type +",
                       "age + sex + race + family_income")

  model <- tryCatch({
    glm(as.formula(formula_str), family = binomial, data = data)
  }, error = function(e) NULL)

  if(!is.null(model)) {
    tbl <- tbl_regression(model, exponentiate = TRUE,
                          label = list(gcs_severity ~ "GCS Severity",
                                       imaging_outcome ~ "CT Imaging",
                                       admission ~ "Admission Type",
                                       injury_type ~ "Injury Type",
                                       age ~ "Age", sex ~ "Sex",
                                       race ~ "Race", family_income ~ "Family Income")) %>%
      bold_p(t = 0.05) %>%
      bold_labels() %>%
      modify_header(estimate ~ "**OR (95% CI)**") %>%
      modify_caption(paste0("**Predictors of ", outcome_label, " 12-month Missingness**"))

    tbl %>%
      as_gt() %>%
      gtsave(filename = paste0(output_dir, "missingness_",
                               gsub("missing_|_12m", "", outcome), "_table.html"))
  }
}

cat("  Saved HTML tables to output directory\n")

# Create forest plots
cat("\n=== Creating Forest Plots ===\n")

for(outcome in c("missing_phq_12m", "missing_bsi_12m")) {
  outcome_label <- switch(outcome, missing_phq_12m = "PHQ-9",
                          missing_bsi_12m = "BSI-18")

  plot_data <- all_multivariate_results %>%
    filter(outcome == !!outcome) %>%
    filter(grepl("gcs|imaging|admission|injury_type|loc_duration|pta_duration|aoc_duration", term)) %>%
    mutate(term_clean = case_when(
      term == "gcs_severityModerate" ~ "Moderate TBI (vs Mild)",
      term == "gcs_severitySevere" ~ "Severe TBI (vs Mild)",
      term == "imaging_outcomeAbnormal" ~ "Abnormal CT (vs Normal)",
      grepl("admission.*ICU", term) ~ "ICU Admission (vs ED)",
      grepl("admission.*no ICU", term) ~ "Hospital Admission (vs ED)",
      term == "injury_typeTBI" ~ "TBI (vs Ortho)",
      grepl("loc_duration", term) ~ gsub("loc_duration", "LOC: ", term),
      grepl("pta_duration", term) ~ gsub("pta_duration", "PTA: ", term),
      grepl("aoc_duration", term) ~ gsub("aoc_duration", "AOC: ", term),
      TRUE ~ term
    ), significant = p.value < 0.05)

  if(nrow(plot_data) > 0) {
    forest_plot <- ggplot(plot_data, aes(x = estimate, y = term_clean, color = significant)) +
      geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
      geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, linewidth = 0.8) +
      geom_point(size = 3) +
      scale_color_manual(values = c("FALSE" = "gray60", "TRUE" = "red"),
                         labels = c("Not significant", "p < 0.05"), name = "") +
      scale_x_log10(breaks = c(0.25, 0.5, 1, 2, 4)) +
      labs(title = paste("Predictors of", outcome_label, "12-Month Missingness"),
           subtitle = "Odds ratios from multivariate logistic regression",
           x = "Odds Ratio (log scale)", y = "") +
      theme_minimal(base_size = 11) +
      theme(legend.position = "bottom", panel.grid.minor = element_blank(),
            plot.title = element_text(face = "bold"))

    ggsave(paste0(output_dir, "missingness_forest_",
                  gsub("missing_|_12m", "", outcome), ".png"),
           forest_plot, width = 10, height = 6, dpi = 300)

    ggsave(paste0(output_dir, "missingness_forest_",
                  gsub("missing_|_12m", "", outcome), ".pdf"),
           forest_plot, width = 10, height = 6)
  }
}

cat("  Saved forest plots to output directory\n")

# Generate comprehensive report
cat("\n=== Generating Comprehensive Report ===\n")

sink(paste0(output_dir, "missingness_severity_analysis_report.txt"))

cat("MISSINGNESS AND SEVERITY ANALYSIS REPORT\n")
cat("=========================================\n\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

cat("RESEARCH QUESTION:\n")
cat("Does acute TBI severity predict missingness in 12-month mental health outcomes?\n\n")

cat("SEVERITY PREDICTORS TESTED:\n")
cat("---------------------------\n")
cat("  - GCS severity (mild/moderate/severe)\n")
cat("  - CT imaging outcome (normal/abnormal)\n")
cat("  - Admission type (ED discharge/hospital/ICU)\n")
cat("  - Injury type (TBI/Ortho)\n")
cat("  - LOC duration (categorical)\n")
cat("  - PTA duration (categorical)\n")
cat("  - AOC duration (categorical)\n")
cat("  - GCS score (continuous, reversed)\n")
cat("  - AIS injury severity score\n")
cat("  - Marshall CT score\n\n")

cat("MISSINGNESS SUMMARY\n")
cat("-------------------\n")
cat("PHQ-9 12-month:  ", sum(data$missing_phq_12m, na.rm=TRUE), " missing (",
    round(mean(data$missing_phq_12m, na.rm=TRUE)*100, 1), "%)\n", sep="")
cat("BSI-18 12-month: ", sum(data$missing_bsi_12m, na.rm=TRUE), " missing (",
    round(mean(data$missing_bsi_12m, na.rm=TRUE)*100, 1), "%)\n", sep="")

cat("KEY FINDINGS - UNIVARIATE MODELS\n")
cat("---------------------------------\n\n")

sig_univariate <- all_univariate_results %>%
  filter(p.value < 0.05) %>%
  arrange(outcome, p.value)

if(nrow(sig_univariate) > 0) {
  cat("Significant associations (p < 0.05):\n\n")
  for(i in 1:nrow(sig_univariate)) {
    outcome_label <- switch(sig_univariate$outcome[i], missing_phq_12m = "PHQ-9",
                            missing_bsi_12m = "BSI-18")
    cat("  ", outcome_label, ": ", sig_univariate$predictor_name[i], " - ",
        sig_univariate$term[i], "\n", sep="")
    cat("    OR = ", round(sig_univariate$estimate[i], 2),
        " (95% CI: ", round(sig_univariate$conf.low[i], 2), "-",
        round(sig_univariate$conf.high[i], 2), "), p = ",
        format.pval(sig_univariate$p.value[i], digits=3), "\n\n", sep="")
  }
} else {
  cat("No significant associations found.\n\n")
}

cat("KEY FINDINGS - MULTIVARIATE MODELS\n")
cat("-----------------------------------\n\n")

sig_multivariate <- all_multivariate_results %>%
  filter(grepl("gcs|imaging|admission|injury_type|loc_duration|pta_duration|aoc_duration", term)) %>%
  filter(p.value < 0.05) %>%
  arrange(outcome, p.value)

if(nrow(sig_multivariate) > 0) {
  cat("Significant associations after adjusting for confounders (p < 0.05):\n\n")
  for(outcome in c("missing_phq_12m", "missing_bsi_12m")) {
    outcome_label <- switch(outcome, missing_phq_12m = "PHQ-9",
                            missing_bsi_12m = "BSI-18")
    outcome_sigs <- sig_multivariate %>% filter(outcome == !!outcome)
    if(nrow(outcome_sigs) > 0) {
      cat("\n", outcome_label, " Missingness:\n", sep="")
      for(i in 1:nrow(outcome_sigs)) {
        cat("  ", outcome_sigs$term[i], ": OR = ", round(outcome_sigs$estimate[i], 2),
            " (95% CI: ", round(outcome_sigs$conf.low[i], 2), "-",
            round(outcome_sigs$conf.high[i], 2), "), p = ",
            format.pval(outcome_sigs$p.value[i], digits=3), "\n", sep="")
      }
    }
  }
} else {
  cat("No significant associations found after adjusting for confounders.\n\n")
}

cat("\nINTERPRETATION AND IMPLICATIONS\n")
cat("--------------------------------\n")

n_sig_findings <- nrow(sig_multivariate)

if(n_sig_findings == 0) {
  cat("\nRESULT: No significant associations found.\n")
  cat("DATA: Likely Missing At Random (MAR) with respect to severity\n")
  cat("IMPLICATIONS: Multiple imputation assumptions are supported\n")
} else {
  cat("\nRESULT: Evidence for severity-related missingness found.\n")
  cat("DATA: May be Missing Not At Random (MNAR)\n")
  cat("IMPLICATIONS: Consider sensitivity analyses\n")
}

sink()

cat("Report saved to:", paste0(output_dir, "missingness_severity_analysis_report.txt"), "\n")

# Save results as R objects
missingness_analysis_results <- list(
  univariate_results = all_univariate_results,
  multivariate_results = all_multivariate_results,
  descriptive_comparisons = descriptive_results
)

saveRDS(missingness_analysis_results,
        paste0(output_dir, "missingness_severity_analysis_results.rds"))

cat("Results saved to:", paste0(output_dir, "missingness_severity_analysis_results.rds"), "\n")
cat("\n=== MISSINGNESS-SEVERITY ANALYSIS COMPLETE ===\n\n")

##############################################################################
# STEP 5: Store subject key and remove before imputation
##############################################################################

id_sk <- select(data_for_imputation, subjectkey)

##############################################################################
# SAVE NON-IMPUTED VERSION: Save dataset before imputation
##############################################################################

cat("\n=== Saving Non-Imputed Dataset ===\n")

# Create non-imputed wide dataset by merging with outcomes
nonimputed_wide_data <- data_for_imputation %>%
  left_join(outcomes_12_months, by = "subjectkey")

# Add derived variables and acute severity measures to non-imputed data
nonimputed_wide_data$drug_total_score <- data$drug_total_score[match(nonimputed_wide_data$subjectkey, data$subjectkey)]
nonimputed_wide_data$any_mh_disorder <- data$any_mh_disorder[match(nonimputed_wide_data$subjectkey, data$subjectkey)]
nonimputed_wide_data$psychologist_total <- data$psychologist_total[match(nonimputed_wide_data$subjectkey, data$subjectkey)]
nonimputed_wide_data$psychiatrist_total <- data$psychiatrist_total[match(nonimputed_wide_data$subjectkey, data$subjectkey)]
nonimputed_wide_data$inpatient_total <- data$inpatient_total[match(nonimputed_wide_data$subjectkey, data$subjectkey)]
nonimputed_wide_data$outpatient_total <- data$outpatient_total[match(nonimputed_wide_data$subjectkey, data$subjectkey)]
nonimputed_wide_data$psyc_hospital_total <- data$psyc_hospital_total[match(nonimputed_wide_data$subjectkey, data$subjectkey)]
nonimputed_wide_data$any_mh_service <- data$any_mh_service[match(nonimputed_wide_data$subjectkey, data$subjectkey)]
nonimputed_wide_data$gcs_score_reversed <- data$gcs_score_reversed[match(nonimputed_wide_data$subjectkey, data$subjectkey)]
nonimputed_wide_data$gcs_severity <- data$gcs_severity[match(nonimputed_wide_data$subjectkey, data$subjectkey)]

# Add previous TBI (not imputed, kept from original data)
nonimputed_wide_data$previous_tbi <- data$previous_tbi[match(nonimputed_wide_data$subjectkey, data$subjectkey)]
nonimputed_wide_data$worst_tbi <- data$worst_tbi[match(nonimputed_wide_data$subjectkey, data$subjectkey)]

# Add acute severity measures (not imputed, kept from original data)
nonimputed_wide_data$gcs_score <- data$gcs_score[match(nonimputed_wide_data$subjectkey, data$subjectkey)]
nonimputed_wide_data$injury_cause <- data$injury_cause[match(nonimputed_wide_data$subjectkey, data$subjectkey)]
nonimputed_wide_data$injury_mechanism <- data$injury_mechanism[match(nonimputed_wide_data$subjectkey, data$subjectkey)]
nonimputed_wide_data$loc_duration <- data$loc_duration[match(nonimputed_wide_data$subjectkey, data$subjectkey)]
nonimputed_wide_data$pta_duration <- data$pta_duration[match(nonimputed_wide_data$subjectkey, data$subjectkey)]
nonimputed_wide_data$aoc_duration <- data$aoc_duration[match(nonimputed_wide_data$subjectkey, data$subjectkey)]
nonimputed_wide_data$loc_indicator <- data$loc_indicator[match(nonimputed_wide_data$subjectkey, data$subjectkey)]
nonimputed_wide_data$pta_indicator <- data$pta_indicator[match(nonimputed_wide_data$subjectkey, data$subjectkey)]
nonimputed_wide_data$aoc_indicator <- data$aoc_indicator[match(nonimputed_wide_data$subjectkey, data$subjectkey)]
nonimputed_wide_data$admission <- data$admission[match(nonimputed_wide_data$subjectkey, data$subjectkey)]
nonimputed_wide_data$admission_type <- data$admission_type[match(nonimputed_wide_data$subjectkey, data$subjectkey)]
nonimputed_wide_data$ais_injury_severity <- data$ais_injury_severity[match(nonimputed_wide_data$subjectkey, data$subjectkey)]
nonimputed_wide_data$imaging_outcome <- data$imaging_outcome[match(nonimputed_wide_data$subjectkey, data$subjectkey)]
nonimputed_wide_data$marshall_score <- data$marshall_score[match(nonimputed_wide_data$subjectkey, data$subjectkey)]

# Save non-imputed dataset
saveRDS(nonimputed_wide_data,
        paste(processed_data_dir, "nonimputed_wide_data.rds", sep = ""))

cat("Non-imputed dataset saved to:", paste(processed_data_dir, "nonimputed_wide_data.rds", sep = ""), "\n")
cat("  - Total observations:", nrow(nonimputed_wide_data), "\n")
cat("  - Total variables:", ncol(nonimputed_wide_data), "\n")

# Also save non-imputed version with only TBI/Ortho cases
nonimputed_wide_data_oi <- nonimputed_wide_data %>%
  filter(injury_type %in% c("TBI", "Ortho"))

saveRDS(nonimputed_wide_data_oi,
        paste(processed_data_dir, "nonimputed_wide_data_oi.rds", sep = ""))

cat("Non-imputed TBI/Ortho dataset saved to:", paste(processed_data_dir, "nonimputed_wide_data_oi.rds", sep = ""), "\n\n")

# Now remove subject key for imputation
data_for_imputation$subjectkey <- NULL

# Convert to data frame
data_for_imputation <- as.data.frame(data_for_imputation)

##############################################################################
# STEP 6: Perform imputation with mice
##############################################################################

# Record start time
start_time <- Sys.time()

# Set seed again for imputation
set.seed(777)

# Perform imputation using mice
# m specifies number of imputed datasets
# maxit specifies maximum iterations
# - convergence plots shows maxit = 60 necessary for convergence
# mice automatically selects appropriate methods based on data types:
#   - "pmm" for numeric variables
#   - "logreg" for binary factors
#   - "polyreg" for unordered factors with >2 levels
#   - "polr" for ordered factors

imputed_result <- mice(
  data_for_imputation,
  m = 20,
  maxit = 100,
  seed = 777,
  printFlag = TRUE
)

# Record end time
end_time <- Sys.time()
duration <- end_time - start_time

cat("Imputation Completed\n")
cat("Duration:", format(duration), "\n")

# Check convergence
pdf(paste(output_dir, "mice_convergence_plot.pdf", sep = ""))
plot(imputed_result)
dev.off()

# Extract first completed dataset to create imputed_data with actual imputed values
imputed_data <- complete(imputed_result, action = 1)

##############################################################################
# STEP 6.5: Post-imputation filtering for 12-month outcomes
# Remove imputed values where all early timepoints (2w, 3m, 6m) are missing
##############################################################################

cat("\n=== Applying Post-Imputation Filtering ===\n")

# Identify subjects with NO early PHQ data (all 3 early timepoints missing)
no_phq_early <- is.na(data$phq_global_score_2_weeks) &
                is.na(data$phq_global_score_3_months) &
                is.na(data$phq_global_score_6_months)

# Identify subjects with NO early BSI data (all 3 early timepoints missing)
no_bsi_early <- is.na(data$bsi_global_score_2_weeks) &
                is.na(data$bsi_global_score_3_months) &
                is.na(data$bsi_global_score_6_months)


cat("Subjects with no early PHQ data (2w, 3m, 6m all missing):", sum(no_phq_early), "\n")
cat("Subjects with no early BSI data (2w, 3m, 6m all missing):", sum(no_bsi_early), "\n")

# Count how many 12-month values were imputed before filtering
phq_12m_was_imputed <- is.na(data$phq_global_score_12_months) & !is.na(imputed_data$phq_global_score_12_months)
bsi_12m_was_imputed <- is.na(data$bsi_global_score_12_months) & !is.na(imputed_data$bsi_global_score_12_months)

cat("PHQ 12-month values imputed by MICE:", sum(phq_12m_was_imputed), "\n")
cat("BSI 12-month values imputed by MICE:", sum(bsi_12m_was_imputed), "\n")

# Revert imputed 12-month values to NA where no early data exists
imputed_data$phq_global_score_12_months[no_phq_early] <- NA
imputed_data$bsi_global_score_12_months[no_bsi_early] <- NA

# Count how many imputed values were removed
phq_removed <- sum(no_phq_early & phq_12m_was_imputed)
bsi_removed <- sum(no_bsi_early & bsi_12m_was_imputed)

cat("PHQ 12-month imputed values removed:", phq_removed, "\n")
cat("BSI 12-month imputed values removed:", bsi_removed, "\n")

# Report final counts
cat("\nFinal 12-month outcome availability:\n")
cat("  PHQ-9 12m:", sum(!is.na(imputed_data$phq_global_score_12_months)), "observations\n")
cat("  BSI-18 12m:", sum(!is.na(imputed_data$bsi_global_score_12_months)), "observations\n")

##############################################################################
# STEP 7: Validate imputation quality
##############################################################################

cat("\n Validating Imputation Quality \n")

# Function to compare distributions
compare_distributions <- function(original, imputed, varname) {
  if(is.numeric(original) && is.numeric(imputed)) {
    cat("\n", varname, "(numeric):\n")
    cat("  Original - Mean:", round(mean(original, na.rm=TRUE), 2),
        "SD:", round(sd(original, na.rm=TRUE), 2), "\n")
    cat("  Imputed  - Mean:", round(mean(imputed, na.rm=TRUE), 2),
        "SD:", round(sd(imputed, na.rm=TRUE), 2), "\n")

    # Check for implausible values
    if(any(imputed < min(original, na.rm=TRUE) | imputed > max(original, na.rm=TRUE))) {
      cat("  WARNING: Imputed values outside original range!\n")
    }
  } else if(is.factor(original) && is.factor(imputed)) {
    cat("\n", varname, "(factor):\n")
    orig_table <- prop.table(table(original, useNA="no"))
    imp_table <- prop.table(table(imputed, useNA="no"))
    comparison <- data.frame(
      Original = round(orig_table, 3),
      Imputed = round(imp_table, 3)
    )
    print(comparison)
  }
}

# Store original data with same structure for comparison
original_data_comparison <- select(data, -subjectkey)
original_data_comparison <- original_data_comparison[, names(imputed_data)]

# Check a few key variables
cat("\nComparing key variables:\n")
compare_distributions(original_data_comparison$age, imputed_data$age, "Age")
compare_distributions(original_data_comparison$audit_total_score, imputed_data$audit_total_score, "AUDIT Score")
compare_distributions(original_data_comparison$loc_duration, imputed_data$loc_duration, "LOC Duration")

# Save validation plot comparing key numeric variables
validation_plot <- imputed_data %>%
  select(where(is.numeric)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
  facet_wrap(~variable, scales = "free") +
  theme_minimal() +
  labs(title = "Distribution of Imputed Numeric Variables",
       subtitle = "Check for implausible values or unusual distributions")

ggsave(paste(output_dir, "imputed_distributions.png", sep = ""),
       plot = validation_plot, width = 14, height = 10)

# Create density plots to compare observed vs imputed
pdf(paste(output_dir, "mice_density_plots.pdf", sep = ""), width = 14, height = 10)
densityplot(imputed_result)
dev.off()

##############################################################################
# STEP 8: Recalculate derived variables AFTER imputation
##############################################################################

cat("\n=== Recalculating Derived Variables ===\n")
cat("Note: From this point forward, 'imputed_data' contains actual imputed values\n\n")

# Restore subject key
imputed_data$subjectkey <- id_sk$subjectkey

# Drug variables, mental health service use, and ACUTE SEVERITY MEASURES were NOT imputed
# Keep original values from complete case
original_nonimputed_data <- data %>%
  select(subjectkey, drug_total_score,
         sedative_2_weeks, tranquiliser_2_weeks, painkiller_2_weeks,
         stimulant_2_weeks, marijuana_2_weeks, cocaine_2_weeks,
         hallucinogen_2_weeks, inhalant_2_weeks, heroin_2_weeks,
         synthetics_2_weeks, drug_other_2_weeks,
         psychologist_2_weeks, psychologist_3_months,
         psychologist_6_months, psychologist_12_months,
         psychiatrist_2_weeks, psychiatrist_3_months,
         psychiatrist_6_months, psychiatrist_12_months,
         inpatient_12_months, outpatient_12_months,
         psychotherapy_12_months, psyc_med_12_months,
         psyc_hospital_12_months,
         # Previous TBI (NOT imputed)
         previous_tbi, worst_tbi,
         # Acute severity measures (NOT imputed)
         gcs_score, gcs_score_reversed, gcs_severity,
         injury_cause, injury_mechanism,
         loc_duration, pta_duration, aoc_duration,
         loc_indicator, pta_indicator, aoc_indicator,
         admission, admission_type,
         ais_injury_severity, imaging_outcome, marshall_score)

imputed_data <- imputed_data %>%
  left_join(original_nonimputed_data, by = "subjectkey")

# Calculate any mental health disorder from individual diagnoses
imputed_data$any_mh_disorder <- rowSums(imputed_data[, c(
  "anxiety_history", "depression_history", "ptsd_history",
  "schizophrenia_history", "bipolar_history", "adhd_history"
)] == "Yes")

# Create psychologist total variable
imputed_data <- imputed_data %>%
  mutate(psychologist_total = ifelse(
    psychologist_2_weeks == 'Yes' | psychologist_3_months == 'Yes' |
      psychologist_6_months == 'Yes' | psychologist_12_months == 'Yes',
    'Yes', 'No')
  )

# Create psychiatrist total variable
imputed_data <- imputed_data %>%
  mutate(psychiatrist_total = ifelse(
    psychiatrist_2_weeks == 'Yes' | psychiatrist_3_months == 'Yes' |
      psychiatrist_6_months == 'Yes' | psychiatrist_12_months == 'Yes',
    'Yes', 'No')
  )

# Create inpatient total variable (only measured at 12 months)
imputed_data <- imputed_data %>%
  mutate(inpatient_total = ifelse(
    inpatient_12_months == 'Yes',
    'Yes', 'No')
  )

# Create outpatient total variable (only measured at 12 months)
imputed_data <- imputed_data %>%
  mutate(outpatient_total = ifelse(
    outpatient_12_months == 'Yes',
    'Yes', 'No')
  )

# Create psyc hospital total variable
imputed_data <- imputed_data %>%
  mutate(psyc_hospital_total = ifelse(
    psyc_hospital_12_months == 'Yes',
    'Yes', 'No')
  )

# Create any mental health service variable
imputed_data <- imputed_data %>%
  mutate(any_mh_service = ifelse(
    psychologist_total == 'Yes' | psychiatrist_total == 'Yes' |
      psychotherapy_12_months == 'Yes' | psyc_hospital_total == 'Yes' |
      psyc_med_12_months == 'Yes',
    'Yes', 'No')
  )

# Convert derived totals to factors
imputed_data$psychologist_total <- as.factor(imputed_data$psychologist_total)
imputed_data$psychiatrist_total <- as.factor(imputed_data$psychiatrist_total)
imputed_data$inpatient_total <- as.factor(imputed_data$inpatient_total)
imputed_data$outpatient_total <- as.factor(imputed_data$outpatient_total)
imputed_data$psyc_hospital_total <- as.factor(imputed_data$psyc_hospital_total)
imputed_data$any_mh_service <- as.factor(imputed_data$any_mh_service)

cat("Derived variables recalculated:\n")
cat("  - any_mh_disorder\n")
cat("  - psychologist_total\n")
cat("  - psychiatrist_total\n")
cat("  - inpatient_total\n")
cat("  - outpatient_total\n")
cat("  - psyc_hospital_total\n")
cat("  - any_mh_service\n")
cat("\nVariables kept from original data (NOT imputed):\n")
cat("  - Previous TBI (previous_tbi, worst_tbi)\n")
cat("  - Acute severity measures (gcs_score, loc/pta/aoc, imaging, marshall, AIS)\n")
cat("  - Injury characteristics (injury_cause, injury_mechanism, admission type)\n")
cat("  - drug_total_score\n")
cat("  - Individual drug use variables (sedative, tranquiliser, etc.)\n")
cat("  - Post-injury mental health service use (psychologist, psychiatrist, etc.)\n")

##############################################################################
# STEP 9: Merge with non-imputed outcome variables
##############################################################################

cat("\n=== Merging with Remaining 12-month Outcomes ===\n")

# Create final dataset with imputed predictors and remaining outcomes
# Note: phq_global_score_12_months, bsi_global_score_12_months, and
# pcl_total_score_12_months are already in imputed_data
# (conditionally imputed with post-filtering)
imputed_wide_data <- imputed_data %>%
  left_join(outcomes_12_months, by = "subjectkey")

# Note: Acute severity measures (including gcs_score_reversed and gcs_severity)
# were already merged in STEP 8 from original_nonimputed_data

cat("Final dataset created with:\n")
cat("  - Imputed predictors and conditionally imputed 12m outcomes:", ncol(imputed_data) - 1, "variables\n")
cat("  - Additional 12-month outcomes (other BSI subscales):", ncol(outcomes_12_months) - 1, "variables\n")
cat("  - Total observations:", nrow(imputed_wide_data), "\n")

# Check how many complete cases for each outcome
cat("\nComplete cases for 12-month outcomes:\n")
cat("  BSI-18 (conditionally imputed):", sum(complete.cases(imputed_wide_data$bsi_global_score_12_months)), "\n")
cat("  PHQ-9 (conditionally imputed):", sum(complete.cases(imputed_wide_data$phq_global_score_12_months)), "\n")

##############################################################################
# STEP 10: Save imputed dataset
##############################################################################

cat("\n=== Saving Imputed Dataset ===\n")

saveRDS(imputed_wide_data,
        paste(processed_data_dir, "imputed_wide_data.rds", sep = ""))

cat("Saved to:", paste(processed_data_dir, "imputed_wide_data.rds", sep = ""), "\n")

# Also save just the TBI cases (excluding ortho controls) if needed
imputed_wide_data_oi <- imputed_wide_data %>%
  filter(injury_type %in% c("TBI", "Ortho"))

saveRDS(imputed_wide_data_oi,
        paste(processed_data_dir, "imputed_wide_data_oi.rds", sep = ""))

cat("Saved ortho/TBI dataset to:", paste(processed_data_dir, "imputed_wide_data_oi.rds", sep = ""), "\n")

# Save the mice object for later use if needed
saveRDS(imputed_result, paste(output_dir, "mice_imputation_object.rds", sep = ""))

# Create summary report
sink(paste(output_dir, "imputation_summary.txt", sep = ""))
cat("IMPUTATION SUMMARY REPORT\n")
cat("=========================\n\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Duration:", format(duration), "\n")
cat("Imputation method: MICE (Multivariate Imputation by Chained Equations)\n\n")
cat("Dataset naming convention:\n")
cat("  - 'data_for_imputation': Dataset BEFORE imputation (with missing values)\n")
cat("  - 'imputed_data': Dataset AFTER imputation (missing values filled in)\n\n")
cat("Variables imputed:", ncol(imputed_data) - 1, "\n")
cat("Observations:", nrow(imputed_data), "\n")
cat("Number of imputed datasets (m):", imputed_result$m, "\n")
cat("Maximum iterations:", imputed_result$iteration, "\n\n")
cat("Missing Data Summary (Top 20):\n")
print(head(miss_summary, 20))
cat("\n\nImputation methods used:\n")
print(imputed_result$method[imputed_result$method != ""])
cat("\n\nDerived variables recalculated after imputation:\n")
cat("  - any_mh_disorder (count of MH diagnoses)\n")
cat("  - psychologist_total, psychiatrist_total, etc.\n")
cat("\nVariables kept from original data (NOT imputed):\n")
cat("  - Previous TBI (previous_tbi, worst_tbi)\n")
cat("  - Acute TBI severity measures (gcs_score, loc/pta/aoc, imaging, marshall, AIS)\n")
cat("  - Injury characteristics (injury_cause, injury_mechanism, admission type)\n")
cat("  - drug_total_score and individual drug use variables\n")
cat("  - Post-injury mental health service use variables\n")
cat("\nRationale: Previous TBI and acute severity measures are primary predictors and may have\n")
cat("non-random missingness (MNAR). Using complete case analysis for these variables.\n")
cat("\n\n12-month outcomes - CONDITIONAL IMPUTATION:\n")
cat("  - bsi_global_score_12_months (imputed ONLY if early data exists)\n")
cat("  - phq_global_score_12_months (imputed ONLY if early data exists)\n")
cat("  - Filtering rule: If all early timepoints (2w, 3m, 6m) are missing, \n")
cat("    12-month value is NOT imputed (kept as NA)\n")
cat("  - PHQ 12m imputed values removed:", phq_removed, "\n")
cat("  - BSI 12m imputed values removed:", bsi_removed, "\n")
cat("\nComplete cases: BSI =", sum(complete.cases(imputed_wide_data$bsi_global_score_12_months)),
    "| PHQ =", sum(complete.cases(imputed_wide_data$phq_global_score_12_months)), "\n")
cat("\nNOTE: Using first completed dataset from MICE. To analyze pooled results\n")
cat("across all m imputed datasets, use the saved mice object with pool() function.\n")
sink()

cat("\n=== IMPUTATION COMPLETE ===\n")
cat("Summary report saved to:", paste(output_dir, "imputation_summary.txt", sep = ""), "\n")
cat("MICE object saved to:", paste(output_dir, "mice_imputation_object.rds", sep = ""), "\n")
