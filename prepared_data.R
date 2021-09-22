library(Hmisc)
library(kableExtra)
library(dplyr)
library(tidyverse)

rm(list=ls())

load("setup_basicfns.RData")

#Read Data
dat = read.csv('../report_card_data/0901/KPMPParticipantManag-RSReport_DATA_2021-09-01_1620.csv')

#Store Data of Participants who entered eligibility assessment
rct = dat[dat$redcap_event_name == "screening_arm_1",
          c("study_id","redcap_data_access_group","np_visit_date",
            "eligibility_assessment_complete","sc_eligible_yn","sc_date",
            "consent_complete","pt_consent","consent_date")]

#Store Data of Participants not followed
notfollowed = dat[dat$redcap_event_name == "patient_tracking_arm_1" & !is.na(dat$study_status),
                  c("study_id","redcap_data_access_group","study_status",
                    "withdraw_date","date_death","lfu_date","loe_date")]

data = merge(rct, notfollowed, by=c("study_id","redcap_data_access_group"), all.x=T)

#Store Data of Contact Log of Biopsy Result
pcl_bpresults = dat[!is.na(dat$pcl_reason) & dat$pcl_reason==1,
                    c("study_id","redcap_data_access_group","pcl_date","pcl_reason","pcl_notes","participant_contact_log_complete")]
pcl_bpresults = pcl_bpresults %>%
  group_by(study_id) %>%
  summarise(pcl_date = min(pcl_date, na.rm = TRUE))
data = merge(data, pcl_bpresults, by = "study_id", all.x = T)

#Store Data of Participants biopsied
bp = dat[dat$redcap_event_name == "biopsy_suite_arm_1",
         c("study_id","redcap_data_access_group",
           "kidney_biopsy_procedure_details_complete","bp_date","bp_kit_id_2")]
data = merge(data, bp, by=c("study_id", "redcap_data_access_group"), all.x=T)

data = data[data$redcap_data_access_group != "test_site",] #Delete test_site data
data$redcap_data_access_group = factor(data$redcap_data_access_group,
                                       levels=c("columbia","jhmi","upmc","yale", # AKI sites
                                                "brigham","ccf","joslin","utsw")) # CKD sites

data = data %>%
  mutate(eac = ifelse(!is.na(eligibility_assessment_complete) & eligibility_assessment_complete == 2, 
                      1, 0),
         eligible = ifelse(!is.na(eligibility_assessment_complete) & !is.na(sc_eligible_yn) & eligibility_assessment_complete==2 & sc_eligible_yn==1, 
                           1, 0),
         enrolled = ifelse(!is.na(consent_complete) & !is.na(pt_consent) & consent_complete==2 & pt_consent==1,
                           1, 0),
         biopsied = ifelse(!is.na(kidney_biopsy_procedure_details_complete) & !is.na(bp_date) & bp_date!="" &
                             !is.na(bp_kit_id_2) & bp_kit_id_2!="" &
                             kidney_biopsy_procedure_details_complete==2,
                           1, 0))

data$study_status.factor = factor(data$study_status, levels=c(3,4,5,6,98),
                                  labels=c("Withdrew from study","Deceased","Lost to follow-up","Loss of eligibility","Other"))

data = data %>%
  mutate(loe_prb = ifelse(!is.na(study_status)&study_status==6&biopsied==0,1,0),
         withdraw_prb = ifelse(!is.na(study_status)&study_status==3&biopsied==0,1,0),
         death_prb = ifelse(!is.na(study_status)&study_status==4&biopsied==0,1,0),
         lfu_prb = ifelse(!is.na(study_status)&study_status==5&biopsied==0,1,0),
         withdraw_pb = ifelse(!is.na(study_status)&study_status==3&biopsied==1,1,0),
         death_pb = ifelse(!is.na(study_status)&study_status==4&biopsied==1,1,0),
         lfu_pb = ifelse(!is.na(study_status)&study_status==5&biopsied==1,1,0))

data=data %>% 
  mutate(notfollowed_date=ifelse(study_status==3, withdraw_date,
                                 ifelse(study_status==4, date_death,
                                        ifelse(study_status==5, lfu_date,
                                               ifelse(study_status==6, loe_date,
                                                      "")))))


########################################################
###################### enrollment ######################
########################################################
enrollment = dat[dat$redcap_event_name=="enrollment_arm_1" & dat$redcap_repeat_instrument==""  & dat$study_id %in% data[data$enrolled==1,]$study_id, 
                 c("study_id","contact_information_complete","demographic_information_complete","participant_medical_history_complete","personal_history_complete","coordinator_medical_history_complete","physical_measurements_complete","participant_reported_outcome_measures_complete","health_literacy_questionnaire_complete","laboratory_results_complete")]

enrollment_blood = dat[dat$redcap_event_name=="enrollment_arm_1" & dat$redcap_repeat_instrument=="biosample_blood" & dat$study_id %in% data[data$enrolled==1,]$study_id,
                       c("study_id","redcap_repeat_instance","biosample_blood_complete")]
enrollment_spot_urine = dat[dat$redcap_event_name=="enrollment_arm_1" & dat$redcap_repeat_instrument=="biosample_spot_urine" & dat$study_id %in% data[data$enrolled==1,]$study_id,
                            c("study_id","redcap_repeat_instance","biosample_spot_urine_complete")]
enrollment_timed_urine = dat[dat$redcap_event_name=="enrollment_arm_1" & dat$redcap_repeat_instrument=="biosample_timed_urine" & dat$study_id %in% data[data$enrolled==1,]$study_id,
                             c("study_id","redcap_repeat_instance","biosample_timed_urine_complete")]
enrollment_stool = dat[dat$redcap_event_name=="enrollment_arm_1" & dat$redcap_repeat_instrument=="biosample_stool" & dat$study_id %in% data[data$enrolled==1,]$study_id,
                       c("study_id","redcap_repeat_instance","biosample_stool_complete")]

enrollment_blood = enrollment_blood %>%
  group_by(study_id) %>%
  summarise(biosample_blood_complete = max(biosample_blood_complete))

enrollment = merge(enrollment, enrollment_blood, by = "study_id", all.x = TRUE)

enrollment_spot_urine = enrollment_spot_urine %>%
  group_by(study_id) %>%
  summarise(biosample_spot_urine_complete = max(biosample_spot_urine_complete))
enrollment = merge(enrollment, enrollment_spot_urine, by = "study_id", all.x = TRUE)

enrollment_timed_urine = enrollment_timed_urine %>%
  group_by(study_id) %>%
  summarise(biosample_timed_urine_complete = max(biosample_timed_urine_complete))
enrollment = merge(enrollment, enrollment_timed_urine, by = "study_id", all.x = TRUE)

enrollment_stool = enrollment_stool %>%
  group_by(study_id) %>%
  summarise(biosample_stool_complete = max(biosample_stool_complete))
enrollment = merge(enrollment, enrollment_stool, by = "study_id", all.x = TRUE)

enrollment = enrollment %>%
  mutate(enrollment_crfs_completed = apply(enrollment[,-1], 1, function(x) sum(x==2, na.rm=T)),
         enrollmentaki_crfs_completed = apply(enrollment[,-c(1,8,9)], 1, function(x) sum(x==2, na.rm=T)))

data = merge(data, enrollment, by = "study_id", all.x = TRUE)
data = data %>%
  mutate(enrollment_crfs_expected = ifelse(enrolled == 1, 1*13, 0),
         enrollmentaki_crfs_expected = ifelse(enrolled == 1, 1*11, 0))

########################################################
######################## biopsy ########################
########################################################
pre_biopsy = dat[dat$redcap_event_name=="prebiopsy_arm_1" & dat$study_id %in% data[data$biopsied==1,]$study_id,
                 c("study_id","preclinical_assessment_clinician_complete","preclinical_assessment_investigator_complete","prebiopsy_safety_crf_complete")]
pre_biopsy = pre_biopsy %>%
  mutate(pre_biopsy_completed = apply(pre_biopsy[,-1], 1, function(x) sum(x==2, na.rm=T)))

biopsy_suite = dat[dat$redcap_event_name=="biopsy_suite_arm_1" & dat$study_id %in% data[data$biopsied==1,]$study_id,
                   c("study_id","kidney_biopsy_procedure_details_complete")]
biopsy_suite = biopsy_suite %>%
  mutate(biopsy_suite_completed = ifelse(kidney_biopsy_procedure_details_complete == 2, 1, 0))
biopsy_visit = merge(pre_biopsy, biopsy_suite, by = "study_id")

post_biopsy = dat[dat$redcap_event_name=="post_biopsy_arm_1" & dat$study_id %in% data[data$biopsied==1,]$study_id,
                  c("study_id","post_biopsy_complete","follow_up_clinical_assessment_complete","tissue_tracking_complete","pathology_images_upload_complete")]
post_biopsy = post_biopsy %>%
  mutate(post_biopsy_completed = apply(post_biopsy[,-1], 1, function(x) sum(x==2, na.rm=T)))
biopsy_visit = merge(biopsy_visit, post_biopsy, by ="study_id")
biopsy_visit = biopsy_visit %>%
  mutate(biopsy_crfs_completed = apply(biopsy_visit[,c("pre_biopsy_completed","biopsy_suite_completed","post_biopsy_completed")], 1, sum))

data = merge(data, biopsy_visit, by = "study_id", all.x =TRUE)
data = data %>%
  mutate(biopsy_crfs_expected = ifelse(biopsied == 1, 1*8, 0))

########################################################
data = data %>%
  mutate(notfollowed_pb = ifelse(lfu_pb==1|withdraw_pb==1|death_pb==1, 1, 0),
         notfollowed_pb_date = ifelse(notfollowed_pb == 1, notfollowed_date, ""),
         notfollowed_pb_since_bp = ifelse(notfollowed_pb == 1, as.Date(notfollowed_date) - as.Date(bp_date), ""))

notfollowed_pb_data = data[data$notfollowed_pb==1,c("study_id","notfollowed_pb_date","notfollowed_pb_since_bp")]

########################################################
######################## fu 24h ########################
########################################################
fu24h = dat[dat$redcap_event_name=="24_hour_followup_arm_1" & dat$redcap_repeat_instrument=="",
            c("study_id","redcap_repeat_instrument","redcap_repeat_instance","pfu_24hr_yn","pfu_24hr_date","hour_participant_followup_complete")]
fu24h = fu24h %>%
  mutate(fu24h_crfs_completed = ifelse(hour_participant_followup_complete == 2, 1, 0))
data = merge(data, fu24h, by = "study_id", all.x = TRUE)
data = data %>%
  mutate(fu24h_crfs_expected = ifelse(biopsied==1 & ((notfollowed_pb == 0 & as.Date(bp_date) + 2 < current_quarter[2]) | (notfollowed_pb == 1 & as.Date(bp_date) + 2 < notfollowed_pb_date)), 1, 0))

data = data %>%
  mutate(fu24h_visit_completed = ifelse(hour_participant_followup_complete == 2 & pfu_24hr_yn == 1, 1, 0),
         fu24h_visit_expected = ifelse(biopsied==1 & ((notfollowed_pb == 0 & as.Date(bp_date) + 2 < current_quarter[2]) | (notfollowed_pb == 1 & as.Date(bp_date) + 2 < notfollowed_pb_date)), 1, 0))

data$fu24h_crfs_completed = ifelse(data$fu24h_crfs_expected==0, 0, data$fu24h_crfs_completed)
data$fu24h_visit_completed = ifelse(data$fu24h_visit_expected==0, 0, data$fu24h_visit_completed)

########################################################
###################### fu 7d aki #######################
########################################################
fu7daki = dat[dat$redcap_event_name == "akionly_arm_1" & dat$redcap_repeat_instrument == "",
              c("study_id","aki_hospitalization_complete","ak57_blood_yn","ak57_urine_yn")]
fu7daki_spot_urine = dat[dat$redcap_event_name == "akionly_arm_1" & 
                           dat$redcap_repeat_instrument == "biosample_spot_urine",
                         c("study_id","redcap_repeat_instance","biosample_spot_urine_complete")]
fu7daki_daily_measurements = dat[dat$redcap_event_name == "akionly_arm_1" & 
                                   dat$redcap_repeat_instrument == "aki_daily_measurements",
                                 c("study_id","redcap_repeat_instance","aki_daily_measurements_complete")]
fu7daki_daily_progress_note = dat[dat$redcap_event_name == "akionly_arm_1" & 
                                    dat$redcap_repeat_instrument == "aki_daily_progress_note",
                                  c("study_id","redcap_repeat_instance","aki_daily_progress_note_complete")]
fu7daki_blood = dat[dat$redcap_event_name == "akionly_arm_1" & 
                      dat$redcap_repeat_instrument == "biosample_blood_aki",
                    c("study_id","redcap_repeat_instance","biosample_blood_aki_complete")]

fu7daki_spot_urine = fu7daki_spot_urine %>%
  group_by(study_id) %>%
  summarise(biosample_spot_urine_complete = max(biosample_spot_urine_complete))
fu7daki = merge(fu7daki, fu7daki_spot_urine, by = "study_id", all.x = TRUE)

fu7daki_daily_measurements = fu7daki_daily_measurements %>%
  group_by(study_id) %>%
  summarise(aki_daily_measurements_complete = max(aki_daily_measurements_complete))
fu7daki = merge(fu7daki, fu7daki_daily_measurements, by = "study_id", all.x = TRUE)

fu7daki_daily_progress_note = fu7daki_daily_progress_note %>%
  group_by(study_id) %>%
  summarise(aki_daily_progress_note_complete = max(aki_daily_progress_note_complete))
fu7daki = merge(fu7daki, fu7daki_daily_progress_note, by = "study_id", all.x = TRUE)

fu7daki_blood = fu7daki_blood %>%
  group_by(study_id) %>%
  summarise(biosample_blood_aki_complete = max(biosample_blood_aki_complete))
fu7daki = merge(fu7daki, fu7daki_blood, by = "study_id", all.x = TRUE)

fu7daki = fu7daki %>%
  mutate(fu7daki_crfs_completed = apply(fu7daki[,c("biosample_spot_urine_complete","aki_hospitalization_complete","aki_daily_measurements_complete","aki_daily_progress_note_complete","biosample_blood_aki_complete")], 1, function(x) sum(x==2, na.rm=T)))
data = merge(data, fu7daki, by = "study_id", all.x = TRUE)

data = data %>%
  mutate(fu7daki_crfs_expected = ifelse(biopsied==1 & (redcap_data_access_group %in% aki_sites) & ((notfollowed_pb == 0 & as.Date(bp_date) + 7 < current_quarter[2]) | (notfollowed_pb == 1 & as.Date(bp_date) + 7 < notfollowed_pb_date)), 1*5, 0))

data = data %>%
  mutate(fu7daki_visit_completed = ifelse(ak57_blood_yn %in% c(1,2) | ak57_urine_yn %in% c(1,2), 1, 0),
         fu7daki_visit_expected = ifelse(biopsied==1 & (redcap_data_access_group %in% aki_sites) & ((notfollowed_pb == 0 & as.Date(bp_date) + 7 < current_quarter[2]) | (notfollowed_pb == 1 & as.Date(bp_date) + 7 < notfollowed_pb_date)), 1, 0))

data$fu7daki_crfs_completed = ifelse(data$fu7daki_crfs_expected==0, 0, data$fu7daki_crfs_completed)
data$fu7daki_visit_completed = ifelse(data$fu7daki_visit_expected==0, 0, data$fu7daki_visit_completed)

########################################################
####################### fu 14d #########################
########################################################
fu14d=dat[dat$redcap_event_name=="14_day_followup_arm_1" & dat$redcap_repeat_instrument == "",
          c("study_id","fu_date","fu_yn","participant_followup_complete")]
colnames(fu14d)=c(colnames(fu14d)[1],paste0(colnames(fu14d)[-1],"_14d"))

fu14d = fu14d %>%
  mutate(fu14d_crfs_completed = ifelse(participant_followup_complete_14d == 2, 1, 0))
data = merge(data, fu14d, by = "study_id", all.x = TRUE)

data = data %>%
  mutate(fu14d_crfs_expected = ifelse(biopsied==1 & ((notfollowed_pb == 0 & as.Date(bp_date) + 20 < current_quarter[2]) | (notfollowed_pb == 1 & as.Date(bp_date) + 20 < notfollowed_pb_date)), 1, 0))

data = data %>%
  mutate(fu14d_visit_completed = ifelse(participant_followup_complete_14d == 2 & fu_yn_14d == 1, 1, 0),
         fu14d_visit_expected = ifelse(biopsied==1 & ((notfollowed_pb == 0 & as.Date(bp_date) + 20 < current_quarter[2]) | (notfollowed_pb == 1 & as.Date(bp_date) + 20 < notfollowed_pb_date)), 1, 0))

data$fu14d_crfs_completed = ifelse(data$fu14d_crfs_expected==0, 0, data$fu14d_crfs_completed)
data$fu14d_visit_completed = ifelse(data$fu14d_visit_expected==0, 0, data$fu14d_visit_completed)

########################################################
####################### fu 28d #########################
########################################################
fu28d=dat[dat$redcap_event_name=="28_day_followup_arm_1" & dat$redcap_repeat_instrument == "",
          c("study_id","fu_date","fu_yn","participant_followup_complete","participant_experience_survey_complete","participant_experience_survey_spanish_complete","participant_experience_survey_old_complete")]
colnames(fu28d)=c(colnames(fu28d)[1],paste0(colnames(fu28d)[2:4],"_28d"),colnames(fu28d[-c(1:4)]))

fu28d = fu28d %>%
  mutate(participant_experience_survey_28d_complete = ifelse(participant_experience_survey_complete==2 | participant_experience_survey_spanish_complete==2 | participant_experience_survey_old_complete==2, 2, 0))

fu28d = fu28d %>%
  mutate(fu28d_crfs_completed = apply(fu28d[,c("participant_followup_complete_28d","participant_experience_survey_28d_complete")], 1, function(x) sum(x==2, na.rm=T)))

data = merge(data, fu28d, by = "study_id", all.x = TRUE)

data = data %>%
  mutate(fu28d_crfs_expected = ifelse(biopsied==1 & ((notfollowed_pb == 0 & as.Date(bp_date) + 34 < current_quarter[2]) | (notfollowed_pb == 1 & as.Date(bp_date) + 34 < notfollowed_pb_date)), 1*2, 0))

data = data %>%
  mutate(fu28d_visit_completed = ifelse(participant_followup_complete_28d == 2 & fu_yn_28d == 1, 1, 0),
         fu28d_visit_expected = ifelse(biopsied==1 & ((notfollowed_pb == 0 & as.Date(bp_date) + 34 < current_quarter[2]) | (notfollowed_pb == 1 & as.Date(bp_date) + 34 < notfollowed_pb_date)), 1, 0))

data$fu28d_crfs_completed = ifelse(data$fu28d_crfs_expected==0, 0, data$fu28d_crfs_completed)
data$fu28d_visit_completed = ifelse(data$fu28d_visit_expected==0, 0, data$fu28d_visit_completed)
data$participant_experience_survey_28d_complete = ifelse(data$fu28d_visit_expected==0, 0, data$participant_experience_survey_28d_complete)

########################################################
###################### fu 3m aki #######################
########################################################
fu3maki = dat[dat$redcap_event_name=="3_months__akionly_arm_1" & dat$redcap_repeat_instrument == "",
              c("study_id","fu_date","fu_yn","participant_followup_complete","physical_measurements_complete","biosample_blood_complete","biosample_spot_urine_complete","participant_reported_outcome_measures_complete","laboratory_results_complete")]
colnames(fu3maki) = c(colnames(fu3maki)[1],paste0(colnames(fu3maki)[-1],"_3maki"))

fu3maki = merge(fu3maki, enrollment[,c("study_id","participant_reported_outcome_measures_complete","health_literacy_questionnaire_complete")], by = "study_id", all.x = TRUE)
fu3maki = fu3maki %>%
  mutate(participant_reported_outcome_measures_complete_enrollment_or_3maki = ifelse(participant_reported_outcome_measures_complete_3maki == 2 | participant_reported_outcome_measures_complete == 2,
                                                                                     2, 0))

fu3maki = fu3maki %>%
  mutate(fu3maki_crfs_completed = apply(fu3maki[,c("participant_followup_complete_3maki","physical_measurements_complete_3maki","biosample_blood_complete_3maki","biosample_spot_urine_complete_3maki","participant_reported_outcome_measures_complete_enrollment_or_3maki","laboratory_results_complete_3maki","health_literacy_questionnaire_complete")], 1, function(x) sum(x==2, na.rm=T)))

data = merge(data, fu3maki, by = "study_id", all.x = TRUE)

data = data %>%
  mutate(fu3maki_crfs_expected = ifelse(biopsied==1 & (redcap_data_access_group %in% aki_sites) & ((notfollowed_pb == 0 & as.Date(bp_date) < as.Date(current_quarter[2]) - months(4)) | (notfollowed_pb == 1 & as.Date(bp_date) < as.Date(notfollowed_pb_date) - months(4))), 1*7, 0))

data = data %>%
  mutate(fu3maki_visit_completed = ifelse(participant_followup_complete_3maki == 2 & fu_yn_3maki == 1, 1, 0),
         fu3maki_visit_expected = ifelse(biopsied==1 & (redcap_data_access_group %in% aki_sites) & ((notfollowed_pb == 0 & as.Date(bp_date) < as.Date(current_quarter[2]) - months(4)) | (notfollowed_pb == 1 & as.Date(bp_date) < as.Date(notfollowed_pb_date) - months(4))), 1, 0))

data$fu3maki_crfs_completed = ifelse(data$fu3maki_crfs_expected==0, 0, data$fu3maki_crfs_completed)
data$fu3maki_visit_completed = ifelse(data$fu3maki_visit_expected==0, 0, data$fu3maki_visit_completed)

########################################################
######################## fu 6m #########################
########################################################
fu6m = dat[dat$redcap_event_name=="6_months_arm_1" & dat$redcap_repeat_instrument == "",
           c("study_id","fu_date","fu_yn","participant_followup_complete","follow_up_medical_events_complete","laboratory_results_complete","participant_experience_survey_complete","participant_experience_survey_spanish_complete")]
colnames(fu6m) = c(colnames(fu6m)[1],paste0(colnames(fu6m)[-1],"_6m"))

fu6m = fu6m %>%
  mutate(participant_experience_survey_6m_complete = ifelse(participant_experience_survey_complete_6m==2 | participant_experience_survey_spanish_complete_6m==2, 2, 0))

fu6m = fu6m %>%
  mutate(fu6m_crfs_completed = apply(fu6m[,c("participant_followup_complete_6m","follow_up_medical_events_complete_6m","laboratory_results_complete_6m","participant_experience_survey_6m_complete")], 1, function(x) sum(x==2, na.rm=T)))

data = merge(data, fu6m, by = "study_id", all.x = TRUE)
data = data %>%
  mutate(fu6m_crfs_expected = ifelse(biopsied==1 & ((notfollowed_pb == 0 & as.Date(bp_date) + months(9) < current_quarter[2]) | (notfollowed_pb == 1 & as.Date(bp_date) + months(9) < notfollowed_pb_date)), 1*4, 0))

data = data %>%
  mutate(fu6m_visit_completed = ifelse(participant_followup_complete_6m == 2 & fu_yn_6m == 1, 1, 0),
         fu6m_visit_expected = ifelse(biopsied==1 & ((notfollowed_pb == 0 & as.Date(bp_date) + months(9) < current_quarter[2]) | (notfollowed_pb == 1 & as.Date(bp_date) + months(9) < notfollowed_pb_date)), 1, 0))

data$fu6m_crfs_completed = ifelse(data$fu6m_crfs_expected==0, 0, data$fu6m_crfs_completed)
data$fu6m_visit_completed = ifelse(data$fu6m_visit_expected==0, 0, data$fu6m_visit_completed)
data$participant_experience_survey_6m_complete = ifelse(data$fu6m_visit_expected==0, 0, data$participant_experience_survey_6m_complete)

########################################################
######################## fu 12m ########################
########################################################
fu12m = dat[dat$redcap_event_name=="12_months_arm_1" & dat$redcap_repeat_instrument == "",
            c("study_id","fu_date","fu_yn","participant_followup_complete","physical_measurements_complete","health_literacy_questionnaire_complete","follow_up_medical_events_complete","follow_up_personal_history_complete","laboratory_results_complete","biosample_blood_complete","biosample_spot_urine_complete")]
colnames(fu12m) = c(colnames(fu12m)[1],paste0(colnames(fu12m)[-1],"_12m"))

fu12m = fu12m %>%
  mutate(fu12m_crfs_completed = apply(fu12m[,c("participant_followup_complete_12m","physical_measurements_complete_12m","health_literacy_questionnaire_complete_12m","follow_up_medical_events_complete_12m","follow_up_personal_history_complete_12m","laboratory_results_complete_12m","biosample_blood_complete_12m","biosample_spot_urine_complete_12m")], 1, function(x) sum(x==2, na.rm=T)))

data = merge(data, fu12m, by = "study_id", all.x = TRUE)

data = data %>%
  mutate(fu12m_crfs_expected = ifelse(biopsied==1 & ((notfollowed_pb == 0 & as.Date(bp_date) + months(15) < current_quarter[2]) | (notfollowed_pb == 1 & as.Date(bp_date) + months(15) < notfollowed_pb_date)), 1*8, 0))

data = data %>%
  mutate(fu12m_visit_completed = ifelse(participant_followup_complete_12m == 2 & fu_yn_12m == 1, 1, 0),
         fu12m_visit_expected = ifelse(biopsied==1 & ((notfollowed_pb == 0 & as.Date(bp_date) + months(15) < current_quarter[2]) | (notfollowed_pb == 1 & as.Date(bp_date) + months(15) < notfollowed_pb_date)), 1, 0))

data$fu12m_crfs_completed = ifelse(data$fu12m_crfs_expected==0, 0, data$fu12m_crfs_completed)
data$fu12m_visit_completed = ifelse(data$fu12m_visit_expected==0, 0, data$fu12m_visit_completed)

########################################################
######################## fu 18m ########################
########################################################
fu18m = dat[dat$redcap_event_name=="18_months_arm_1" & dat$redcap_repeat_instrument == "",
            c("study_id","fu_date","fu_yn","participant_followup_complete","laboratory_results_complete")]
colnames(fu18m) = c(colnames(fu18m)[1],paste0(colnames(fu18m)[-1],"_18m"))

fu18m = fu18m %>%
  mutate(fu18m_crfs_completed = apply(fu18m[,c("participant_followup_complete_18m","laboratory_results_complete_18m")], 1, function(x) sum(x==2, na.rm=T)))

data = merge(data, fu18m, by = "study_id", all.x = TRUE)

data = data %>%
  mutate(fu18m_crfs_expected = ifelse(biopsied==1 & ((notfollowed_pb == 0 & as.Date(bp_date) < as.Date(current_quarter[2]) - months(21)) | (notfollowed_pb == 1 & as.Date(bp_date) < as.Date(notfollowed_pb_date) - months(21))), 1*2, 0))
data$fu18m_crfs_completed = ifelse(data$fu18m_crfs_expected==0, 0, data$fu18m_crfs_completed)

data = data %>%
  mutate(fu18m_visit_completed = ifelse(participant_followup_complete_18m == 2 & fu_yn_18m == 1, 1, 0),
         fu18m_visit_expected = ifelse(biopsied==1 & ((notfollowed_pb == 0 & as.Date(bp_date) < as.Date(current_quarter[2]) - months(21)) | (notfollowed_pb == 1 & as.Date(bp_date) < as.Date(notfollowed_pb_date) - months(21))), 1, 0))

data$fu18m_crfs_completed = ifelse(data$fu18m_crfs_expected==0, 0, data$fu18m_crfs_completed)
data$fu18m_visit_completed = ifelse(data$fu18m_visit_expected==0, 0, data$fu18m_visit_completed)

################ Return of biopsy results ################
data = data %>%
  mutate(return_of_bp_1m_completed = ifelse(!is.na(pcl_date) & (as.Date(pcl_date) < as.Date(bp_date) + months(1)), 1, 0),
         return_of_bp_1m_expected = ifelse(biopsied == 1 & ((notfollowed_pb == 0 & as.Date(bp_date) + months(1) < current_quarter[2]) | (notfollowed_pb == 1 & as.Date(bp_date) + months(1) < notfollowed_pb_date)), 1, 0))

data$return_of_bp_1m_completed = ifelse(data$return_of_bp_1m_expected==0, 0, data$return_of_bp_1m_completed)

############## Column Names for each Metric ##############
visit_expected_aki = names(data)[str_detect(names(data), "visit_expected$")]
visit_expected_ckd = names(data)[str_detect(names(data), "visit_expected$") & !(str_detect(names(data), "aki_visit_expected$"))]
visit_completed_aki = names(data)[str_detect(names(data), "visit_completed$")]
visit_completed_ckd = names(data)[str_detect(names(data), "visit_completed$") & !(str_detect(names(data), "aki_visit_completed$"))]
crfs_expected_aki = names(data)[str_detect(names(data), "crfs_expected$")]
crfs_expected_ckd = names(data)[str_detect(names(data), "crfs_expected$") & !(str_detect(names(data), "aki_crfs_expected$"))]
crfs_completed_aki = names(data)[str_detect(names(data), "crfs_completed$")]
crfs_completed_ckd = names(data)[str_detect(names(data), "crfs_completed$") & !(str_detect(names(data), "aki_crfs_completed$"))]

library(chron)

blood_urine = read.csv('../report_card_data/0901/kit-qc-list-export0901.csv')
names(blood_urine) = c("kit_id", "redcap_data_access_group", "kit_type", "collection_date", "time_from_collection_to_frozen_hrs", "n_samples_affected")
blood_urine = blood_urine[-1,]
blood_urine = blood_urine %>% filter(redcap_data_access_group %in% c("CLU","JHMI","UPMC","YLE","BMC","CCF","JOS","UTSW"))
blood_urine$redcap_data_access_group = factor(blood_urine$redcap_data_access_group,
                                              levels=c("CLU","JHMI","UPMC","YLE", "BMC","CCF","JOS","UTSW"),
                                              labels=c("columbia","jhmi","upmc","yale", "brigham","ccf","joslin","utsw"))
blood_urine$max_time_from_collection_to_frozen_hrs = as.numeric(sapply(str_split(blood_urine$time_from_collection_to_frozen_hrs, " "), function(x) x[1]))
blood_urine = blood_urine %>%
  mutate(blood_samples = ifelse(kit_type %in% c("KPMP Followup Blood Kit 1st Annual", "KPMP Followup Blood Kit 3mAKI y2-10", "KPMP Post-Biopsy Blood Kit", "KPMP Standard Blood Kit"), 1, 0),
         spot_urine_samples = ifelse(kit_type %in% c("KPMP 20ml Spot Urine Kit", "KPMP Standard Urine Kit"), 1, 0))

acd_cbr = read.csv('../report_card_data/0901/acd-list-export0901.csv')
names(acd_cbr) = c("status", "timing_days", "sample_id", "from_org", "acd_created", "volume", "ship_date", "receipt_date", "pbmcs", "pbcms_created", "acd_terminated", "term_reason")
acd_cbr = acd_cbr[-1,]
acd_cbr$redcap_data_access_group = factor(sapply(str_split(acd_cbr$from_org, "\\("), function(x) x[2]),
                                          levels=c("CLU)","JHMI)","UPMC)","YLE)", "BMC)","CCF)","JOS)","UTSW)"),
                                          labels=c("columbia","jhmi","upmc","yale", "brigham","ccf","joslin","utsw"))
acd_cbr = acd_cbr[!is.na(acd_cbr$redcap_data_access_group) & acd_cbr$status == "Complete",]
acd_cbr$time_from_created_to_received = difftime(acd_cbr$receipt_date, acd_cbr$acd_created)

shipment = read.csv('../report_card_data/0901/shipment-qc-list-export0901.csv')[,-1]
shipment$redcap_data_access_group = factor(shipment$Ship.From,
                                           levels=c("CLU","JHMI","UPMC","YLE", "BMC","CCF","JOS","UTSW"),
                                           labels=c("columbia","jhmi","upmc","yale", "brigham","ccf","joslin","utsw"))
shipment = shipment[!is.na(shipment$redcap_data_access_group),]
shipment = shipment %>%
  mutate(issues = ifelse(Incorrect.Shipping.Conditions=="Yes" | Incorrect.Temp=="Yes", 1, 0),
         ship_date = as.Date(shipment$Ship.Date, format = "%m/%d/%Y"))

cores_triages = read.csv("../report_card_data/0901/biopsy-qc-dashboard-export0901.csv")[-c(1:3),]
cores_triages = cores_triages[,c(1:4,6,14,16,18,20,23,27)]
colnames(cores_triages) = c("study_id","bp_kit_id_2","redcap_data_access_group","bp_date","kit_type","core1_vial1","core1_vial2","core1_vial3","core2_vial4","core3_vial5","core3_vial6")
cores_triages = cores_triages %>%
  mutate(core3 = ifelse(kit_type=="A", core3_vial5, core3_vial6))

cores_triages$cores_obtained = apply(cores_triages[,c("core1_vial1","core1_vial2","core1_vial3","core2_vial4","core3")], 
                                     1, function(x) sum(x!="None"))

cores_triages$redcap_data_access_group = factor(cores_triages$redcap_data_access_group,
                                                levels=c("CLU","JHMI","UPMC","YLE",
                                                         "BMC","CCF","JOS","UTSW"),
                                                labels=c("columbia","jhmi","upmc","yale",
                                                         "brigham","ccf","joslin","utsw"))

cores_triages[cores_triages=="None"] <- "0:59:59"
cores_triages = cores_triages %>%
  mutate(core1_vial1_less10 = ifelse(as.times(core1_vial1) < as.times("00:10:00"), 1, 0),
         core1_vial2_less10 = ifelse(as.times(core1_vial2) < as.times("00:10:00"), 1, 0),
         core1_vial3_less10 = ifelse(as.times(core1_vial3) < as.times("00:10:00"), 1, 0),
         core2_vial4_less10 = ifelse(as.times(core2_vial4) < as.times("00:10:00"), 1, 0),
         core3_less10 = ifelse(as.times(core3) < as.times("00:10:00"), 1, 0),
         core3_less5 = ifelse(as.times(core3) < as.times("00:05:00"), 1, 0))

cryostor = read.csv('../report_card_data/0901/tissue-sample-tracker-export0901.csv')
names(cryostor) = c("sample_id","study_id","bp_kit_id_2","current_site","available","sample_type","length_mm",
                    "redcap_data_access_group","created","time_at_rs_days","shipped_to_cbr","time_at_cbr_days","tis1","shipped_to_tis1","t1", "tis2","shipped_to_tis2","t2","sample_terminal_date")
cryostor = cryostor[-c(1,2),]
cryostor$redcap_data_access_group = factor(cryostor$redcap_data_access_group,
                                           levels=c("CLU","JHMI","UPMC","YLE","BMC","CCF","JOS","UTSW"),
                                           labels=c("columbia","jhmi","upmc","yale","brigham","ccf","joslin","utsw"))
cryostor$created_date = as.Date(cryostor$created, format="%b %d, %Y")

save.image(file = "../report_card_data/0901/prepared_data0901.RData")