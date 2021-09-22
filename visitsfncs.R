library(Hmisc)
library(kableExtra)
library(dplyr)
library(tidyverse)

rm(list=ls())

load("setup_basicfns.RData")

calc_visits_crfs = function(data, type = "ckd", 
                            quarter = c(as.Date("2021-03-02"), as.Date("2021-06-04")), cum = FALSE){
  
  ckd_sites = c("brigham", "ccf", "joslin", "utsw")
  aki_sites = c("columbia", "jhmi", "upmc", "yale")
  
  data.enrolled.ckd = data %>% filter(enrolled == 1 & redcap_data_access_group %in% ckd_sites)
  data.enrolled.aki = data %>% filter(enrolled == 1 & redcap_data_access_group %in% aki_sites)
  
  data.biopsied.ckd = data %>% filter(biopsied == 1 & redcap_data_access_group %in% ckd_sites)
  data.biopsied.aki = data %>% filter(biopsied == 1 & redcap_data_access_group %in% aki_sites)
  
  range_24h = quarter - 2
  range_7daki = quarter - 7
  range_14d = quarter - 20
  range_28d = quarter - 34
  range_3maki = quarter - months(4)
  range_6m = quarter - months(9)
  range_12m = quarter - months(15)
  range_18m = quarter - months(21)
  
  if(cum == FALSE){ # calculate stats for a quarter
    
    if(type == "ckd"){
      
      data.enrolled.ckd_enrollment = data.enrolled.ckd %>%
        filter(consent_date >= quarter[1] & consent_date <= quarter[2])
      expected_crfs_enrollment = sum(data.enrolled.ckd_enrollment$enrollment_crfs_expected, na.rm=T)
      completed_crfs_enrollment = sum(data.enrolled.ckd_enrollment$enrollment_crfs_completed, na.rm=T)
      
      data.biopsied.ckd_biopsy = data.biopsied.ckd %>%
        filter(bp_date >= quarter[1] & bp_date <= quarter[2])
      expected_crfs_biopsy = sum(data.biopsied.ckd_biopsy$biopsy_crfs_expected, na.rm=T)
      completed_crfs_biopsy = sum(data.biopsied.ckd_biopsy$biopsy_crfs_completed, na.rm=T)
      
      data.biopsied.ckd_24h = data.biopsied.ckd %>% 
        filter(bp_date >= range_24h[1] & bp_date <= range_24h[2])
      expected_visits_24h = sum(data.biopsied.ckd_24h$fu24h_visit_expected, na.rm=T)
      completed_visits_24h = sum(data.biopsied.ckd_24h$fu24h_visit_completed, na.rm=T)
      expected_crfs_24h = sum(data.biopsied.ckd_24h$fu24h_crfs_expected, na.rm=T)
      completed_crfs_24h = sum(data.biopsied.ckd_24h$fu24h_crfs_completed, na.rm=T)
      
      data.biopsied.ckd_14d = data.biopsied.ckd %>% 
        filter(bp_date >= range_14d[1] & bp_date <= range_14d[2])
      expected_visits_14d = sum(data.biopsied.ckd_14d$fu14d_visit_expected, na.rm=T)
      completed_visits_14d = sum(data.biopsied.ckd_14d$fu14d_visit_completed, na.rm=T)
      expected_crfs_14d = sum(data.biopsied.ckd_14d$fu14d_crfs_expected, na.rm=T)
      completed_crfs_14d = sum(data.biopsied.ckd_14d$fu14d_crfs_completed, na.rm=T)
      
      data.biopsied.ckd_28d = data.biopsied.ckd %>% 
        filter(bp_date >= range_28d[1] & bp_date <= range_28d[2])
      expected_visits_28d = sum(data.biopsied.ckd_28d$fu28d_visit_expected, na.rm=T)
      completed_visits_28d = sum(data.biopsied.ckd_28d$fu28d_visit_completed, na.rm=T)
      expected_crfs_28d = sum(data.biopsied.ckd_28d$fu28d_crfs_expected, na.rm=T)
      completed_crfs_28d = sum(data.biopsied.ckd_28d$fu28d_crfs_completed, na.rm=T)
      
      data.biopsied.ckd_6m = data.biopsied.ckd %>% 
        filter(bp_date >= range_6m[1] & bp_date <= range_6m[2])
      expected_visits_6m = sum(data.biopsied.ckd_6m$fu6m_visit_expected, na.rm=T)
      completed_visits_6m = sum(data.biopsied.ckd_6m$fu6m_visit_completed, na.rm=T)
      expected_crfs_6m = sum(data.biopsied.ckd_6m$fu6m_crfs_expected, na.rm=T)
      completed_crfs_6m = sum(data.biopsied.ckd_6m$fu6m_crfs_completed, na.rm=T)
      
      data.biopsied.ckd_12m = data.biopsied.ckd %>% 
        filter(bp_date >= range_12m[1] & bp_date <= range_12m[2])
      expected_visits_12m = sum(data.biopsied.ckd_12m$fu12m_visit_expected, na.rm=T)
      completed_visits_12m = sum(data.biopsied.ckd_12m$fu12m_visit_completed, na.rm=T)
      expected_crfs_12m = sum(data.biopsied.ckd_12m$fu12m_crfs_expected, na.rm=T)
      completed_crfs_12m = sum(data.biopsied.ckd_12m$fu12m_crfs_completed, na.rm=T)
      
      data.biopsied.ckd_18m = data.biopsied.ckd %>%
        filter(bp_date >= range_18m[1] & bp_date <= range_18m[2])
      expected_visits_18m = sum(data.biopsied.ckd_18m$fu18m_visit_expected, na.rm=T)
      completed_visits_18m = sum(data.biopsied.ckd_18m$fu18m_visit_completed, na.rm=T)
      expected_crfs_18m = sum(data.biopsied.ckd_18m$fu18m_crfs_expected, na.rm=T)
      completed_crfs_18m = sum(data.biopsied.ckd_18m$fu18m_crfs_completed, na.rm=T)
      
      expected_visits = sum(expected_visits_24h, expected_visits_14d, expected_visits_28d, expected_visits_6m, expected_visits_12m, expected_visits_18m)
      completed_visits = sum(completed_visits_24h, completed_visits_14d, completed_visits_28d, completed_visits_6m, completed_visits_12m, completed_visits_18m)
      expected_crfs = sum(expected_crfs_enrollment, expected_crfs_biopsy, expected_crfs_24h, expected_crfs_14d, expected_crfs_28d, expected_crfs_6m, expected_crfs_12m, expected_crfs_18m)
      completed_crfs = sum(completed_crfs_enrollment, completed_crfs_biopsy, completed_crfs_24h, completed_crfs_14d, completed_crfs_28d, completed_crfs_6m, completed_crfs_12m, completed_crfs_18m)
      
      expected_pes_28d = expected_visits_28d
      completed_pes_28d = sum(data.biopsied.ckd_28d$participant_experience_survey_28d_complete==2, na.rm=T)
      
      expected_pes_6m = expected_visits_6m
      completed_pes_6m = sum(data.biopsied.ckd_6m$participant_experience_survey_6m_complete==2, na.rm=T)
      
      data.biopsied.ckd_bp = data.biopsied.ckd %>%
        filter(bp_date >= quarter[1] & bp_date <= quarter[2])
      expected_return_of_bp_1m = sum(data.biopsied.ckd_bp$return_of_bp_1m_expected, na.rm=T)
      completed_return_of_bp_1m = sum(data.biopsied.ckd_bp$return_of_bp_1m_completed, na.rm=T)

    } else{
      
      data.enrolled.aki_enrollment = data.enrolled.aki %>%
        filter(consent_date >= quarter[1] & consent_date <= quarter[2])
      expected_crfs_enrollment = sum(data.enrolled.aki_enrollment$enrollmentaki_crfs_expected, na.rm=T)
      completed_crfs_enrollment = sum(data.enrolled.aki_enrollment$enrollmentaki_crfs_completed, na.rm=T)
      
      data.biopsied.aki_biopsy = data.biopsied.aki %>%
        filter(bp_date >= quarter[1] & bp_date <= quarter[2])
      expected_crfs_biopsy = sum(data.biopsied.aki_biopsy$biopsy_crfs_expected, na.rm=T)
      completed_crfs_biopsy = sum(data.biopsied.aki_biopsy$biopsy_crfs_completed, na.rm=T)
      
      data.biopsied.aki_24h = data.biopsied.aki %>% 
        filter(bp_date >= range_24h[1] & bp_date <= range_24h[2])
      expected_visits_24h = sum(data.biopsied.aki_24h$fu24h_visit_expected, na.rm=T)
      completed_visits_24h = sum(data.biopsied.aki_24h$fu24h_visit_completed, na.rm=T)
      expected_crfs_24h = sum(data.biopsied.aki_24h$fu24h_crfs_expected, na.rm=T)
      completed_crfs_24h = sum(data.biopsied.aki_24h$fu24h_crfs_completed, na.rm=T)
      
      data.biopsied.aki_7daki = data.biopsied.aki %>% 
        filter(bp_date >= range_7daki[1] & bp_date <= range_7daki[2])
      expected_visits_7daki = sum(data.biopsied.aki_7daki$fu7daki_visit_expected, na.rm=T)
      completed_visits_7daki = sum(data.biopsied.aki_7daki$fu7daki_visit_completed, na.rm=T)
      expected_crfs_7daki = sum(data.biopsied.aki_7daki$fu7daki_crfs_expected, na.rm=T)
      completed_crfs_7daki = sum(data.biopsied.aki_7daki$fu7daki_crfs_completed, na.rm=T)
      
      data.biopsied.aki_14d = data.biopsied.aki %>% 
        filter(bp_date >= range_14d[1] & bp_date <= range_14d[2])
      expected_visits_14d = sum(data.biopsied.aki_14d$fu14d_visit_expected, na.rm=T)
      completed_visits_14d = sum(data.biopsied.aki_14d$fu14d_visit_completed, na.rm=T)
      expected_crfs_14d = sum(data.biopsied.aki_14d$fu14d_crfs_expected, na.rm=T)
      completed_crfs_14d = sum(data.biopsied.aki_14d$fu14d_crfs_completed, na.rm=T)
      
      data.biopsied.aki_28d = data.biopsied.aki %>% 
        filter(bp_date >= range_28d[1] & bp_date <= range_28d[2])
      expected_visits_28d = sum(data.biopsied.aki_28d$fu28d_visit_expected, na.rm=T)
      completed_visits_28d = sum(data.biopsied.aki_28d$fu28d_visit_completed, na.rm=T)
      expected_crfs_28d = sum(data.biopsied.aki_28d$fu28d_crfs_expected, na.rm=T)
      completed_crfs_28d = sum(data.biopsied.aki_28d$fu28d_crfs_completed, na.rm=T)
      
      data.biopsied.aki_3maki = data.biopsied.aki %>% 
        filter(bp_date >= range_3maki[1] & bp_date <= range_3maki[2])
      expected_visits_3maki = sum(data.biopsied.aki_3maki$fu3maki_visit_expected, na.rm=T)
      completed_visits_3maki = sum(data.biopsied.aki_3maki$fu3maki_visit_completed, na.rm=T)
      expected_crfs_3maki = sum(data.biopsied.aki_3maki$fu3maki_crfs_expected, na.rm=T)
      completed_crfs_3maki = sum(data.biopsied.aki_3maki$fu3maki_crfs_completed, na.rm=T)
      
      data.biopsied.aki_6m = data.biopsied.aki %>% 
        filter(bp_date >= range_6m[1] & bp_date <= range_6m[2])
      expected_visits_6m = sum(data.biopsied.aki_6m$fu6m_visit_expected, na.rm=T)
      completed_visits_6m = sum(data.biopsied.aki_6m$fu6m_visit_completed, na.rm=T)
      expected_crfs_6m = sum(data.biopsied.aki_6m$fu6m_crfs_expected, na.rm=T)
      completed_crfs_6m = sum(data.biopsied.aki_6m$fu6m_crfs_completed, na.rm=T)
      
      data.biopsied.aki_12m = data.biopsied.aki %>% 
        filter(bp_date >= range_12m[1] & bp_date <= range_12m[2])
      expected_visits_12m = sum(data.biopsied.aki_12m$fu12m_visit_expected, na.rm=T)
      completed_visits_12m = sum(data.biopsied.aki_12m$fu12m_visit_completed, na.rm=T)
      expected_crfs_12m = sum(data.biopsied.aki_12m$fu12m_crfs_expected, na.rm=T)
      completed_crfs_12m = sum(data.biopsied.aki_12m$fu12m_crfs_completed, na.rm=T)
      
      data.biopsied.aki_18m = data.biopsied.aki %>%
        filter(bp_date >= range_18m[1] & bp_date <= range_18m[2])
      expected_visits_18m = sum(data.biopsied.aki_18m$fu18m_visit_expected, na.rm=T)
      completed_visits_18m = sum(data.biopsied.aki_18m$fu18m_visit_completed, na.rm=T)
      expected_crfs_18m = sum(data.biopsied.aki_18m$fu18m_crfs_expected, na.rm=T)
      completed_crfs_18m = sum(data.biopsied.aki_18m$fu18m_crfs_completed, na.rm=T)

      expected_visits = sum(expected_visits_24h, expected_visits_7daki, expected_visits_14d, expected_visits_28d, expected_visits_3maki, expected_visits_6m, expected_visits_12m, expected_visits_18m)
      completed_visits = sum(completed_visits_24h, completed_visits_7daki, completed_visits_14d, completed_visits_28d, completed_visits_3maki, completed_visits_6m, completed_visits_12m, completed_visits_18m)
      expected_crfs = sum(expected_crfs_enrollment, expected_crfs_biopsy, expected_crfs_24h, expected_crfs_7daki, expected_crfs_14d, expected_crfs_28d, expected_crfs_3maki, expected_crfs_6m, expected_crfs_12m, expected_crfs_18m)
      completed_crfs = sum(completed_crfs_enrollment, completed_crfs_biopsy, completed_crfs_24h, completed_crfs_7daki, completed_crfs_14d, completed_crfs_28d, completed_crfs_3maki, completed_crfs_6m, completed_crfs_12m, completed_crfs_18m)
      
      expected_pes_28d = expected_visits_28d
      completed_pes_28d = sum(data.biopsied.aki_28d$participant_experience_survey_28d_complete==2, na.rm=T)
      
      expected_pes_6m = expected_visits_6m
      completed_pes_6m = sum(data.biopsied.aki_6m$participant_experience_survey_6m_complete==2, na.rm=T)
      
      data.biopsied.aki_bp = data.biopsied.aki %>%
        filter(bp_date >= quarter[1] & bp_date <= quarter[2])
      expected_return_of_bp_1m = sum(data.biopsied.aki_bp$return_of_bp_1m_expected, na.rm=T)
      completed_return_of_bp_1m = sum(data.biopsied.aki_bp$return_of_bp_1m_completed, na.rm=T)
      
    }
  } else{
    
    if(type == "ckd"){
      
      expected_crfs_enrollment = sum(data.enrolled.ckd$enrollment_crfs_expected, na.rm=T)
      completed_crfs_enrollment = sum(data.enrolled.ckd$enrollment_crfs_completed, na.rm=T)

      expected_crfs_biopsy = sum(data.biopsied.ckd$biopsy_crfs_expected, na.rm=T)
      completed_crfs_biopsy = sum(data.biopsied.ckd$biopsy_crfs_completed, na.rm=T)
      
      expected_visits_24h = sum(data.biopsied.ckd$fu24h_visit_expected, na.rm=T)
      completed_visits_24h = sum(data.biopsied.ckd$fu24h_visit_completed, na.rm=T)
      expected_crfs_24h = sum(data.biopsied.ckd$fu24h_crfs_expected, na.rm=T)
      completed_crfs_24h = sum(data.biopsied.ckd$fu24h_crfs_completed, na.rm=T)
      
      expected_visits_14d = sum(data.biopsied.ckd$fu14d_visit_expected, na.rm=T)
      completed_visits_14d = sum(data.biopsied.ckd$fu14d_visit_completed, na.rm=T)
      expected_crfs_14d = sum(data.biopsied.ckd$fu14d_crfs_expected, na.rm=T)
      completed_crfs_14d = sum(data.biopsied.ckd$fu14d_crfs_completed, na.rm=T)
      
      expected_visits_28d = sum(data.biopsied.ckd$fu28d_visit_expected, na.rm=T)
      completed_visits_28d = sum(data.biopsied.ckd$fu28d_visit_completed, na.rm=T)
      expected_crfs_28d = sum(data.biopsied.ckd$fu28d_crfs_expected, na.rm=T)
      completed_crfs_28d = sum(data.biopsied.ckd$fu28d_crfs_completed, na.rm=T)
      
      expected_visits_6m = sum(data.biopsied.ckd$fu6m_visit_expected, na.rm=T)
      completed_visits_6m = sum(data.biopsied.ckd$fu6m_visit_completed, na.rm=T)
      expected_crfs_6m = sum(data.biopsied.ckd$fu6m_crfs_expected, na.rm=T)
      completed_crfs_6m = sum(data.biopsied.ckd$fu6m_crfs_completed, na.rm=T)
      
      expected_visits_12m = sum(data.biopsied.ckd$fu12m_visit_expected, na.rm=T)
      completed_visits_12m = sum(data.biopsied.ckd$fu12m_visit_completed, na.rm=T)
      expected_crfs_12m = sum(data.biopsied.ckd$fu12m_crfs_expected, na.rm=T)
      completed_crfs_12m = sum(data.biopsied.ckd$fu12m_crfs_completed, na.rm=T)
      
      expected_visits_18m = sum(data.biopsied.ckd$fu18m_visit_expected, na.rm=T)
      completed_visits_18m = sum(data.biopsied.ckd$fu18m_visit_completed, na.rm=T)
      expected_crfs_18m = sum(data.biopsied.ckd$fu18m_crfs_expected, na.rm=T)
      completed_crfs_18m = sum(data.biopsied.ckd$fu18m_crfs_completed, na.rm=T)

      expected_visits = sum(expected_visits_24h, expected_visits_14d, expected_visits_28d, expected_visits_6m, expected_visits_12m, expected_visits_18m)
      completed_visits = sum(completed_visits_24h, completed_visits_14d, completed_visits_28d, completed_visits_6m, completed_visits_12m, completed_visits_18m)
      expected_crfs = sum(expected_crfs_enrollment, expected_crfs_biopsy, expected_crfs_24h, expected_crfs_14d, expected_crfs_28d, expected_crfs_6m, expected_crfs_12m, expected_crfs_18m)
      completed_crfs = sum(completed_crfs_enrollment, completed_crfs_biopsy, completed_crfs_24h, completed_crfs_14d, completed_crfs_28d, completed_crfs_6m, completed_crfs_12m, completed_crfs_18m)
      
      expected_pes_28d = expected_visits_28d
      completed_pes_28d = sum(data.biopsied.ckd$participant_experience_survey_28d_complete==2, na.rm=T)
      
      expected_pes_6m = expected_visits_6m
      completed_pes_6m = sum(data.biopsied.ckd$participant_experience_survey_6m_complete==2, na.rm=T)
      
      expected_return_of_bp_1m = sum(data.biopsied.ckd$return_of_bp_1m_expected, na.rm=T)
      completed_return_of_bp_1m = sum(data.biopsied.ckd$return_of_bp_1m_completed, na.rm=T)
      
    } else{
      
      expected_crfs_enrollment = sum(data.enrolled.aki$enrollmentaki_crfs_expected, na.rm=T)
      completed_crfs_enrollment = sum(data.enrolled.aki$enrollmentaki_crfs_completed, na.rm=T)
      
      expected_crfs_biopsy = sum(data.biopsied.aki$biopsy_crfs_expected, na.rm=T)
      completed_crfs_biopsy = sum(data.biopsied.aki$biopsy_crfs_completed, na.rm=T)
      
      expected_visits_24h = sum(data.biopsied.aki$fu24h_visit_expected, na.rm=T)
      completed_visits_24h = sum(data.biopsied.aki$fu24h_visit_completed, na.rm=T)
      expected_crfs_24h = sum(data.biopsied.aki$fu24h_crfs_expected, na.rm=T)
      completed_crfs_24h = sum(data.biopsied.aki$fu24h_crfs_completed, na.rm=T)
      
      expected_visits_7daki = sum(data.biopsied.aki$fu7daki_visit_expected, na.rm=T)
      completed_visits_7daki = sum(data.biopsied.aki$fu7daki_visit_completed, na.rm=T)
      expected_crfs_7daki = sum(data.biopsied.aki$fu7daki_crfs_expected, na.rm=T)
      completed_crfs_7daki = sum(data.biopsied.aki$fu7daki_crfs_completed, na.rm=T)
      
      expected_visits_14d = sum(data.biopsied.aki$fu14d_visit_expected, na.rm=T)
      completed_visits_14d = sum(data.biopsied.aki$fu14d_visit_completed, na.rm=T)
      expected_crfs_14d = sum(data.biopsied.aki$fu14d_crfs_expected, na.rm=T)
      completed_crfs_14d = sum(data.biopsied.aki$fu14d_crfs_completed, na.rm=T)
      
      expected_visits_28d = sum(data.biopsied.aki$fu28d_visit_expected, na.rm=T)
      completed_visits_28d = sum(data.biopsied.aki$fu28d_visit_completed, na.rm=T)
      expected_crfs_28d = sum(data.biopsied.aki$fu28d_crfs_expected, na.rm=T)
      completed_crfs_28d = sum(data.biopsied.aki$fu28d_crfs_completed, na.rm=T)
      
      expected_visits_3maki = sum(data.biopsied.aki$fu3maki_visit_expected, na.rm=T)
      completed_visits_3maki = sum(data.biopsied.aki$fu3maki_visit_completed, na.rm=T)
      expected_crfs_3maki = sum(data.biopsied.aki$fu3maki_crfs_expected, na.rm=T)
      completed_crfs_3maki = sum(data.biopsied.aki$fu3maki_crfs_completed, na.rm=T)
      
      expected_visits_6m = sum(data.biopsied.aki$fu6m_visit_expected, na.rm=T)
      completed_visits_6m = sum(data.biopsied.aki$fu6m_visit_completed, na.rm=T)
      expected_crfs_6m = sum(data.biopsied.aki$fu6m_crfs_expected, na.rm=T)
      completed_crfs_6m = sum(data.biopsied.aki$fu6m_crfs_completed, na.rm=T)
      
      expected_visits_12m = sum(data.biopsied.aki$fu12m_visit_expected, na.rm=T)
      completed_visits_12m = sum(data.biopsied.aki$fu12m_visit_completed, na.rm=T)
      expected_crfs_12m = sum(data.biopsied.aki$fu12m_crfs_expected, na.rm=T)
      completed_crfs_12m = sum(data.biopsied.aki$fu12m_crfs_completed, na.rm=T)
      
      expected_visits_18m = sum(data.biopsied.aki$fu18m_visit_expected, na.rm=T)
      completed_visits_18m = sum(data.biopsied.aki$fu18m_visit_completed, na.rm=T)
      expected_crfs_18m = sum(data.biopsied.aki$fu18m_crfs_expected, na.rm=T)
      completed_crfs_18m = sum(data.biopsied.aki$fu18m_crfs_completed, na.rm=T)

      expected_visits = sum(expected_visits_24h, expected_visits_7daki, expected_visits_14d, expected_visits_28d, expected_visits_3maki, expected_visits_6m, expected_visits_12m, expected_visits_18m)
      completed_visits = sum(completed_visits_24h, completed_visits_7daki, completed_visits_14d, completed_visits_28d, completed_visits_3maki, completed_visits_6m, completed_visits_12m, completed_visits_18m)
      expected_crfs = sum(expected_crfs_enrollment, expected_crfs_biopsy, expected_crfs_24h, expected_crfs_7daki, expected_crfs_14d, expected_crfs_28d, expected_crfs_3maki, expected_crfs_6m, expected_crfs_12m, expected_crfs_18m)
      completed_crfs = sum(completed_crfs_enrollment, completed_crfs_biopsy, completed_crfs_24h, completed_crfs_7daki, completed_crfs_14d, completed_crfs_28d, completed_crfs_3maki, completed_crfs_6m, completed_crfs_12m, completed_crfs_18m)
      
      expected_pes_28d = expected_visits_28d
      completed_pes_28d = sum(data.biopsied.aki$participant_experience_survey_28d_complete==2, na.rm=T)
      
      expected_pes_6m = expected_visits_6m
      completed_pes_6m = sum(data.biopsied.aki$participant_experience_survey_6m_complete==2, na.rm=T)
      
      expected_return_of_bp_1m = sum(data.biopsied.aki$return_of_bp_1m_expected, na.rm=T)
      completed_return_of_bp_1m = sum(data.biopsied.aki$return_of_bp_1m_completed, na.rm=T)
      
    }
  }
  
  if(type == "ckd"){
    return(list(expected_stats = c(expected_visits, expected_crfs, expected_pes_28d, expected_pes_6m, 
                                   expected_crfs_enrollment, expected_crfs_biopsy, expected_crfs_24h, expected_crfs_14d,
                                   expected_crfs_28d, expected_crfs_6m, expected_crfs_12m, expected_crfs_18m,
                                   expected_return_of_bp_1m),
                completed_stats = c(completed_visits, completed_crfs, completed_pes_28d, completed_pes_6m, 
                                    completed_crfs_enrollment, completed_crfs_biopsy, completed_crfs_24h, completed_crfs_14d,
                                    completed_crfs_28d, completed_crfs_6m, completed_crfs_12m, completed_crfs_18m,
                                    completed_return_of_bp_1m)))
  } else{
    return(list(expected_stats = c(expected_visits, expected_crfs, expected_pes_28d, expected_pes_6m, 
                                   expected_crfs_enrollment, expected_crfs_biopsy, expected_crfs_24h, expected_crfs_7daki, expected_crfs_14d,
                                   expected_crfs_28d, expected_crfs_3maki, expected_crfs_6m, expected_crfs_12m, expected_crfs_18m,
                                   expected_return_of_bp_1m),
                completed_stats = c(completed_visits, completed_crfs, completed_pes_28d, completed_pes_6m, 
                                    completed_crfs_enrollment, completed_crfs_biopsy, completed_crfs_24h, completed_crfs_7daki, completed_crfs_14d,
                                    completed_crfs_28d, completed_crfs_3maki, completed_crfs_6m, completed_crfs_12m, completed_crfs_18m,
                                    completed_return_of_bp_1m)))
  }
}


summarise_visits_crfs_each = function(data, site = "brigham", type = "ckd",
                                      current_quarter = c(as.Date("2021-03-02"),as.Date("2021-06-04")),
                                      previous_quarter = c(as.Date("2020-12-01"), as.Date("2021-03-01"))){
  
  data_site = data %>% filter(redcap_data_access_group %in% site)
  
  current_stats = calc_visits_crfs(data_site, type = type, quarter = current_quarter, cum = F)
  previous_stats = calc_visits_crfs(data_site, type = type, quarter = previous_quarter, cum = F)
  cumulative_stats = calc_visits_crfs(data_site, type = type, cum = T)
  
  current_pct_num = round(current_stats$completed_stats / current_stats$expected_stats * 100)
  previous_pct_num = round(previous_stats$completed_stats / previous_stats$expected_stats * 100)
  cumulative_pct_num = round(cumulative_stats$completed_stats / cumulative_stats$expected_stats * 100)
  
  change_images = calc_change(current_pct_num, previous_pct_num)
  bgc = list(current = sapply(current_pct_num, color_pct),
             previous = sapply(previous_pct_num, color_pct))
  
  current = data.frame(completed = current_stats$completed_stats,
                       expected = current_stats$expected_stats,
                       pct = current_pct_num)
  
  previous = data.frame(completed = previous_stats$completed_stats,
                        expected = previous_stats$expected_stats,
                        pct = previous_pct_num)
  
  cumulative = data.frame(completed = cumulative_stats$completed_stats,
                          expected = cumulative_stats$expected_stats,
                          pct = cumulative_pct_num)
  
  current_chrc = apply(current, 1, function(x) format_pct_rst(x[1], x[2], x[3]))
  previous_chrc = apply(previous, 1, function(x) format_pct_rst(x[1], x[2], x[3]))
  cumulative_chrc = apply(cumulative, 1, function(x) format_pct_rst(x[1], x[2], x[3]))
  
  if(type == "ckd"){
    
    summarised_kb = data.frame(
      site = c(paste0("Visits \\& CRFs (Completed/Expected",footnote_marker_alphabet(1, "latex")," (\\%))"),
               paste0("Visits", footnote_marker_alphabet(2, "latex")), paste0("CRFs", footnote_marker_alphabet(3, "latex")),
               "28d Participant Experience Survey", paste0("6m Participant Experience Survey", footnote_marker_alphabet(4, "latex")),
               "Enrollment (13)","Biopsy Visit (8)","24h Follow-up (1)","14d Follow-up (1)","28d Follow-up (2)",
               "6m Phone Visit (4)","12m Visit (8)", "18m Phone Visit (2)",
               "Return of Biopsy Results within 1 Month"),
      current = c("", current_chrc),
      previous = c("", previous_chrc),
      change = "",
      cumulative = c("", cumulative_chrc)
    )
    
  } else{
    
    summarised_kb = data.frame(
      site = c(paste0("Visits \\& CRFs (Completed/Expected",footnote_marker_alphabet(1, "latex")," (\\%))"),
               paste0("Visits", footnote_marker_alphabet(2, "latex")), paste0("CRFs", footnote_marker_alphabet(3, "latex")),
               "28d Participant Experience Survey", paste0("6m Participant Experience Survey", footnote_marker_alphabet(4, "latex")),
               "Enrollment (11)","Biopsy Visit (8)","24h Follow-up (1)","AKI-only (up to 7 days) (5)","14d Follow-up (1)","28d Follow-up (2)",
               paste0("3m AKI Visit", footnote_marker_alphabet(6, "latex"), " (7)"),"6m Phone Visit (4)","12m Visit (8)", "18m Phone Visit (2)",
               "Return of Biopsy Results within 1 Month"),
      current = c("", current_chrc),
      previous = c("", previous_chrc),
      change = "",
      cumulative = c("", cumulative_chrc)
    )
    
  }
  
  
  return(list(kb = summarised_kb,
              cum_stats.vct = cumulative_pct_num,
              change = change_images,
              bgc = bgc))
}

summarise_crfs_all_sites_by_type = function(data, type = "ckd"){
  
  if(type == "ckd"){
    sites = c("brigham", "ccf", "joslin", "utsw")
  } else{
    sites = c("columbia", "jhmi", "upmc", "yale")
  }
  
  sum_site1 = summarise_visits_crfs_each(data, site = sites[1], type = type)$cum_stats.vct
  sum_site2 = summarise_visits_crfs_each(data, site = sites[2], type = type)$cum_stats.vct
  sum_site3 = summarise_visits_crfs_each(data, site = sites[3], type = type)$cum_stats.vct
  sum_site4 = summarise_visits_crfs_each(data, site = sites[4], type = type)$cum_stats.vct
  
  sum = data.frame(site1 = sum_site1,
                   site2 = sum_site2,
                   site3 = sum_site3,
                   site4 = sum_site4)
  
  all_sites = calc_visits_crfs(data, type = type, cum = T)
  all_sites_pct = round(all_sites$completed_stats / all_sites$expected_stats * 100)
  
  all_min_max = data.frame(all = all_sites_pct,
                           min = apply(sum, 1, min, na.rm=T),
                           max = apply(sum, 1, max, na.rm=T))
  
  all_rst = apply(all_min_max, 1, function(x){
    paste0(x[1], "\\% (", x[2], "\\%-", x[3], "\\%)")
  })
  
  return(all_rst)
}

format_crfs_kable = function(data, site = "brigham", type = "ckd",
                             current_quarter = c(as.Date("2021-03-02"),as.Date("2021-06-04")),
                             previous_quarter = c(as.Date("2020-12-01"), as.Date("2021-03-01"))){
  
  sum_each = summarise_visits_crfs_each(data = data, site = site, type = type,
                                        current_quarter = current_quarter, previous_quarter = previous_quarter)
  
  sum_all_sites = summarise_crfs_all_sites_by_type(data = data, type = type)
  
  kb = sum_each$kb %>%
    mutate(all = linebreak(c("", sum_all_sites), align="c"))
  
  return(list(kb = kb,
              change = sum_each$change,
              bgc = sum_each$bgc))
}

save.image(file = "visitsfns.RData")
