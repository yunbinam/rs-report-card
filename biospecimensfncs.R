library(Hmisc)
library(kableExtra)
library(dplyr)
library(tidyverse)

rm(list=ls())

load("setup_basicfns.RData")

calc_biospecimens = function(blood_urine, acd_cbr, shipment, cores_triages, cryostor,
                             quarter = c(as.Date("2021-03-02"), as.Date("2021-06-04")), cum = F){
  
  if(cum == FALSE){
    
    blood_urine_quarter = blood_urine %>%
      filter(as.Date(collection_date) >= quarter[1] & as.Date(collection_date) <= quarter[2])
    
    blood_quarter = blood_urine_quarter %>% filter(blood_samples == 1)
    urine_quarter = blood_urine_quarter %>% filter(spot_urine_samples == 1)
    
    blood_total = nrow(blood_quarter)
    blood_success = sum(blood_quarter$max_time_from_collection_to_frozen_hrs < 2)
    blood_pct = round(blood_success/blood_total * 100)
    
    urine_total = nrow(urine_quarter)
    urine_success = sum(urine_quarter$max_time_from_collection_to_frozen_hrs < 4)
    urine_pct = round(urine_success/urine_total * 100)
    
    acd_cbr_quarter = acd_cbr %>%
      filter(as.Date(acd_created) >= quarter[1] & as.Date(acd_created) <= quarter[2])
    
    acd_cbr_total = nrow(acd_cbr_quarter)
    acd_cbr_success = sum(acd_cbr_quarter$time_from_created_to_received < 24)
    acd_cbr_pct = round(acd_cbr_success/acd_cbr_total * 100)
    
    shipment_quarter = shipment %>%
      filter(as.Date(ship_date) >= quarter[1] & as.Date(ship_date) <= quarter[2])
    
    shipment_total = nrow(shipment_quarter)
    shipment_success = sum(shipment_quarter$issues == 0)
    shipment_pct = round(shipment_success/shipment_total * 100)
    
    cores_triages_quarter = cores_triages %>%
      filter(as.Date(bp_date) >= quarter[1] & as.Date(bp_date) <= quarter[2])
    
    core3_total = sum(cores_triages_quarter$core3 != "0:59:59")
    core3_success = sum(cores_triages_quarter$core3_less5)
    core3_pct = round(core3_success/core3_total * 100)
    
    cores_total = sum(cores_triages_quarter$cores_obtained)
    cores_success = with(cores_triages_quarter, sum(core1_vial1_less10, core1_vial2_less10,
                                                    core1_vial3_less10, core2_vial4_less10,
                                                    core3_less10))
    cores_pct = round(cores_success/cores_total * 100)
    
    cryostor_quarter = cryostor %>%
      filter(as.Date(created_date) >= quarter[1] & as.Date(created_date) <= quarter[2])
    
    cryostor_total = nrow(cryostor_quarter)
    cryostor_success = sum(cryostor_quarter$time_at_rs_days < 7)
    cryostor_pct = round(cryostor_success/cryostor_total * 100)
    
  } else{
    
    blood = blood_urine %>% filter(blood_samples == 1)
    urine = blood_urine %>% filter(spot_urine_samples == 1)
    
    blood_total = nrow(blood)
    blood_success = sum(blood$max_time_from_collection_to_frozen_hrs < 2)
    blood_pct = round(blood_success/blood_total * 100)
    
    urine_total = nrow(urine)
    urine_success = sum(urine$max_time_from_collection_to_frozen_hrs < 4)
    urine_pct = round(urine_success/urine_total * 100)
    
    acd_cbr_total = nrow(acd_cbr)
    acd_cbr_success = sum(acd_cbr$time_from_created_to_received < 24)
    acd_cbr_pct = round(acd_cbr_success/acd_cbr_total * 100)
    
    shipment_total = nrow(shipment)
    shipment_success = sum(shipment$issues == 0)
    shipment_pct = round(shipment_success/shipment_total * 100)
    
    core3_total = sum(cores_triages$core3 != "0:59:59")
    core3_success = sum(cores_triages$core3_less5)
    core3_pct = round(core3_success/core3_total * 100)
    
    cores_total = sum(cores_triages$cores_obtained)
    cores_success = with(cores_triages, sum(core1_vial1_less10, core1_vial2_less10,
                                            core1_vial3_less10, core2_vial4_less10,
                                            core3_less10))
    cores_pct = round(cores_success/cores_total * 100)
    
    cryostor_total = nrow(cryostor)
    cryostor_success = sum(cryostor$time_at_rs_days < 7)
    cryostor_pct = round(cryostor_success/cryostor_total * 100)
  }
  
  return(list(total_stats = c(blood_total, urine_total, acd_cbr_total, shipment_total,
                              core3_total, cores_total, cryostor_total),
              success_stats = c(blood_success, urine_success, acd_cbr_success, shipment_success,
                                core3_success, cores_success, cryostor_success),
              pct_stats = c(blood_pct, urine_pct, acd_cbr_pct, shipment_pct,
                            core3_pct, cores_pct, cryostor_pct)))
}

summarise_biospecimens_each = function(blood_urine, acd_cbr, shipment, cores_triages, cryostor, site = "brigham",
                                       current_quarter = c(as.Date("2021-03-02"),as.Date("2021-06-04")),
                                       previous_quarter = c(as.Date("2020-12-01"), as.Date("2021-03-01"))){
  
  blood_urine_site = blood_urine %>% filter(redcap_data_access_group == site)
  acd_cbr_site = acd_cbr %>% filter(redcap_data_access_group == site)
  shipment_site = shipment %>% filter(redcap_data_access_group == site)
  cores_triages_site = cores_triages %>% filter(redcap_data_access_group == site)
  cryostor_site = cryostor %>% filter(redcap_data_access_group == site)
  
  current_stats = calc_biospecimens(blood_urine_site, acd_cbr_site, shipment_site, cores_triages_site, cryostor_site,
                                    quarter = current_quarter, cum = F)
  previous_stats = calc_biospecimens(blood_urine_site, acd_cbr_site, shipment_site, cores_triages_site, cryostor_site,
                                     quarter = previous_quarter, cum = F)
  cumulative_stats = calc_biospecimens(blood_urine_site, acd_cbr_site, shipment_site, cores_triages_site, cryostor_site,
                                       cum = T)
  
  current_pct_num = current_stats$pct_stats
  previous_pct_num = previous_stats$pct_stats
  cumulative_pct_num = cumulative_stats$pct_stats
  
  change_images = calc_change(current_pct_num, previous_pct_num)
  bgc = list(current = sapply(current_pct_num, color_pct),
             previous = sapply(previous_pct_num, color_pct))
  
  current = data.frame(completed = current_stats$success_stats,
                       expected = current_stats$total_stats,
                       pct = current_pct_num)
  
  previous = data.frame(completed = previous_stats$success_stats,
                        expected = previous_stats$total_stats,
                        pct = previous_pct_num)
  
  cumulative = data.frame(completed = cumulative_stats$success_stats,
                          expected = cumulative_stats$total_stats,
                          pct = cumulative_pct_num)
  
  current_chrc = apply(current, 1, function(x) format_pct_rst(x[1], x[2], x[3]))
  previous_chrc = apply(previous, 1, function(x) format_pct_rst(x[1], x[2], x[3]))
  cumulative_chrc = apply(cumulative, 1, function(x) format_pct_rst(x[1], x[2], x[3]))
  
  if(site == "joslin"){
    cumulative_chrc_freeze = "15/15 (100\\%)"
  } else if(site == "brigham"){
    cumulative_chrc_freeze = "5/6 (83\\%)"
  } else if(site == "utsw"){
    cumulative_chrc_freeze = "22/22 (100\\%)"
  } else if(site == "jhmi"){
    cumulative_chrc_freeze = "8/8 (100\\%)"
  } else if(site == "ccf"){
    cumulative_chrc_freeze = "18/19 (95\\%)"
  } else if(site == "columbia"){
    cumulative_chrc_freeze = "7/7 (100\\%)"
  } else if(site == "upmc"){
    cumulative_chrc_freeze = "5/5 (100\\%)"
  } else if(site == "yale"){
    cumulative_chrc_freeze = "4/5 (80\\%)"
  }
  
  summarised_kb = data.frame(
    site=c("Biospecimens",
           "Blood Samples Frozen within 2 Hours/Total Blood Samples Collected (\\%)",
           "Spot Urine Samples Frozen within 4 Hours/Total Spot Urine Samples Collected (\\%)",
           "ACD Received at CBR within 24 Hours of Collection/Total ACD Collected (\\%)",
           paste0("Number of Shipments without Issues", footnote_marker_symbol(1, "latex"), "/Total Number of Shipments ", "(\\%)"),
           "Core 3 Triaged <5 Minutes after Harvest/Total Number of Core 3 Obtained (\\%)",
           "Number of Core Components Triaged <10 Minutes after Harvest/Total Number of Core Components Obtained (\\%)",
           "Cryostor Shipped to CBR within 1 Week of Biopsy/Total Number of Kit A Biopsies (\\%)",
           "Biopsy Cases without Freeze Artifacts Reported by TIS/Total Number of OCT Samples Obtained (\\%)"),
    current = c("", current_chrc, ""),
    previous = c("", previous_chrc, ""),
    change = "",
    cumulative = c("", cumulative_chrc, cumulative_chrc_freeze)
  )
  
  return(list(kb = summarised_kb,
              cum_stats.vct = cumulative_pct_num,
              change = change_images,
              bgc = bgc))
}

summarise_biospecimens_all_sites_by_type = function(blood_urine, acd_cbr, shipment, cores_triages, cryostor, 
                                                    type = "ckd"){
  if(type == "ckd"){
    sites = c("brigham", "ccf", "joslin", "utsw")
    
    blood_urine_sites = blood_urine %>% filter(redcap_data_access_group %in% sites)
    acd_cbr_sites = acd_cbr %>% filter(redcap_data_access_group %in% sites)
    shipment_sites = shipment %>% filter(redcap_data_access_group %in% sites)
    cores_triages_sites = cores_triages %>% filter(redcap_data_access_group %in% sites)
    cryostor_sites = cryostor %>% filter(redcap_data_access_group %in% sites)
    
  } else{
    sites = c("columbia", "jhmi", "upmc", "yale")
    
    blood_urine_sites = blood_urine %>% filter(redcap_data_access_group %in% sites)
    acd_cbr_sites = acd_cbr %>% filter(redcap_data_access_group %in% sites)
    shipment_sites = shipment %>% filter(redcap_data_access_group %in% sites)
    cores_triages_sites = cores_triages %>% filter(redcap_data_access_group %in% sites)
    cryostor_sites = cryostor %>% filter(redcap_data_access_group %in% sites)
    
  }
  
  sum_site1 = summarise_biospecimens_each(blood_urine, acd_cbr, shipment, cores_triages, cryostor, site = sites[1])$cum_stats.vct
  sum_site2 = summarise_biospecimens_each(blood_urine, acd_cbr, shipment, cores_triages, cryostor, site = sites[2])$cum_stats.vct
  sum_site3 = summarise_biospecimens_each(blood_urine, acd_cbr, shipment, cores_triages, cryostor, site = sites[3])$cum_stats.vct
  sum_site4 = summarise_biospecimens_each(blood_urine, acd_cbr, shipment, cores_triages, cryostor, site = sites[4])$cum_stats.vct

  sum_sites = data.frame(site1 = sum_site1,
                         site2 = sum_site2,
                         site3 = sum_site3,
                         site4 = sum_site4)  
  
  all_sites_pct = calc_biospecimens(blood_urine_sites, acd_cbr_sites, shipment_sites, cores_triages_sites, cryostor_sites, cum = T)$pct_stats
  
  all_min_max = data.frame(all = all_sites_pct,
                           min = apply(sum_sites, 1, min),
                           max = apply(sum_sites, 1, max))
  
  all_rst = apply(all_min_max, 1, function(x){
    paste0(x[1], "\\% (", x[2], "\\%-", x[3], "\\%)")
  })
  
  if(type == "ckd"){
    all_rst = c(all_rst, "97\\% (83\\%-100\\%)")
  } else{
    all_rst = c(all_rst, "96\\% (80\\%-100\\%)")
  }
  
  return(all_rst)
}

format_biospecimens_kable = function(blood_urine, acd_cbr, shipment, cores_triages, cryostor, 
                                     site = "brigham", type = "ckd",
                                     current_quarter = c(as.Date("2021-03-02"),as.Date("2021-06-04")),
                                     previous_quarter = c(as.Date("2020-12-01"), as.Date("2021-03-01"))){
  
  sum_each = summarise_biospecimens_each(blood_urine, acd_cbr, shipment, cores_triages, cryostor, site = site,
                                         current_quarter = current_quarter, previous_quarter = previous_quarter)
  
  sum_all_sites = summarise_biospecimens_all_sites_by_type(blood_urine, acd_cbr, shipment, cores_triages, cryostor, 
                                                           type = type)
  
  kb = sum_each$kb %>%
    mutate(all = c("", sum_all_sites))
  
  return(list(kb = kb,
              change = sum_each$change,
              bgc = sum_each$bgc))
}

save.image(file = "biospecimensfncs.RData")