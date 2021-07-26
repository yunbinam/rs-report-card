library(Hmisc)
library(kableExtra)
library(dplyr)
library(tidyverse)

rm(list=ls())

load("setup_basicfns.RData")

calc_rct = function(data, start = as.Date("2021-03-02"), end = as.Date("2021-06-04"), cum = FALSE){
  
  if(cum == FALSE){ # calculate stats for a quarter
    # use date of assessment
    data_egb = data %>% filter(as.Date(sc_date) >= as.Date(start) & as.Date(sc_date) <= as.Date(end))
    eac = sum(data_egb$eac == 1)
    eligible = sum(data_egb$eligible == 1)
    
    # use date consent was signed
    data_consent = data %>% filter(as.Date(consent_date) >= as.Date(start) & as.Date(consent_date) <= as.Date(end))
    enrolled = sum(data_consent$enrolled == 1)
    
    # use biopsy date
    data_bp = data %>% filter(as.Date(bp_date) >= as.Date(start) & as.Date(bp_date) <= as.Date(end))
    biopsied = sum(data_bp$biopsied == 1)
    
    # use date participant withdrew from study
    data_withdraw = data %>% filter(!is.na(withdraw_date) & as.Date(withdraw_date) >= as.Date(start) & 
                                      as.Date(withdraw_date) <= as.Date(end))
    withdraw_prb = sum(data_withdraw$withdraw_prb == 1)
    withdraw_pb = sum(data_withdraw$withdraw_pb == 1)
    
    # use date of death
    data_death = data %>% filter(!is.na(date_death) & as.Date(date_death) >= as.Date(start) & 
                                   as.Date(date_death) <= as.Date(end))
    death_prb = sum(data_death$death_prb == 1)
    death_pb = sum(data_death$death_pb == 1)
    
    # use date participant was lost to follow-up
    data_lfu = data %>% filter(!is.na(lfu_date) & as.Date(lfu_date) >= as.Date(start) &
                                 as.Date(lfu_date) <= as.Date(end))
    lfu_prb = sum(data_lfu$lfu_prb == 1)
    lfu_pb=sum(data_lfu$lfu_pb == 1)
    
    # use date participant removed from study due to loss of eligibility
    data_loe = data %>% filter(!is.na(loe_date) & as.Date(loe_date) >= as.Date(start) &
                                 as.Date(loe_date) <= as.Date(end))
    loe_prb = sum(data_loe$loe_prb == 1)
    
  } else{ # calculate stats for cumulative period
    eac = sum(data$eac == 1)
    eligible = sum(data$eligible == 1)
    enrolled = sum(data$enrolled == 1)
    biopsied = sum(data$biopsied == 1)
    
    withdraw_prb = sum(data$withdraw_prb == 1)
    withdraw_pb = sum(data$withdraw_pb == 1)
    death_prb = sum(data$death_prb == 1)
    death_pb = sum(data$death_pb == 1)
    lfu_prb = sum(data$lfu_prb == 1)
    lfu_pb=sum(data$lfu_pb == 1)
    loe_prb = sum(data$loe_prb == 1)
  }
  
  return(list(rct_stats = c(eac, eligible, enrolled, biopsied),
              not_followed_stats = c(withdraw_prb, death_prb, lfu_prb, loe_prb, withdraw_pb, death_pb, lfu_pb)))
}

calc_goal_achv = function(data, goal = 3, start = as.Date("2021-03-02"), end = as.Date("2021-06-04"), cum = FALSE){
  
  if(cum == FALSE){ # calculate % of goal for a quarter
    data_bp = data %>% filter(as.Date(bp_date) >= as.Date(start) & as.Date(bp_date) <= as.Date(end))
    biopsied = sum(data_bp$biopsied == 1)
  } else{ # calculate % of goal for cumulative period
    biopsied = sum(data$biopsied == 1)
  }
  
  achv = round(biopsied/goal * 100, 0)
  achv_color = color_pct(achv)
  
  return(list(rst = format_pct_rst(biopsied, goal, achv),
              num = achv,
              bgc = achv_color))
}

calc_retention = function(data, start = as.Date("2021-03-02"), end = as.Date("2021-06-04"), cum = FALSE){
  
  if(cum == FALSE){ # calculate % of goal for a quarter
    data_bp = data %>% filter(as.Date(bp_date) >= as.Date(start) & as.Date(bp_date) <= as.Date(end))
    biopsied = sum(data_bp$biopsied == 1)
    not_followed = with(data_bp, sum(withdraw_pb, death_pb, lfu_pb))
  } else{ # calculate % of goal for cumulative period
    biopsied = sum(data$biopsied == 1)
    not_followed = with(data, sum(withdraw_pb, death_pb, lfu_pb))
  }
  
  retention = round(100 - (not_followed/biopsied * 100), 0)
  retention_color = color_pct(retention)
  
  return(list(rst = format_pct_rst(biopsied-not_followed, biopsied, retention),
              num = retention,
              bgc = retention_color))
}

summarise_rct_each = function(data, site = "brigham", 
                              current_quarter = c(as.Date("2021-03-02"),as.Date("2021-06-04")),
                              previous_quarter = c(as.Date("2020-12-01"), as.Date("2021-03-01")),
                              goal = c(3, 20-3*4)){
  
  data_site = data %>% filter(redcap_data_access_group %in% site)
  
  current_rct = calc_rct(data_site, start = current_quarter[1], end = current_quarter[2], cum = F)
  previous_rct = calc_rct(data_site, start = previous_quarter[1], end = previous_quarter[2], cum = F)
  cumulative_rct = calc_rct(data_site, cum = T)
  
  current_goal_achv = calc_goal_achv(data_site, goal = goal[1], start = current_quarter[1], end = current_quarter[2], cum = F)
  previous_goal_achv = calc_goal_achv(data_site, goal = goal[1], start = previous_quarter[1], end = previous_quarter[2], cum = F)
  cumulative_goal_achv = calc_goal_achv(data_site, goal = goal[2], cum = T)
  
  current_retention = calc_retention(data_site, start = current_quarter[1], end = current_quarter[2], cum = F)
  previous_retention = calc_retention(data_site, start = previous_quarter[1], end = previous_quarter[2], cum = F)
  cumulative_retention = calc_retention(data_site, cum = T)
  
  current_stats.vector = c(current_rct[["rct_stats"]], current_goal_achv[["num"]], current_rct[["not_followed_stats"]], current_retention[["num"]])
  previous_stats.vector = c(previous_rct[["rct_stats"]], previous_goal_achv[["num"]], previous_rct[["not_followed_stats"]], previous_retention[["num"]])
  cumulative_stats.vector = c(cumulative_rct[["rct_stats"]], cumulative_goal_achv[["num"]], cumulative_rct[["not_followed_stats"]], cumulative_retention[["num"]])
  
  change_images = c(calc_change(current_stats.vector[1:5], previous_stats.vector[1:5]),
                    calc_change(current_stats.vector[6:12], previous_stats.vector[6:12], higher_better = FALSE),
                    calc_change(current_stats.vector[13], previous_stats.vector[13]))
  bgc = list(bgc_goal = c(current_goal_achv[["bgc"]], previous_goal_achv[["bgc"]]),
             bgc_retention = c(current_retention[["bgc"]], previous_retention[["bgc"]]))
  
  current_stats.chrc = c(current_rct[["rct_stats"]], current_goal_achv[["rst"]], current_rct[["not_followed_stats"]], current_retention[["rst"]])
  previous_stats.chrc = c(previous_rct[["rct_stats"]], previous_goal_achv[["rst"]], previous_rct[["not_followed_stats"]], previous_retention[["rst"]])
  cumulative_stats.chrc = c(cumulative_rct[["rct_stats"]], cumulative_goal_achv[["rst"]], cumulative_rct[["not_followed_stats"]], cumulative_retention[["rst"]])
  
  summarised_kb = data.frame(
    site = c("Recruitment \\& retention", "Eligibility Assessment Complete (N)", "Eligible (N)", "Enrolled (N)", "Biopsied (N)",
             "\\% of goal (Biopsied/Goal (\\%))", "Withdrew", "Deceased", "Lost to Follow-up", "Loss of Eligibility", 
             "Withdrew", "Deceased", "Lost to Follow-up", paste0("Retention", footnote_marker_number(1, "latex"), " (\\%)")),
    current = c("", current_stats.chrc),
    previous = c("", previous_stats.chrc),
    change = "",
    cumulative = c("", cumulative_stats.chrc)
  )
  
  return(list(kb = summarised_kb,
              cum_stats.vct = cumulative_stats.vector,
              change = change_images,
              bgc = bgc))
}

summarise_all_sites = function(sum_site1, sum_site2, sum_site3, sum_site4, goal_all_sites = (20-3*4)*2 + (40-5*4)*2){
  
  df = data.frame(site1 = sum_site1,
                  site2 = sum_site2,
                  site3 = sum_site3,
                  site4 = sum_site4)
  
  summarise_n_all_sites = function(x){
    return(paste0(sum(x), " (", min(x), "-", max(x), ")"))
  }
  
  biopsied_all_sites = sum(df[4,])
  notfollowed_pb_all_sites = sum(df[10:12,])
  goal_achv_all_sites = round(100 * biopsied_all_sites/goal_all_sites, 0)
  retention_all_sites = round(100 - (notfollowed_pb_all_sites/biopsied_all_sites*100), 0)
  
  summarise_pct_all_sites = function(x, pct_all_sites){
    return(paste0(pct_all_sites, "\\% (", min(x), "\\%-", max(x), "\\%)"))
  }
  
  rct = apply(df[1:4,], 1, FUN = summarise_n_all_sites)
  goal = summarise_pct_all_sites(df[5,], goal_achv_all_sites)
  not_followed = apply(df[6:12,], 1, FUN = summarise_n_all_sites)
  retention = summarise_pct_all_sites(df[13,], retention_all_sites)
  
  return(c(rct, goal, not_followed, retention))
}

summarise_all_sites_by_type = function(data, type = "ckd", goal1=c(3, 20-3*4), goal2=c(5, 40-5*4), goal3=c(3, 20-3*4), goal4=c(5, 40-5*4)){
  if(type == "ckd"){
    sites = c("brigham", "ccf", "joslin", "utsw")
  } else{
    sites = c("columbia", "jhmi", "upmc", "yale")
  }
  
  goal_all_sites = goal1[2] + goal2[2] + goal3[2] + goal4[2]
  sum_site1 = summarise_rct_each(data, sites[1], goal = goal1)$cum_stats.vct
  sum_site2 = summarise_rct_each(data, sites[2], goal = goal2)$cum_stats.vct
  sum_site3 = summarise_rct_each(data, sites[3], goal = goal3)$cum_stats.vct
  sum_site4 = summarise_rct_each(data, sites[4], goal = goal4)$cum_stats.vct
  
  sum_all_sites = summarise_all_sites(sum_site1, sum_site2, sum_site3, sum_site4, goal_all_sites = goal_all_sites)
  
  return(sum_all_sites)
}

format_rct_kable = function(data, site = "brigham", type = "ckd",
                            current_quarter = c(as.Date("2021-03-02"),as.Date("2021-06-04")),
                            previous_quarter = c(as.Date("2020-12-01"), as.Date("2021-03-01")),
                            goal = c(3, 20-3*4),
                            goal1=c(3, 20-3*4), goal2=c(5, 40-5*4), goal3=c(3, 20-3*4), goal4=c(5, 40-5*4)){
  
  sum_each = summarise_rct_each(data = data, site = site, 
                                current_quarter = current_quarter, 
                                previous_quarter = previous_quarter,
                                goal = goal)
  
  sum_all_sites = summarise_all_sites_by_type(data = data, type = type, 
                                              goal1 = goal1, goal2 = goal2, goal3 = goal3, goal4 = goal4)
  
  kb = sum_each$kb %>%
    mutate(all = linebreak(c("", sum_all_sites), align="c"))
  
  return(list(kb = kb,
              change = sum_each$change,
              bgc = sum_each$bgc))
}

save.image(file = "rctfns.RData")