library(Hmisc)
library(kableExtra)
library(dplyr)
library(tidyverse)

rm(list=ls())

ckd_sites = c("brigham", "ccf", "joslin", "utsw")
aki_sites = c("columbia", "jhmi", "upmc", "yale")

current_quarter = c(as.Date("2021-06-05"), as.Date("2021-09-01"))
previous_quarter = c(as.Date("2021-03-02"), as.Date("2021-06-04"))

goal_brigham = c(3, 20-3*3)
goal_ccf = c(5, 40-5*3) 
goal_joslin = c(3, 20-3*3) 
goal_utsw = c(5, 40-5*3)

goal_columbia = c(3, 25-3*3)
goal_jhmi = c(2, 15-2*3)
goal_upmc = c(3, 25-3*3)
goal_yale = c(2, 15-2*3)

calc_change = function(current_stats, previous_stats, higher_better = T){
  
  df = data.frame(current = current_stats,
                  previous = previous_stats)
  
  if(higher_better == T){
    change_images <- apply(df, 1, function(x){
      if(is.na(x[1]) | is.na(x[2]) | x[1]=="" | x[2]==""){
        "images/empty.png"
      } else if(as.numeric(x[1]) > as.numeric(x[2])){
        "images/arrow_up.png"
      } else if(as.numeric(x[1]) < as.numeric(x[2])){
        "images/arrow_down.png"
      } else{
        "images/empty.png"
      }
    })
  } else{
    change_images <- apply(df, 1, function(x){
      if(is.na(x[1]) | is.na(x[2]) | x[1]=="" | x[2]==""){
        "images/empty.png"
      } else if(as.numeric(x[1]) > as.numeric(x[2])){
        "images/arrow_up_red.png"
      } else if(as.numeric(x[1]) < as.numeric(x[2])){
        "images/arrow_down_green.png"
      } else{
        "images/empty.png"
      }
    })
  }

  return(change_images)
}

format_pct_rst = function(completed, expected, pct){
  if(expected == 0){
    return("0/0")
  } else{
    return(paste0(completed, "/", expected, " (", pct, "\\%)"))
  }
}

color_pct = function(pct){
  if(is.na(pct)){
    background = "white"
  } else if(pct >= 90){
    background = "#99e5bc"
  } else if(pct >= 70){
    background = "#ffff99"
  } else{
    background = "#ff999a"
  }
  
  return(background)
}

color_dist = data.frame(color = cell_spec("", 
                                          background = c("#99e5bc", "#ffff99", "#ff999a")),
                        dist = c(">=90\\%", "70-<90\\%", "<70\\%"))
colnames(color_dist) = NULL

save.image(file = "setup_basicfns.RData")