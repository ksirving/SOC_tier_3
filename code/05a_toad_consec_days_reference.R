## toad model

## simplistic model to get a probability of occurrence based on 
## the likelyhood of 80 days of flow

## packages

library(tidyverse)
library(tidyr)
library(sm)
library(lubridate) # work with dates
library(dplyr)     # data manipulation (filter, summarize, mutate)
library(ggplot2)   # graphics
library(gridExtra) # tile several plots next to each other
library(scales)
library(data.table)
library(gdata)
getwd()


## upload hydraulic data
setwd("/Users/katieirving/Documents/git/SOC_tier_3")

## upload hydraulic data
setwd("input_data/Hydraulics/reference")

h <- list.files(pattern="hydraulic")
length(h) ## 28
h

min_limit <- 0.0 ## define min limit for depth. can be changed

setwd("/Users/katieirving/Documents/git/SOC_tier_3")
n=5
p=1

for(n in 1: length(h)) {
  
  hydraul <- read.csv(file=paste("input_data/Hydraulics/reference/", h[n], sep=""))
  # head(hydraul)
  
  ## define nodename
  NodeName <- str_split(h[n], "_", 3)[[1]]
  NodeName <- NodeName[1]
  # NodeName
  ## change names and add datenum
  
  hyd_dep <- hydraul %>%
    rename(Q = q.cms) %>%
    mutate(date_num = seq(1,length(date), 1))
  
  ## format date time
  hyd_dep$DateTime<-as.POSIXct(hyd_dep$date,
                               format = "%m/%d/%Y",
                               tz = "GMT")
  
  hyd_dep <- hyd_dep %>%
    mutate(month = month(DateTime)) %>%
    mutate(year = year(DateTime)) %>%
    mutate(day = day(DateTime)) %>%
    mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1))
  
  # head(hyd_dep)
  # ## melt channel position data
  
  hyd_dep <- hyd_dep %>%
    select(DateTime, Q, date_num, month, day, water_year, contains("max.depth"))
  
  # transform m to cm
  hyd_dep<-reshape2::melt(hyd_dep, id=c("DateTime","Q",  "date_num", "month", "day", "water_year"))
  hyd_dep <- hyd_dep %>% 
    mutate(depth_cm = as.numeric(as.character(value*100)))
  # head(hyd_dep)
  
  
  
  ## define critical period
  non_critical <- c(1:2,7:12) ## winter months
  critical <- c(3:6) ## summer months
  
  all_data <- hyd_dep %>%
    mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") ) %>%
    separate(variable, into = c("Hydraulic", "Position"), sep="_", remove = F)
  
  head(all_data)
  
  ## define days that have Q> 0 for all hours
  wet_days <- all_data %>%
    group_by(water_year, month, day, Position, season) %>%
    mutate(wet_hour = ifelse(Q > 0.01, 1, 0)) %>%
    summarise(n_wet_hours = sum(wet_hour))  %>%
    mutate(WetDay= ifelse(n_wet_hours == 24, 1, 0)) # %>%
  
  ## define positions
  positions <- unique(wet_days$Position)
  # positions
  
  days_datax <- NULL
  
  for(p in 1:length(positions)) {
    
    ## subset position
    new_data <- wet_days %>% 
      filter(Position  == positions[p])
    
    days_data <- new_data  %>%
      ungroup() %>%
      filter(season == "critical") %>%
      group_by(water_year) %>%
      mutate(consec_days = sequence(rle(as.character(WetDay))$lengths)) %>%
      summarise(wetDaysYear = max(consec_days)) %>%
      mutate(Position = positions[p], season = "critical")
    
    days_datax <- rbind(days_datax, days_data)
    
  } ## end 2nd loop
  
  
  melt_days <- days_datax %>%
    mutate(Species ="Toad", Life_Stage = "Breeding", Hydraulic = "80_days", Node = NodeName)
  
  ## save df
  write.csv(melt_days, paste("output_data/05_", NodeName, "_Toad_Breeding_Q_total_days_long_reference.csv", sep="") )
  
  cat(paste("Finished Node", NodeName))
  
} ## end 1st loop
