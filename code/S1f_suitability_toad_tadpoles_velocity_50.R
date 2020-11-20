### merge results
library(tidyverse)
library(tidyr)
### work flow
## upload time stats
## converge all nodes per species
## converge all species
## calculate suitability per node position
## calculate suitability per node (if 1 position suitable, node is suitable)

# setwd("/Users/katieirving/Documents/git/SOC_tier_3/output_data")
## water year types 

getwd()
### santa ana sucker
## velocity

## upload all time stats csvs

## time stats
ts <- list.files("output_data/", pattern="time_stats")
length(ts) ## 219
ts
ts <- Filter(function(x) grepl("Velocity", x), ts)
ts <- Filter(function(x) grepl("Tadpole", x), ts)
ts <- Filter(function(x) grepl("Toad", x), ts)

time_statsx <- NULL
j=1
j
for(j in 1: length(ts)) {
  
  
  time_stats <- read.csv(file=paste("output_data/", ts[j], sep=""))
  
  
  ######## juvenile
  time_stats <- time_stats %>% 
    select(-X) %>%
    # filter(Probability_Threshold == ann_stats) %>%
    rename(TimePeriod = season, TimePercentage = value, TimePeriod2 = Probability_Threshold) %>%
    # mutate(Species = "Willow", Life_Stage = "Seedling", Hydraulic = "Depth", Node = paste(stringx[3])) %>%
    distinct()
  
  time_statsx <- rbind(time_statsx, time_stats)
  
}

head(time_stats)
unique(time_statsx$TimePeriod2)
## change time period to seasonal 

time_stats_seas <- time_statsx %>%
  filter(TimePeriod == "critical") %>%
  select(-TimePeriod) %>%
  distinct()
head(time_stats_seas)

## calculate suitability

time_stats_seas <- time_stats_seas %>%
  mutate(Suitability_Class = NA)
# group_by(Node, position, Species, Life_Stage, water_year) %>%

probs <- seq(1, dim(time_stats_seas)[1], 1)  

for(p in 1: length(probs)) {
  
  time_stats_seas$Suitability_Class[p] = if(time_stats_seas$TimePercentage[p] >= 50) {
    paste("High")
  } else  if(time_stats_seas$TimePercentage[p] >= 25 & time_stats_seas$TimePercentage[p] <= 50 ){
    paste("Partial")
  } else  if(time_stats_seas$TimePercentage[p] < 25){
    paste("Low")
  } else {
    paste("Partial")
  }
  
}
time_stats_seas


head(time_stats_seas)


## join back together and save
time_stats_all <-time_stats_seas
write.csv(time_stats_all, "/Users/katieirving/Documents/git/SOC_tier_3/output_data/results/Toad_Tadpoles_Velocity_time_stats_50.csv")

# Days per month ----------------------------------------------------------


### days per month
td <- list.files("output_data/", pattern="total_days")
length(td) ## 153

td <- Filter(function(x) grepl("Velocity", x), td)
td <- Filter(function(x) grepl("Tadpole", x), td)
td <- Filter(function(x) grepl("Toad", x), td)

td

total_daysx <- NULL


for(j in 1: length(td)) {
  
  
  total_days <- read.csv(file=paste("output_data/", td[j], sep=""))
  
  ######## juvenile
  total_days <- total_days %>% 
    select(-X,-Probability_Threshold) %>%
    # filter(Probability_Threshold == ann_stats) %>%
    rename(TimePeriod = season,  DaysPerMonth = n_days) %>%
    # mutate(Species = "Willow", Life_Stage = "Seedling", Hydraulic = "Depth", Node = paste(stringx[2])) %>%
    distinct()
  
  total_daysx <- rbind(total_daysx, total_days)
  
}

head(total_days)


## change time period to seasonal and add bottom and water year type

total_days_seas <- total_daysx 

total_days_seas <- total_days_seas %>%
  mutate(Suitability_Class = NA)

probs <- seq(1, dim(total_days_seas)[1], 1)  

for(p in 1: length(probs)) {
  
  total_days_seas$Suitability_Class[p] = if(total_days_seas$DaysPerMonth[p] >= 14) {
    paste("High")
  } else  if(total_days_seas$DaysPerMonth[p] >= 7 & total_days_seas$DaysPerMonth[p] <= 14 ){
    paste("Partial")
  } else  if(total_days_seas$DaysPerMonth[p] < 7){
    paste("Low")
  } else {
    paste("Partial")
  }
  
}
total_days_seas
### bind together and save
total_days_all <- total_days_seas
write.csv(total_days_all,"/Users/katieirving/Documents/git/SOC_tier_3/output_data/results/Toad_Tadpoles_Velocity_total_days_50.csv")

total_days_all


