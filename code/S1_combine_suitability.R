### combine for overall 
library(tidyr)
library(tidyverse)
getwd()
## upload all species suitability data

ts <- list.files("output_data/results", pattern="time_stats")
ts
ts <- ts[-c(15, 16, 17, 18, 19)]
all_data <- NULL
s
## first 3 dfs have different format than 2nd 3, combine in sections 
for(s in 1: length(ts)) {
  
  time_stats <- read.csv(file=paste("output_data/results/", ts[s], sep=""))
  
  all_data <- rbind(all_data, time_stats)
  
}

## 15, 
all_data_first <- all_data

head(all_data)
head(time_stats)
ts <- list.files("output_data/results", pattern="time_stats")
ts
ts <- ts[c(15, 16, 17, 18, 19)]
all_data <- NULL

for(s in 1: length(ts)) {
  
  time_stats <- read.csv(file=paste("output_data/results/", ts[s], sep=""))
  
  all_data <- rbind(all_data, time_stats)
  
}

## reformat and combine the 2 dfs together 

head(all_data)
head(all_data_first)

## reformat
all_data_first <- all_data_first %>%
  rename(TimePeriod = TimePeriod2) %>% 
  select(TimePeriod, position, water_year, Node, TimePercentage:Suitability_Class)

all_data <- all_data %>%
  select(-X)
  

all_datax <- rbind(all_data_first, all_data)

write.csv(all_datax, "output_data/results/S1_all_suitability_all_years.csv")
head(all_datax)

time_stats <- all_datax  %>%
  select(Species, Life_Stage, Node,Hydraulic, water_year,TimePeriod, position,
         Suitability_Class) %>%
  distinct()


SuitClassOverYears <- time_stats %>%
  group_by(Species, Life_Stage, Hydraulic, TimePeriod, position, Node) %>%
  summarise(Overall_Class = tail(names(sort(table(Suitability_Class))), 1))

SuitClassOverYears


write.csv(SuitClassOverYears, "output_data/results/S1_all_suitability_combined_years.csv")

## find highest suitability for each slice
head(all_data )

SuitabilityPerSlice <- all_datax %>%
  group_by(Species, Life_Stage, Hydraulic, position, Node) %>%
  mutate(MaxPercentage = max(TimePercentage)) %>%
  select(-water_year,  - TimePercentage, -Suitability_Class) %>%
  distinct()

head(SuitabilityPerSlice)
SuitabilityPerSlice

write.csv(SuitabilityPerSlice, "output_data/results/S1_suitability_per_slice.csv")


# Q limits for high Probs -------------------------------------------------

## different number of slices per node, so combine Q limits by node, not species

## define nodes
NodeNames <- unique(SuitabilityPerSlice$Node)
NodeNames
## list all files
ts <- list.files("output_data/", pattern="Q_limits")
ts <- Filter(function(x) !grepl("High_Probs", x), ts)

ts

for(n in 1:length(NodeNames)) {
  ## subset per node
  limitsx <- NULL
  ns <- Filter(function(x) grepl(paste(NodeNames[n]), x), ts)
   for(s in 1: length(ns)) {
     ## upload species per node and combine
     limits <- read.csv(file=paste("output_data/", ns[s], sep=""))
     limitsx <- rbind(limitsx, limits)
     limits
     limitsx
  
   }
  write.csv(limitsx, paste("output_data/results/S1_", NodeNames[n], "_Q_limits_all_species.csv", sep=""))
}

limitsx

