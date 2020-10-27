### upload and designate suitability
library(dplyr)
library(tidyverse)
## time stats

willow_seed_depth <- read.csv("output_data/02_willow_seedling_depth_time_stats.csv")
willow_seed_shear <- read.csv("output_data/02_willow_seedling_shear_time_stats_long.csv")
head(willow_seed_depth)
head(willow_seed_shear)

willow_germ_depth <- read.csv("output_data/01_willow_germ_depth_time_stats_long.csv")
head(willow_germ_depth)

chub_depth <- read.csv("output_data/04_chub_adult_depth_time_stats.csv")
chub_velocity <- read.csv("output_data/04_chub_adult_velocity_time_stats.csv")
head(chub_depth)
head(chub_velocity)


# Format Data -------------------------------------------------------------


chub_depth <- chub_depth %>%
  mutate(Time_Period = ifelse(Time_Period == "Mid", "Annual", "Seasonal")) %>%
  rename(TimePercentage = value) %>%
  mutate(Species = "Arroyo Chub", Life_Stage = "Adult", Hydraulic = "Depth") %>%
  filter(season == "critical") %>%
  distinct()

chub_depth


chub_velocity <- chub_velocity %>%
  mutate(Time_Period = ifelse(Time_Period == "Mid", "Annual", "Seasonal")) %>%
  rename(TimePercentage = value) %>%
  mutate(Species = "Arroyo Chub", Life_Stage = "Adult", Hydraulic = "Velocity") %>%
  filter(season == "critical") %>%
  distinct()
chub_velocity


willow_seed_depth <- willow_seed_depth %>%
  # mutate(Time_Period = ifelse(Time_Period == "Mid", "Annual", "Seasonal")) %>%
  rename(TimePercentage = value) %>%
  mutate(Species = "Willow", Life_Stage = "Seedling", Hydraulic = "Depth") %>%
  filter(season == "critical") %>%
  distinct()

willow_seed_depth

willow_seed_shear <- willow_seed_shear %>%
  # mutate(Time_Period = ifelse(Time_Period == "Mid", "Annual", "Seasonal")) %>%
  rename(TimePercentage = value) %>%
  mutate(Species = "Willow", Life_Stage = "Seedling", Hydraulic = "Shear Stress") %>%
  filter(season == "critical") %>%
  distinct()

willow_seed_shear

willow_germ_depth <- willow_germ_depth %>%
  # mutate(Time_Period = ifelse(Time_Period == "Mid", "Annual", "Seasonal")) %>%
  rename(TimePercentage = value) %>%
  mutate(Species = "Willow", Life_Stage = "Germination", Hydraulic = "Depth") %>%
  filter(season == "critical") %>%
  distinct()

willow_germ_depth


# Calculate suitability ---------------------------------------------------

## willow germination depth
willow_germ_depth <- willow_germ_depth %>%
  filter(Time_Period == "Seasonal") %>%
  distinct()

# ## add water year - ADD LATER!!!!!
# time_stats_ann <- full_join(time_stats_ann, water_year_type, by = "water_year")
# time_stats_ann <- na.omit(time_stats_ann)

## calculate suitability
probs <- seq(1, length(willow_germ_depth$X), 1)  

for(p in 1: length(probs)) {
  
willow_germ_depth$Suitability_Class[p] <- if(willow_germ_depth$TimePercentage[p] >= 50) {
    paste("High") 
    
  } else if (willow_germ_depth$TimePercentage[p] <= 50) {
    paste("Low") 
    
  } else {
    paste("Partial")
  }
  
}

willow_germ_depth

## willow seedling depth
willow_seed_depth <- willow_seed_depth %>%
  filter(Time_Period == "Seasonal") %>%
  distinct()
willow_seed_depth
# ## add water year - ADD LATER!!!!!
# time_stats_ann <- full_join(time_stats_ann, water_year_type, by = "water_year")
# time_stats_ann <- na.omit(time_stats_ann)

## calculate suitability
probs <- seq(1, length(willow_seed_depth$X), 1)  

for(p in 1: length(probs)) {
  
  willow_seed_depth$Suitability_Class[p] <- if(willow_seed_depth$TimePercentage[p] >= 50) {
    paste("High") 
    
  } else if (willow_seed_depth$TimePercentage[p] <= 50) {
    paste("Low") 
    
  } else {
    paste("Partial")
  }
  
}

willow_seed_depth

## willow seedling shear
willow_seed_shear <- willow_seed_shear %>%
  filter(Time_Period == "Seasonal") %>%
  distinct()

# ## add water year - ADD LATER!!!!!
# time_stats_ann <- full_join(time_stats_ann, water_year_type, by = "water_year")
# time_stats_ann <- na.omit(time_stats_ann)

## calculate suitability
probs <- seq(1, length(willow_seed_shear$X), 1)  

for(p in 1: length(probs)) {
  
  willow_seed_shear$Suitability_Class[p] <- if(willow_seed_shear$TimePercentage[p] >= 50) {
    paste("High") 
    
  } else if (willow_seed_shear$TimePercentage[p] <= 50) {
    paste("Low") 
    
  } else {
    paste("Partial")
  }
  
}

willow_seed_shear

## chub depth
chub_depth <- chub_depth %>%
  filter(Time_Period == "Annual") %>%
  distinct()

# ## add water year - ADD LATER!!!!!
# time_stats_ann <- full_join(time_stats_ann, water_year_type, by = "water_year")
# time_stats_ann <- na.omit(time_stats_ann)

## calculate suitability
probs <- seq(1, length(chub_depth$X), 1)  

for(p in 1: length(probs)) {
  
  chub_depth$Suitability_Class[p] <- if(chub_depth$TimePercentage[p] >= 50) {
    paste("High") 
    
  } else if (chub_depth$TimePercentage[p] <= 50) {
    paste("Low") 
    
  } else {
    paste("Partial")
  }
  
}

chub_depth

## chub velocity
chub_velocity <- chub_velocity %>%
  filter(Time_Period == "Annual") %>%
  distinct()

# ## add water year - ADD LATER!!!!!
# time_stats_ann <- full_join(time_stats_ann, water_year_type, by = "water_year")
# time_stats_ann <- na.omit(time_stats_ann)

## calculate suitability
probs <- seq(1, length(chub_velocity$X), 1)  

for(p in 1: length(probs)) {
  
  chub_velocity$Suitability_Class[p] <- if(chub_velocity$TimePercentage[p] >= 50) {
    paste("High") 
    
  } else if (chub_velocity$TimePercentage[p] <= 50) {
    paste("Low") 
    
  } else {
    paste("Partial")
  }
  
}

chub_velocity
chub_depth
sum_stats_vel
sum_stats_dep

# chub_velocity_high <- chub_velocity %>%
#   filter(Suitability_Class == "High")
# 
# chub_velocity_high
# # 1994 = mod
# # 1997 - wet
# # 2004 - Dry
# # 2010 - wet
# 
# sum_stats_vel <- chub_velocity %>%
#   group_by(position) %>%
#   mutate(mean_time = mean(TimePercentage)) %>%
#   select(position, Species, Life_Stage, Hydraulic, mean_time) %>%
#   distinct()
# 
# sum_stats_dep <- chub_depth %>%
#   group_by(position) %>%
#   mutate(mean_time = mean(TimePercentage)) %>%
#   select(position, Species, Life_Stage, Hydraulic, mean_time) %>%
#   distinct()

# sum_stats_dep

## combine years
head(chub_depth)

all_data <- rbind(chub_velocity, chub_depth, willow_germ_depth, willow_seed_depth, willow_seed_shear)
write.csv(all_data, "output_data/S1_all_suitability_all_years.csv")


time_stats <- all_data  %>%
  select(Species, Life_Stage, Hydraulic, water_year,Time_Period, position,
         Suitability_Class) %>%
  distinct()


SuitClassOverYears <- time_stats %>%
  group_by(Species, Life_Stage, Hydraulic, Time_Period, position) %>%
  summarise(Overall_Class = tail(names(sort(table(Suitability_Class))), 1))

SuitClassOverYears


write.csv(SuitClassOverYears, "output_data/S1_all_suitability_combined_years.csv")

## find highest suitability for each slice
head(all_data )

SuitabilityPerSlice <- all_data %>%
  group_by(Species, Life_Stage, Hydraulic, position) %>%
  mutate(MaxPercentage = max(TimePercentage)) %>%
  select(-water_year, -X, - TimePercentage, -Suitability_Class) %>%
  distinct()

head(SuitabilityPerSlice)
SuitabilityPerSlice

write.csv(SuitabilityPerSlice, "output_data/S1_suitability_per_slice.csv")

# Q limits for high Probs -------------------------------------------------

seed_shear <- read.csv("output_data/01_willow_seedling_shear_Q_limits.csv")
seed_depth <- read.csv("output_data/02_willow_seedling_depth_Q_limits.csv")

chub_velocity <- read.csv("output_data/04_chub_adult_velocity_Q_limits.csv")
chub_depth <- read.csv("output_data/04_chub_adult_depth_Q_limits.csv")

germ_depth <- read.csv("output_data/01_willow_germ_depth_Q_limits.csv")

head(seed_shear)
seed_shear <- seed_shear %>%
  mutate(Species = "Willow", Life_Stage = "Seedling", Hydraulic = "Shear Stress")
head(seed_depth)
seed_depth <- seed_depth %>%
  mutate(Species = "Willow", Life_Stage = "Seedling", Hydraulic = "Depth")

head(chub_velocity)
chub_velocity <- chub_velocity %>%
  mutate(Species = "Arroyo Chub", Life_Stage = "Adult", Hydraulic = "Velocity")

chub_depth <- chub_depth %>%
  mutate(Species = "Arroyo Chub", Life_Stage = "Adult", Hydraulic = "Depth")

head(germ_depth)

germ_depth <- germ_depth %>%
  mutate(Species = "Willow", Life_Stage = "Germination", Hydraulic = "Depth")


all_limits <- rbind(seed_shear, seed_depth, chub_velocity, chub_depth, germ_depth)

write.csv(all_limits, "output_data/S1_all_suit_limits.csv")

head(all_limits)
