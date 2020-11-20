## willow model application
## Jenny Rogers & Katie Irving

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

## upload model
setwd("/Users/katieirving/Documents/git/SOC_tier_3")
load(file = "models_functions/depth_seedling_mod.rda")
summary(depth_seedling_mod)
## upload shear stress model
load(file="models_functions/shear_seedling.rda")
summary(shear_seedling)

## root function
load(file="models_functions/root_interpolation_function.Rdata")

## define root equation
load(file="models_functions/expression_Q_limit_function.RData")


## upload hydraulic data
setwd("/Users/katieirving/Documents/git/SOC_tier_3")

## upload hydraulic data
setwd("input_data/Hydraulics")

h <- list.files(pattern="hydraulic")
length(h) ## 28
h

setwd("/Users/katieirving/Documents/git/SOC_tier_3")
n=1
p=2

for(n in 1: length(h)) {
  
hydraul <- read.csv(file=paste("input_data/Hydraulics/", h[n], sep=""))

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


# ## melt channel position data
hyd_shear <- hyd_dep %>%
  select(DateTime, Q, date_num, month, day, water_year, contains("shear") )

hyd_dep <- hyd_dep %>%
  select(DateTime, Q, date_num, month, day, water_year, contains("av.depth"))

# transform m to cm
hyd_dep<-reshape2::melt(hyd_dep, id=c("DateTime","Q",  "date_num", "month", "day", "water_year"))
hyd_dep <- hyd_dep %>% 
  mutate(depth_cm = as.numeric(as.character(value*100)))

hyd_shear<-reshape2::melt(hyd_shear, id=c("DateTime","Q",  "date_num", "month", "day", "water_year"))
hyd_shear <- hyd_shear %>% rename(shear = value)


## predict values

all_data <- hyd_dep %>%
  mutate(prob_fit = predict(depth_seedling_mod, newdata = hyd_dep, type="response")) %>%
  mutate(prob_fit = ifelse(prob_fit<=0, 0, prob_fit)) %>%## predicts negative percentages - cut off at 0 for quick fix
  mutate(prob_fit = ifelse(prob_fit>=100, 100, prob_fit)) 
  

## define positions
positions <- unique(all_data$variable)

## Q Limits
limits <- as.data.frame(matrix(ncol=length(positions), nrow=2)) 
limits$Type<-c("Q_limit1", "Q_limit2")

H_limits <- as.data.frame(matrix(ncol=length(positions), nrow=2)) 
H_limits$Type<-c("Hydraulic_limit1", "Hydraulic_limit2")


NodeName <- str_split(h[n], "_", 3)[[1]]
NodeName <- NodeName[1]

time_statsx <- NULL
days_data <- NULL


  for(p in 1:length(positions)) {

# probability as a function of discharge -----------------------------------

new_data <- all_data %>% 
  filter(variable  == positions[p])

## define position
PositionName <- str_split(positions[p], "_", 3)[[1]]
PositionName <- PositionName[2]


peak <- new_data %>%
  filter(prob_fit == max(prob_fit)) #%>%

peakQ  <- max(peak$Q)
# min_limit <- filter(new_data, depth_cm >= 0.1)
# min_limit <- min(min_limit$Q) ## min_limit not needed for willow as don't need flow

## find roots for each probability
newx1 <- RootLinearInterpolant(new_data$Q, new_data$prob_fit, 50)
hy_lim <- RootLinearInterpolant(new_data$depth_cm, new_data$prob_fit, 50)

if(max(new_data$prob_fit < 50 )) {
  newx1 <- max(new_data$Q)
  hy_lim <- max(new_data$shear)
} else {
  newx1 <- RootLinearInterpolant(new_data$Q, new_data$prob_fit, 50)
  hy_lim <- RootLinearInterpolant(new_data$shear, new_data$prob_fit, 50)
}

if(length(newx1)>1) {
  newx1 <- sort(newx1)[1]
  hy_lim <- sort(hy_lim)[1]
}


## MAKE DF OF Q LIMITS

limits[,p] <- c(newx1)
H_limits[,p] <- c(hy_lim)

# create year_month column       
new_datax <- new_data %>% unite(month_year, water_year:month, sep="-", remove=F) 

# dataframe for stats -----------------------------------------------------

## define critical period or season for seedling as all year is critical
non_critical <- c(1:3,10:12) ## winter months
critical <- c(4:9) ## summer months

new_datax <- new_datax %>%
  mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") )


# time stats - mid channel ------------------------------------------------

## Main channel curves

thresh <- expression_Q(newx1, peakQ) 
thresh <-as.expression(do.call("substitute", list(thresh[[1]], list(limit = as.name("newx1")))))
thresh
###### calculate amount of time

time_stats <- new_datax %>%
  dplyr::group_by(water_year, season) %>%
  dplyr::mutate(Seasonal = sum(Q <= newx1)/length(DateTime)*100) %>%
  distinct(water_year, Seasonal) %>%
  mutate(position= paste(PositionName), Node = NodeName)
time_stats

time_statsx <- rbind(time_statsx, time_stats)
# startsWith(NodeName, "J", trim=T)
### count hours per day
if(startsWith(NodeName, "J", trim=T)) {

  new_datax <- new_datax %>%
  ungroup() %>%
  group_by(month, day, water_year, ID01 = data.table::rleid(Q <= newx1)) %>%
  mutate(Mid = if_else(Q <= newx1, row_number(), 0L)) %>%
  mutate(position= paste(PositionName), Node = NodeName)
} else { ## count days per month
  
  new_datax <- new_datax %>%
    ungroup() %>%
    group_by(month, water_year, ID01 =  data.table::rleid(Q <= newx1)) %>%
    mutate(Mid = if_else(Q <= newx1, row_number(), 0L)) %>%
    mutate(position= paste(PositionName), Node = NodeName)
}

days_data <- rbind(days_data, new_datax)


} ## end 2nd loop

limits <- rbind(limits, H_limits)

## note that 0.1 upper/lower limit is max/min Q to adhere to 0.1 bound
limits <- limits %>%
  mutate(Species ="Willow", Life_Stage = "Seedling", Hydraulic = "Depth", Node = NodeName)


write.csv(limits, paste("output_data/02_",NodeName,"_Willow_Seedling_depth_Q_limits.csv", sep=""))
## percentage time
melt_time<-reshape2::melt(time_statsx, id=c("season", "position", "water_year", "Node"))
melt_time <- melt_time %>% 
  rename( Probability_Threshold = variable) %>%
  mutate(Species ="Willow", Life_Stage = "Seedling", Hydraulic = "Depth", Node = NodeName)

write.csv(melt_time, paste("output_data/02_", NodeName, "_Willow_Seedling_depth_time_stats.csv", sep=""))

### days per month
days_data <- select(days_data, c(Q, month, water_year, day, ID01, Mid, position, DateTime, Node) )# all probs

melt_data<-reshape2::melt(days_data, id=c("ID01", "day", "month", "water_year", "Q", "position", "Node", "DateTime"))
melt_data <- rename(melt_data, Probability_Threshold = variable, 
                    consec_hours = value)



if(startsWith(NodeName, "J", trim=T)) {
  ## count how many full days i.e. 24 hours
  total_days01 <- melt_data %>% 
    group_by(ID01, day, month, water_year, position) %>%
    summarise(n_hours = max(consec_hours))  %>%
    mutate(n_days = ifelse(n_hours >= 24, 1, 0)) # %>%
  
  ## count the number of days in each month
  total_days_per_month01 <- total_days01 %>%
    group_by(month, water_year, position) %>%
    summarise(days_per_month = sum(n_days))
  
  # # create year_month column       
  total_days <- ungroup(total_days_per_month01) %>%
    unite(month_year, water_year:month, sep="-", remove=F) %>%
    mutate(Node= paste(NodeName)) #%>%
} else {
  
  total_days01 <- melt_data %>% 
    group_by(ID01, month, water_year, position) %>%
    summarise(days_per_month = max(consec_hours)) 
  
  # # create year_month column       
  total_days <- ungroup(total_days01) %>%
    unite(month_year, water_year:month, sep="-", remove=F) %>%
    mutate(Node= paste(NodeName)) #%>%
  
}


total_days$month_year <-  zoo::as.yearmon(total_days$month_year)

total_days <- total_days %>%
  mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") )

melt_days<-reshape2::melt(total_days, id=c("month_year", "water_year", "month", "season", "position", "Node"))
melt_days <- melt_days %>%
  rename(Probability_Threshold = variable, n_days = value) %>%
  mutate(Species ="Willow", Life_Stage = "Seedling", Hydraulic = "Depth")

## save df
write.csv(melt_days, paste("output_data/02_", NodeName, "_Willow_Seedling_depth_total_days_long.csv", sep="") )

cat(paste("Finished Node", NodeName))

} ## end 1st loop


# Shear Stress ------------------------------------------------------------

for(n in 1: length(h)) {
  
  hydraul <- read.csv(file=paste("input_data/Hydraulics/", h[n], sep=""))
  
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
  
  
  # ## melt channel position data
  hyd_shear <- hyd_dep %>%
    select(DateTime, Q, date_num, month, day, water_year, contains("shear") )
  


  hyd_shear<-reshape2::melt(hyd_shear, id=c("DateTime","Q",  "date_num", "month", "day", "water_year"))
  hyd_shear <- hyd_shear %>% rename(shear = value)
  
  
  ## predict values
  
  all_data <- hyd_shear %>%
    mutate(prob_fit = predict(shear_seedling, newdata = hyd_shear, type="response")) %>%
    mutate(prob_fit = ifelse(prob_fit<=0, 0, prob_fit)) %>%## predicts negative percentages - cut off at 0 for quick fix
    mutate(prob_fit = ifelse(prob_fit>=100, 100, prob_fit)) 

  
  ## define positions
  positions <- unique(all_data$variable)
  
  ## Q Limits
  limits <- as.data.frame(matrix(ncol=length(positions), nrow=2)) 
  limits$Type<-c("Q_limit1", "Q_limit2")
  
  H_limits <- as.data.frame(matrix(ncol=length(positions), nrow=2)) 
  H_limits$Type<-c("Hydraulic_limit1", "Hydraulic_limit2")
  
  
  NodeName <- str_split(h[n], "_", 3)[[1]]
  NodeName <- NodeName[1]
  
  time_statsx <- NULL
  days_data <- NULL
  
  
  for(p in 1:length(positions)) {
    
    # probability as a function of discharge -----------------------------------
    range(new_data$prob_fit)
    new_data <- all_data %>% 
      filter(variable  == positions[p])
    
    ## define position
    PositionName <- str_split(positions[p], "_", 3)[[1]]
    PositionName <- PositionName[2]
    
    
    peak <- new_data %>%
      filter(prob_fit == max(prob_fit)) #%>%
    
    peakQ  <- max(peak$Q)

    # min_limit <- filter(new_data, depth_cm >= 0.1)
    # min_limit <- min(min_limit$Q) ## min_limit not needed for willow as don't need flow
    
    ## find roots for each probability

    
    if(max(new_data$prob_fit < 50 )) {
      newx1 <- max(new_data$Q)
      hy_lim <- max(new_data$shear)
    } else {
      newx1 <- RootLinearInterpolant(new_data$Q, new_data$prob_fit, 50)
      hy_lim <- RootLinearInterpolant(new_data$shear, new_data$prob_fit, 50)
    }
    
    if(length(newx1)>1) {
      newx1 <- sort(newx1)[1]
      hy_lim <- sort(hy_lim)[1]
    }
    
    
    ## MAKE DF OF Q LIMITS
    
    limits[,p] <- c(newx1)
    H_limits[,p] <- c(hy_lim)
    
    # create year_month column       
    new_datax <- new_data %>% unite(month_year, water_year:month, sep="-", remove=F) 
    
    # dataframe for stats -----------------------------------------------------
    
    ## define critical period or season for seedling as all year is critical
    non_critical <- c(1:3,10:12) ## winter months
    critical <- c(4:9) ## summer months
    
    new_datax <- new_datax %>%
      mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") )
    
    
    # time stats - mid channel ------------------------------------------------
    
    ## Main channel curves
    
    thresh <- expression_Q(newx1, peakQ) 
    thresh <-as.expression(do.call("substitute", list(thresh[[1]], list(limit = as.name("newx1")))))
   
    ###### calculate amount of time
    
    time_stats <- new_datax %>%
      dplyr::group_by(water_year, season) %>%
      dplyr::mutate(Seasonal = sum(Q <= newx1)/length(DateTime)*100) %>%
      distinct(water_year, Seasonal) %>%
      mutate(position= paste(PositionName), Node = NodeName)
    time_stats
    
    time_statsx <- rbind(time_statsx, time_stats)
    
    if(startsWith(NodeName, "J", trim=T)) {
      
      new_datax <- new_datax %>%
        ungroup() %>%
        group_by(month, day, water_year, ID01 = data.table::rleid(Q <= newx1)) %>%
        mutate(Mid = if_else(Q <= newx1, row_number(), 0L)) %>%
        mutate(position= paste(PositionName), Node = NodeName)
    } else { ## count days per month
      
      new_datax <- new_datax %>%
        ungroup() %>%
        group_by(month, water_year, ID01 =  data.table::rleid(Q <= newx1)) %>%
        mutate(Mid = if_else(Q <= newx1, row_number(), 0L)) %>%
        mutate(position= paste(PositionName), Node = NodeName)
    }
    
    
    days_data <- rbind(days_data, new_datax)
    
    
  } ## end 2nd loop
  
  limits <- rbind(limits, H_limits)
  limits <- limits %>%
    mutate(Species ="Willow", Life_Stage = "Seedling", Hydraulic = "Shear Stress", Node = NodeName)
  
  write.csv(limits, paste("output_data/02_",NodeName,"_Willow_Seedling_shear_Q_limits.csv", sep=""))
  ## percentage time
  melt_time<-reshape2::melt(time_statsx, id=c("season", "position", "water_year", "Node"))
  melt_time <- melt_time %>% 
    rename( Probability_Threshold = variable) %>%
    mutate(Species ="Willow", Life_Stage = "Seedling", Hydraulic = "Shear Stress", Node = NodeName)
  head(melt_time)
  write.csv(melt_time, paste("output_data/02_", NodeName, "_Willow_Seedling_shear_time_stats.csv", sep=""))
  
  ### days per month
  days_data <- select(days_data, c(Q, month, water_year, day, ID01, Mid, position, DateTime, Node) )# all probs
  
  melt_data<-reshape2::melt(days_data, id=c("ID01", "day", "month", "water_year", "Q", "position", "Node", "DateTime"))
  melt_data <- rename(melt_data, Probability_Threshold = variable, 
                      consec_hours = value)
  
  if(startsWith(NodeName, "J", trim=T)) {
    ## count how many full days i.e. 24 hours
    total_days01 <- melt_data %>% 
      group_by(ID01, day, month, water_year, position) %>%
      summarise(n_hours = max(consec_hours))  %>%
      mutate(n_days = ifelse(n_hours >= 24, 1, 0)) # %>%
    
    ## count the number of days in each month
    total_days_per_month01 <- total_days01 %>%
      group_by(month, water_year, position) %>%
      summarise(days_per_month = sum(n_days))
    
    # # create year_month column       
    total_days <- ungroup(total_days_per_month01) %>%
      unite(month_year, water_year:month, sep="-", remove=F) %>%
      mutate(Node= paste(NodeName)) #%>%
  } else {
    
    total_days01 <- melt_data %>% 
      group_by(ID01, month, water_year, position) %>%
      summarise(days_per_month = max(consec_hours)) 
    
    # # create year_month column       
    total_days <- ungroup(total_days01) %>%
      unite(month_year, water_year:month, sep="-", remove=F) %>%
      mutate(Node= paste(NodeName)) #%>%
    
  }
  
  total_days$month_year <-  zoo::as.yearmon(total_days$month_year)
  
  total_days <- total_days %>%
    mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") )
  
  melt_days<-reshape2::melt(total_days, id=c("month_year", "water_year", "month", "season", "position", "Node"))
  melt_days <- melt_days %>%
    rename(Probability_Threshold = variable, n_days = value) %>%
    mutate(Species ="Willow", Life_Stage = "Seedling", Hydraulic = "Shear Stress")
  
  ## save df
  write.csv(melt_days, paste("output_data/02_", NodeName, "_Willow_Seedling_shear_total_days_long.csv", sep="") )
  
  cat(paste("Finished Node", NodeName))
  
} ## end 1st loop
