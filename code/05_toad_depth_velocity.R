## toad

## Katie Irving

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
setwd("input_data/Hydraulics")

h <- list.files(pattern="hydraulic")
length(h) ## 28
h

setwd("/Users/katieirving/Documents/git/SOC_tier_3")
n=30
p=2

### breeding/eggs depth

for(n in 1: length(h)) {
  
  hydraul <- read.csv(file=paste("input_data/Hydraulics/", h[n], sep=""))
  head(hydraul)
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
  
  head(hyd_dep)
  # ## melt channel position data
  # hyd_power <- hyd_dep %>%
  #   select(DateTime, Q, date_num, month, day, water_year, contains("power") )
  
  hyd_dep <- hyd_dep %>%
    select(DateTime, Q, date_num, month, day, water_year, contains("av.depth"))
  # 
  # transform m to cm
  hyd_dep<-reshape2::melt(hyd_dep, id=c("DateTime","Q",  "date_num", "month", "day", "water_year"))
  all_data<- hyd_dep %>%
    mutate(depth_cm = as.numeric(as.character(value*100)))
  
  # hyd_power<-reshape2::melt(hyd_power, id=c("DateTime","Q",  "date_num", "month", "day", "water_year"))
  # all_data <- hyd_power %>% rename(power = value)
  
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
    
    min_limit <- filter(new_data, Q > 0)
    min_limit <- min(min_limit$Q)

    curve <- spline(new_data$Q, new_data$depth_cm,
                    xmin = min(new_data$Q), xmax = max(new_data$Q), ties = mean)
    
 
    if(max(curve$y)<15) {
      newx1<- max(curve$x)
    } else {
      newx1 <- approx(x = curve$y, y = curve$x, xout = 15)$y
    }
      
    
    

    limits[,p] <- c(newx1)
    H_limits[, p] <- 15

    # create year_month column       
    new_datax <- new_data %>% unite(month_year, water_year:month, sep="-", remove=F) 
    
    # dataframe for stats -----------------------------------------------------
    
    ## define critical period or season for Adult as all year is critical
    non_critical <- c(1:2,10:12) ## winter months
    critical <- c(3:9) ## summer months
    
    new_datax <- new_datax %>%
      mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") )

    
    # time stats - mid channel ------------------------------------------------
    # sum(new_data$Q >= min_limit & new_data$Q < newx1)/length(new_data$DateTime)*100
    # sum(new_data$Q >= min_limit & new_data$Q < 15)/length(new_data$DateTime)*100
    ###### calculate amount of time
    
    time_stats <- new_datax %>%
      dplyr::group_by(water_year, season) %>%
      dplyr::mutate(Seasonal = sum(Q >= min_limit & Q < newx1)/length(DateTime)*100) %>%
      distinct(water_year, Seasonal) %>%
      mutate(position= paste(PositionName), Node = NodeName)
    time_stats
    
    time_statsx <- rbind(time_statsx, time_stats)
    # startsWith(NodeName, "J", trim=T)
    ### count hours per day
    if(startsWith(NodeName, "J", trim=T)) {
      
      new_datax <- new_datax %>%
        ungroup() %>%
        group_by(month, day, water_year, ID01 = data.table::rleid(Q >= min_limit & Q < newx1)) %>%
        mutate(Mid = if_else(Q >= min_limit & Q < newx1, row_number(), 0L)) %>%
        mutate(position= paste(PositionName), Node = NodeName)
    } else { ## count days per month
      
      new_datax <- new_datax %>%
        ungroup() %>%
        group_by(month, water_year, ID01 =  data.table::rleid(Q >= min_limit & Q < newx1)) %>%
        mutate(Mid = if_else(Q >= min_limit & Q < newx1, row_number(), 0L)) %>%
        mutate(position= paste(PositionName), Node = NodeName)
    }
    
    days_data <- rbind(days_data, new_datax)

    
  } ## end 2nd loop

  limits <- rbind(limits, H_limits)
  
  limits <- limits %>%
    mutate(Species ="Toad", Life_Stage = "Breeding/Eegs", Hydraulic = "Depth", Node = NodeName)
  
  write.csv(limits, paste("output_data/05_",NodeName,"_Toad_Breeding_Eggs_Depth_Q_limits.csv", sep=""))
  ## percentage time
  melt_time<-reshape2::melt(time_statsx, id=c("position", "water_year", "Node","season"))
  
  melt_time <- melt_time %>% 
    rename( Probability_Threshold = variable) %>%
    mutate(Species ="Toad", Life_Stage = "Breeding/Eggs", Hydraulic = "Depth", Node = NodeName)
  
  write.csv(melt_time, paste("output_data/05_", NodeName, "_Toad_Breeding_Eggs_Depth_time_stats.csv", sep=""))
  
  ### days per month
  days_data <- select(days_data, c(Q, month, water_year, day, ID01, Mid, position, DateTime, Node, season) )# all probs

  melt_data<-reshape2::melt(days_data, id=c("ID01", "day", "month", "Q", "water_year", "position", "Node", "DateTime", "season"))
  melt_data <- melt_data %>% rename(consec_hours = value) %>%
    select(-variable)
head(melt_data)
  tail(melt_data)
  
  
  if(startsWith(NodeName, "J", trim=T)) {
    total_days01 <- melt_data %>% 
      group_by(ID01, day, month, water_year, position) %>%
      summarise(n_hours = max(consec_hours))  %>%
      mutate(n_days = ifelse(n_hours >= 24, 1, 0)) # %>%

    ## count the number of days in each month
    total_days_per_month01 <- total_days01 %>%
      group_by(month, water_year, position) %>%
      summarise(days_per_month = sum(n_days))
    total_days_per_month01
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
  
  melt_days<-reshape2::melt(total_days, id=c("month_year", "water_year", "month",  "position", "Node", "season"))
  melt_days <- melt_days %>%
    rename(Probability_Threshold = variable, n_days = value) %>%
    mutate(Species ="Toad", Life_Stage = "Breeding/Eggs", Hydraulic = "Depth")
  
  ## save df
  write.csv(melt_days, paste("output_data/05_", NodeName, "_Toad_Breeding_Eggs_Depth_total_days_long.csv", sep="") )
  
  cat(paste("Finished Node", NodeName))
  
} ## end 1st loop


# velocity ------------------------------------------------------------
h
n=20
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
  
  
  # # ## melt channel position data
  # hyd_shear <- hyd_dep %>%
  #   select(DateTime, Q, date_num, month, day, water_year, contains("shear") )
  
  hyd_vel <- hyd_dep %>%
    select(DateTime, Q, date_num, month, day, water_year, contains("vel"))

  
  # hyd_shear<-reshape2::melt(hyd_shear, id=c("DateTime","Q",  "date_num", "month", "day", "water_year"))
  # hyd_shear <- hyd_shear %>% rename(shear = value)
  
  hyd_vel<-reshape2::melt(hyd_vel, id=c("DateTime","Q",  "date_num", "month", "day", "water_year"))
  all_data <- hyd_vel %>% rename(vel_m = value)

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
    
    min_limit <- filter(new_data, Q > 0)
    min_limit <- min(min_limit$Q)
    ## define position
    PositionName <- str_split(positions[p], "_", 3)[[1]]
    PositionName <- PositionName[2]
    PositionName
    curve <- spline(new_data$Q, new_data$vel_m,
                    xmin = min(new_data$Q), xmax = max(new_data$Q), ties = mean)

    ## main channel values
    if(min(curve$y)>0.05) {
      newx1 <- try(min(curve$x), silent=T)
    } else if (max(curve$y) < 0.05) {
      newx1 <- try(max(curve$x), silent=T)
    } else {
      newx1 <- try(approx(x = curve$y, y = curve$x, xout = 0.05)$y, silent =T)
    }
      
    
    newx1
    min_limit
    ## MAKE DF OF Q LIMITS
    
    limits[,p] <- c(newx1)
    H_limits[, p] <- 0.05
    
    # create year_month column       
    new_datax <- new_data %>% unite(month_year, water_year:month, sep="-", remove=F) 
    
    # dataframe for stats -----------------------------------------------------
    
    ## define critical period or season for Adult as all year is critical
    non_critical <- c(1:2,10:12) ## winter months
    critical <- c(3:9) ## summer months
    
    new_datax <- new_datax %>%
      mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") )
    
    
    # time stats - mid channel ------------------------------------------------
    
    
    ###### calculate amount of time

    time_stats <- new_datax %>%
      dplyr::group_by(water_year, season) %>%
      dplyr::mutate(Seasonal = sum(Q >= min_limit & Q < newx1)/length(DateTime)*100) %>%
      distinct(water_year, Seasonal) %>%
      mutate(position= paste(PositionName), Node = NodeName)
    time_stats
    
    time_statsx <- rbind(time_statsx, time_stats)
    
    if(startsWith(NodeName, "J", trim=T)) {
      
      new_datax <- new_datax %>%
        ungroup() %>%
        group_by(month, day, water_year, ID01 = data.table::rleid(Q >= min_limit & Q < newx1)) %>%
        mutate(Mid = if_else(Q >= min_limit & Q < newx1, row_number(), 0L)) %>%
        mutate(position= paste(PositionName), Node = NodeName)
    } else { ## count days per month
      
      new_datax <- new_datax %>%
        ungroup() %>%
        group_by(month, water_year, ID01 =  data.table::rleid(Q >= min_limit & Q < newx1)) %>%
        mutate(Mid = if_else(Q >= min_limit & Q < newx1, row_number(), 0L)) %>%
        mutate(position= paste(PositionName), Node = NodeName)
    }
    
    
    days_data <- rbind(days_data, new_datax)
    
    
  } ## end 2nd loop
  
  limits <- rbind(limits, H_limits)
  
  limits <- limits %>%
    mutate(Species ="Toad", Life_Stage = "Breeding/Eggs", Hydraulic = "Velocity", Node = NodeName)
  
  write.csv(limits, paste("output_data/05_",NodeName,"_Toad_Breeding_Velocity_Q_limits.csv", sep=""))
  ## percentage time
  melt_time<-reshape2::melt(time_statsx, id=c("season", "position", "water_year", "Node"))
  melt_time <- melt_time %>% 
    rename( Probability_Threshold = variable) %>%
    mutate(Species ="Toad", Life_Stage = "Breeding/Eggs", Hydraulic = "Velocity", Node = NodeName)
  head(melt_time)
  write.csv(melt_time, paste("output_data/05_", NodeName, "_Toad_Breeding_Velocity_time_stats.csv", sep=""))
  
  ### days per month
  days_data <- select(days_data, c(Q, month, water_year, day, ID01, Mid, position, DateTime, Node, season) )# all probs
  
  melt_data<-reshape2::melt(days_data, id=c("ID01", "day", "month", "Q", "water_year", "position", "Node", "DateTime", "season"))
  melt_data <- melt_data %>% rename(consec_hours = value) %>%
    select(-variable)
  head(melt_data)
  tail(melt_data)
  
  
  if(startsWith(NodeName, "J", trim=T)) {
    total_days01 <- melt_data %>% 
      group_by(ID01, day, month, water_year, position) %>%
      summarise(n_hours = max(consec_hours))  %>%
      mutate(n_days = ifelse(n_hours >= 24, 1, 0)) # %>%
    
    ## count the number of days in each month
    total_days_per_month01 <- total_days01 %>%
      group_by(month, water_year, position) %>%
      summarise(days_per_month = sum(n_days))
    total_days_per_month01
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
  
  melt_days<-reshape2::melt(total_days, id=c("month_year", "water_year", "month",  "position", "Node", "season"))
  
  melt_days <- melt_days %>%
    rename(Probability_Threshold = variable, n_days = value) %>%
    mutate(Species ="Toad", Life_Stage = "Breeding/Eggs", Hydraulic = "Velocity")
  head(melt_days)
  ## save df
  write.csv(melt_days, paste("output_data/05_", NodeName, "_Toad_Breeding_Velocity_total_days_long.csv", sep="") )
  
  cat(paste("Finished Node", NodeName))
  
} ## end 1st loop
warnings()


# Tadpoles ----------------------------------------------------------------


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
  
  head(hyd_dep)
  # ## melt channel position data
  # hyd_power <- hyd_dep %>%
  #   select(DateTime, Q, date_num, month, day, water_year, contains("power") )
  
  hyd_dep <- hyd_dep %>%
    select(DateTime, Q, date_num, month, day, water_year, contains("av.depth"))
  # 
  # transform m to cm
  hyd_dep<-reshape2::melt(hyd_dep, id=c("DateTime","Q",  "date_num", "month", "day", "water_year"))
  all_data<- hyd_dep %>%
    mutate(depth_cm = as.numeric(as.character(value*100)))
  
  # hyd_power<-reshape2::melt(hyd_power, id=c("DateTime","Q",  "date_num", "month", "day", "water_year"))
  # all_data <- hyd_power %>% rename(power = value)
  
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
    
    min_limit <- filter(new_data, Q > 0)
    min_limit <- min(min_limit$Q)
    
    curve <- spline(new_data$Q, new_data$depth_cm,
                    xmin = min(new_data$Q), xmax = max(new_data$Q), ties = mean)
    
    
    if(max(curve$y)<30) {
      newx1<- max(curve$x)
    } else {
      newx1 <- approx(x = curve$y, y = curve$x, xout = 30)$y
    }
    
    
    
    limits
    limits[,p] <- c(newx1)
    H_limits[, p] <- 30
    
    # create year_month column       
    new_datax <- new_data %>% unite(month_year, water_year:month, sep="-", remove=F) 
    
    # dataframe for stats -----------------------------------------------------
    
    ## define critical period or season for Adult as all year is critical
    non_critical <- c(1:2,10:12) ## winter months
    critical <- c(3:9) ## summer months
    
    new_datax <- new_datax %>%
      mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") )
    
    
    # time stats - mid channel ------------------------------------------------
    
    ###### calculate amount of time
    
    time_stats <- new_datax %>%
      dplyr::group_by(water_year, season) %>%
      dplyr::mutate(Seasonal = sum(Q >= min_limit & Q < newx1)/length(DateTime)*100) %>%
      distinct(water_year, Seasonal) %>%
      mutate(position= paste(PositionName), Node = NodeName)
    time_stats
    
    time_statsx <- rbind(time_statsx, time_stats)
    # startsWith(NodeName, "J", trim=T)
    ### count hours per day
    if(startsWith(NodeName, "J", trim=T)) {
      
      new_datax <- new_datax %>%
        ungroup() %>%
        group_by(month, day, water_year, ID01 = data.table::rleid(Q >= min_limit & Q < newx1)) %>%
        mutate(Mid = if_else(Q >= min_limit & Q < newx1, row_number(), 0L)) %>%
        mutate(position= paste(PositionName), Node = NodeName)
    } else { ## count days per month
      
      new_datax <- new_datax %>%
        ungroup() %>%
        group_by(month, water_year, ID01 =  data.table::rleid(Q >= min_limit & Q < newx1)) %>%
        mutate(Mid = if_else(Q >= min_limit & Q < newx1, row_number(), 0L)) %>%
        mutate(position= paste(PositionName), Node = NodeName)
    }
    
    days_data <- rbind(days_data, new_datax)

    
  } ## end 2nd loop
  
  limits <- rbind(limits, H_limits)
  
  limits <- limits %>%
    mutate(Species ="Toad", Life_Stage = "Tadpole", Hydraulic = "Depth", Node = NodeName)
  
  write.csv(limits, paste("output_data/05_",NodeName,"_Toad_Tadpole_Depth_Q_limits.csv", sep=""))
  ## percentage time
  melt_time<-reshape2::melt(time_statsx, id=c("position", "water_year", "Node","season"))
  
  melt_time <- melt_time %>% 
    rename( Probability_Threshold = variable) %>%
    mutate(Species ="Toad", Life_Stage = "Tadpole", Hydraulic = "Depth", Node = NodeName)
  
  write.csv(melt_time, paste("output_data/05_", NodeName, "_Toad_Tadpole_Depth_time_stats.csv", sep=""))
  
  ### days per month
  days_data <- select(days_data, c(Q, month, water_year, day, ID01, Mid, position, DateTime, Node, season) )# all probs
  
  melt_data<-reshape2::melt(days_data, id=c("ID01", "day", "month", "Q", "water_year", "position", "Node", "DateTime", "season"))
  melt_data <- melt_data %>% rename(consec_hours = value) %>%
    select(-variable)
  head(melt_data)
  tail(melt_data)
  
  
  if(startsWith(NodeName, "J", trim=T)) {
    total_days01 <- melt_data %>% 
      group_by(ID01, day, month, water_year, position) %>%
      summarise(n_hours = max(consec_hours))  %>%
      mutate(n_days = ifelse(n_hours >= 24, 1, 0)) # %>%
    
    ## count the number of days in each month
    total_days_per_month01 <- total_days01 %>%
      group_by(month, water_year, position) %>%
      summarise(days_per_month = sum(n_days))
    total_days_per_month01
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
  
  melt_days<-reshape2::melt(total_days, id=c("month_year", "water_year", "month",  "position", "Node", "season"))
  melt_days <- melt_days %>%
    rename(Probability_Threshold = variable, n_days = value) %>%
    mutate(Species ="Toad", Life_Stage = "Tadpole", Hydraulic = "Depth")
  
  ## save df
  write.csv(melt_days, paste("output_data/05_", NodeName, "_Toad_Tadpole_Depth_total_days_long.csv", sep="") )
  
  cat(paste("Finished Node", NodeName))
  
} ## end 1st loop


# velocity ------------------------------------------------------------

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
  
  
  # # ## melt channel position data
  # hyd_shear <- hyd_dep %>%
  #   select(DateTime, Q, date_num, month, day, water_year, contains("shear") )
  
  hyd_vel <- hyd_dep %>%
    select(DateTime, Q, date_num, month, day, water_year, contains("vel"))

  
  # hyd_shear<-reshape2::melt(hyd_shear, id=c("DateTime","Q",  "date_num", "month", "day", "water_year"))
  # hyd_shear <- hyd_shear %>% rename(shear = value)
  
  hyd_vel<-reshape2::melt(hyd_vel, id=c("DateTime","Q",  "date_num", "month", "day", "water_year"))
  all_data <- hyd_vel %>% rename(vel_m = value)
  
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
    
    curve <- spline(new_data$Q, new_data$vel_m,
                    xmin = min(new_data$Q), xmax = max(new_data$Q), ties = mean)
    
    min_limit <- filter(new_data, Q > 0)
    min_limit <- min(min_limit$Q)
    ## main channel values
    
    if(min(curve$y)>0.4) {
      newx1 <- try(min(curve$x), silent=T)
    } else if (max(curve$y) < 0.4) {
      newx1 <- try(max(curve$x), silent=T)
    } else {
      newx1 <- try(approx(x = curve$y, y = curve$x, xout = 0.4)$y, silent =T)
    }
    
    
    # if(min(curve$y)>0.4) {
    #   newx2 <- try(min(curve$x), silent=T)
    # } else {
    #  
    # }
    
    ## MAKE DF OF Q LIMITS
    
    limits[,p] <- c(newx1)
    H_limits[, p] <- c(0.4)
    
    # create year_month column       
    new_datax <- new_data %>% unite(month_year, water_year:month, sep="-", remove=F) 
    
    # dataframe for stats -----------------------------------------------------
    
    ## define critical period or season for Adult as all year is critical
    non_critical <- c(1:2,10:12) ## winter months
    critical <- c(3:9) ## summer months
    
    new_datax <- new_datax %>%
      mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") )
    
    
    # time stats - mid channel ------------------------------------------------
    
    
    ###### calculate amount of time
    
    time_stats <- new_datax %>%
      dplyr::group_by(water_year, season) %>%
      dplyr::mutate(Seasonal = sum(Q >= min_limit & Q < newx1)/length(DateTime)*100) %>%
      distinct(water_year, Seasonal) %>%
      mutate(position= paste(PositionName), Node = NodeName)
    
    
    time_statsx <- rbind(time_statsx, time_stats)
    
    if(startsWith(NodeName, "J", trim=T)) {
      
      new_datax <- new_datax %>%
        ungroup() %>%
        group_by(month, day, water_year, ID01 = data.table::rleid(Q >= min_limit & Q < newx1)) %>%
        mutate(Mid = if_else(Q >= min_limit & Q < newx1, row_number(), 0L)) %>%
        mutate(position= paste(PositionName), Node = NodeName)
    } else { ## count days per month
      
      new_datax <- new_datax %>%
        ungroup() %>%
        group_by(month, water_year, ID01 =  data.table::rleid(Q >= min_limit & Q < newx1)) %>%
        mutate(Mid = if_else(Q >= min_limit & Q < newx1, row_number(), 0L)) %>%
        mutate(position= paste(PositionName), Node = NodeName)
    }
    
    
    days_data <- rbind(days_data, new_datax)
    
    
  } ## end 2nd loop
  
  limits <- rbind(limits, H_limits)
  
  limits <- limits %>%
    mutate(Species ="Toad", Life_Stage = "Tadpole", Hydraulic = "Velocity", Node = NodeName)
  
  write.csv(limits, paste("output_data/05_",NodeName,"_Toad_Tadpole_Velocity_Q_limits.csv", sep=""))
  ## percentage time
  melt_time<-reshape2::melt(time_statsx, id=c("season", "position", "water_year", "Node"))
  melt_time <- melt_time %>% 
    rename( Probability_Threshold = variable) %>%
    mutate(Species ="Toad", Life_Stage = "Tadpole", Hydraulic = "Velocity", Node = NodeName)
  head(melt_time)
  write.csv(melt_time, paste("output_data/05_", NodeName, "_Toad_Tadpole_Velocity_time_stats.csv", sep=""))
  
  ### days per month
  days_data <- select(days_data, c(Q, month, water_year, day, ID01, Mid, position, DateTime, Node, season) )# all probs
  
  melt_data<-reshape2::melt(days_data, id=c("ID01", "day", "month", "Q", "water_year", "position", "Node", "DateTime", "season"))
  melt_data <- melt_data %>% rename(consec_hours = value) %>%
    select(-variable)
  head(melt_data)
  tail(melt_data)
  
  
  if(startsWith(NodeName, "J", trim=T)) {
    total_days01 <- melt_data %>% 
      group_by(ID01, day, month, water_year, position) %>%
      summarise(n_hours = max(consec_hours))  %>%
      mutate(n_days = ifelse(n_hours >= 24, 1, 0)) # %>%
    
    ## count the number of days in each month
    total_days_per_month01 <- total_days01 %>%
      group_by(month, water_year, position) %>%
      summarise(days_per_month = sum(n_days))
    total_days_per_month01
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
  
  melt_days<-reshape2::melt(total_days, id=c("month_year", "water_year", "month",  "position", "Node", "season"))
  melt_days <- melt_days %>%
    rename(Probability_Threshold = variable, n_days = value) %>%
    mutate(Species ="Toad", Life_Stage = "Tadpole", Hydraulic = "Velocity")
  head(melt_days)
  ## save df
  write.csv(melt_days, paste("output_data/05_", NodeName, "_Toad_Tadpole_Velocity_total_days_long.csv", sep="") )
  
  cat(paste("Finished Node", NodeName))
  
} ## end 1st loop
warnings()

