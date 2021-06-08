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

## upload hydraulic data
setwd("/Users/katieirving/Documents/git/SOC_tier_3")

## upload hydraulic data
setwd("input_data/Hydraulics")

h <- list.files(pattern="hydraulic")
length(h) ## 28
h

setwd("/Users/katieirving/Documents/git/SOC_tier_3")
n=2
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
  
  head(hyd_dep)
  # ## melt channel position data
  hyd_power <- hyd_dep %>%
    select(DateTime, Q, date_num, month, day, water_year, contains("power") )
  
  # hyd_dep <- hyd_dep %>%
  #   select(DateTime, Q, date_num, month, day, water_year, contains("av.depth"))
  # 
  # transform m to cm
  # hyd_dep<-reshape2::melt(hyd_dep, id=c("DateTime","Q",  "date_num", "month", "day", "water_year"))
  # hyd_dep <- hyd_dep %>% 
  #   mutate(depth_cm = as.numeric(as.character(value*100)))
  
  hyd_power<-reshape2::melt(hyd_power, id=c("DateTime","Q",  "date_num", "month", "day", "water_year"))
  all_data <- hyd_power %>% rename(power = value)
  
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
    
    
    curve <- spline(new_data$Q, new_data$power,
                     xmin = min(new_data$Q), xmax = max(new_data$Q), ties = mean)
  
    curve
    if(max(curve$y)<4000) {
      newx1<- max(curve$x)
    } else {
      newx1 <- approx(x = curve$y, y = curve$x, xout = 4000)$y
    }

    limits
    limits[,p] <- c(newx1)
    H_limits[, p] <- 4000
    
    # create year_month column       
    new_datax <- new_data %>% unite(month_year, water_year:month, sep="-", remove=F) 
    
    # dataframe for stats -----------------------------------------------------
    
    ## define critical period or season for Adult as all year is critical
    non_critical <- c(1:3,10:12) ## winter months
    critical <- c(4:9) ## summer months
    
    new_datax <- new_datax %>%
      mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") )
    
    
    # time stats - mid channel ------------------------------------------------
    

    ###### calculate amount of time
    
    time_stats <- new_datax %>%
      dplyr::group_by(water_year) %>%
      dplyr::mutate(Annual = sum( Q < newx1)/length(DateTime)*100) %>%
      distinct(water_year, Annual) %>%
      mutate(position= paste(PositionName), Node = NodeName)
    time_stats
    
    time_statsx <- rbind(time_statsx, time_stats)
    # startsWith(NodeName, "J", trim=T)
    ### count hours per day
    if(startsWith(NodeName, "J", trim=T)) {
      
      new_datax <- new_datax %>%
        ungroup() %>%
        group_by(month, day, water_year, ID01 = data.table::rleid(Q < newx1)) %>%
        mutate(Mid = if_else(Q < newx1, row_number(), 0L)) %>%
        mutate(position= paste(PositionName), Node = NodeName)
    } else { ## count days per month
      
      new_datax <- new_datax %>%
        ungroup() %>%
        group_by(month, water_year, ID01 =  data.table::rleid(Q < newx1)) %>%
        mutate(Mid = if_else(Q < newx1, row_number(), 0L)) %>%
        mutate(position= paste(PositionName), Node = NodeName)
    }
    
    days_data <- rbind(days_data, new_datax)
    days_data
    
  } ## end 2nd loop
  
  limits <- rbind(limits, H_limits)

  limits <- limits %>%
    mutate(Species ="Willow", Life_Stage = "Adult", Hydraulic = "StreamPower", Node = NodeName)

  write.csv(limits, paste("output_data/01_",NodeName,"_Willow_Adult_StreamPower_Q_limits.csv", sep=""))
  ## percentage time
  melt_time<-reshape2::melt(time_statsx, id=c("position", "water_year", "Node"))
  
  melt_time <- melt_time %>% 
    rename( Probability_Threshold = variable) %>%
    mutate(Species ="Willow", Life_Stage = "Adult", Hydraulic = "StreamPower", Node = NodeName)
  
  write.csv(melt_time, paste("output_data/01_", NodeName, "_Willow_Adult_StreamPower_time_stats.csv", sep=""))
  
  ### days per month
  days_data <- select(days_data, c(Q, month, water_year, day, ID01, Mid, position, DateTime, Node) )# all probs

  melt_data<-reshape2::melt(days_data, id=c("ID01", "day", "month", "Q", "water_year", "position", "Node", "DateTime"))
  melt_data <- melt_data %>% rename(consec_hours = value) %>%
    select(-variable)
  head(melt_data)
  

  
  if(startsWith(NodeName, "J", trim=T)) {
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
  
  melt_days<-reshape2::melt(total_days, id=c("month_year", "water_year", "month",  "position", "Node"))
  melt_days <- melt_days %>%
    rename(Probability_Threshold = variable, n_days = value) %>%
    mutate(Species ="Willow", Life_Stage = "Adult", Hydraulic = "StreamPower")
  
  ## save df
  write.csv(melt_days, paste("output_data/01_", NodeName, "_Willow_Adult_StreamPower_total_days_long.csv", sep="") )
  
  cat(paste("Finished Node", NodeName))
  
} ## end 1st loop


# Germination Depth ------------------------------------------------------------

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
  hyd_dep <- hyd_dep %>%
    select(DateTime, Q, date_num, month, day, water_year, contains("av.depth"))
  
  # transform m to cm
  hyd_dep<-reshape2::melt(hyd_dep, id=c("DateTime","Q",  "date_num", "month", "day", "water_year"))
  all_data <- hyd_dep %>% 
    mutate(depth_cm = as.numeric(as.character(value*100)))

  
  
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
  
  p=3
  for(p in 1:length(positions)) {
    
    # probability as a function of discharge -----------------------------------

    new_data <- all_data %>% 
      filter(variable  == positions[p])


    ## define position
    PositionName <- str_split(positions[p], "_", 3)[[1]]
    PositionName <- PositionName[2]
    
    curve <- spline(new_data$Q, new_data$depth_cm,
                     xmin = min(new_data$Q), xmax = max(new_data$Q), ties = mean)
  

    ## main channel values
    if(min(curve$y)>5) {
      newx1 <- try(min(curve$x), silent=T)
    } else {
      newx1 <- try(approx(x = curve$y, y = curve$x, xout = 5)$y, silent =T)
    }

    ## MAKE DF OF Q LIMITS
    newx1
    limits[,p] <- c(newx1)
    H_limits[, p] <- 5
    
    # create year_month column       
    new_datax <- new_data %>% unite(month_year, water_year:month, sep="-", remove=F) 
    
    # dataframe for stats -----------------------------------------------------
    
    ## define critical period or season for Adult as all year is critical
    non_critical <- c(1:3,10:12) ## winter months
    critical <- c(4:9) ## summer months
    
    new_datax <- new_datax %>%
      mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") )
    
    
    # time stats - mid channel ------------------------------------------------
    

    ###### calculate amount of time
    
    time_stats <- new_datax %>%
      dplyr::group_by(water_year, season) %>%
      dplyr::mutate(Seasonal = sum(Q > newx1)/length(DateTime)*100) %>%
      distinct(water_year, Seasonal) %>%
      mutate(position= paste(PositionName), Node = NodeName)

    
    time_statsx <- rbind(time_statsx, time_stats)
    
    if(startsWith(NodeName, "J", trim=T)) {
      
      new_datax <- new_datax %>%
        ungroup() %>%
        group_by(month, day, water_year, ID01 = data.table::rleid(Q > newx1)) %>%
        mutate(Mid = if_else(Q > newx1, row_number(), 0L)) %>%
        mutate(position= paste(PositionName), Node = NodeName)
    } else { ## count days per month
      
      new_datax <- new_datax %>%
        ungroup() %>%
        group_by(month, water_year, ID01 =  data.table::rleid(Q > newx1)) %>%
        mutate(Mid = if_else(Q > newx1, row_number(), 0L)) %>%
        mutate(position= paste(PositionName), Node = NodeName)
    }

    
    days_data <- rbind(days_data, new_datax)
    
    
  } ## end 2nd loop
  
  limits <- rbind(limits, H_limits)

  limits <- limits %>%
    mutate(Species ="Willow", Life_Stage = "Germination", Hydraulic = "Depth", Node = NodeName)
  
  write.csv(limits, paste("output_data/01_",NodeName,"_Willow_Germination_Depth_Q_limits.csv", sep=""))
  ## percentage time
  melt_time<-reshape2::melt(time_statsx, id=c("season", "position", "water_year", "Node"))
  melt_time <- melt_time %>% 
    rename( Probability_Threshold = variable) %>%
    mutate(Species ="Willow", Life_Stage = "Germination", Hydraulic = "Depth", Node = NodeName)
  head(melt_time)
  write.csv(melt_time, paste("output_data/01_", NodeName, "_Willow_Germination_Depth_time_stats.csv", sep=""))
  
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
    mutate(Species ="Willow", Life_Stage = "Germination", Hydraulic = "Depth")
  head(melt_days)
  ## save df
  write.csv(melt_days, paste("output_data/01_", NodeName, "_Willow_Germination_Depth_total_days_long.csv", sep="") )
  
  cat(paste("Finished Node", NodeName))
  
} ## end 1st loop
warnings()
