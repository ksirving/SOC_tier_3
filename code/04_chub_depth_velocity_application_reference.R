### chub application
## depth and velocity
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
# install.packages("pryr")
library(pryr)
getwd()


## upload chub data
fitdata <- read.csv("/Users/katieirving/Documents/git/SOC_tier_3/output_data/old_data/03_chub_adult_depth_prob_curve_data.csv")
fitdata <- rename(fitdata, depth_fit = depth_cm)

## root function
load(file="/Users/katieirving/Documents/git/flow_eco_mech/root_interpolation_function.Rdata")

## define root equation
load(file="/Users/katieirving/Documents/git/flow_eco_mech/expression_Q_limit_function.RData")


## upload hydraulic data
setwd("/Users/katieirving/Documents/git/SOC_tier_3")

## upload hydraulic data
setwd("input_data/Hydraulics/reference")

h <- list.files(pattern="hydraulic")
length(h) ## 32
h
n=2

setwd("/Users/katieirving/Documents/git/SOC_tier_3")

for(n in 1: length(h)) {
  
  hydraul <- read.csv(file=paste("input_data/Hydraulics/reference/", h[n], sep=""))
  
  ## change names and add datenum
  
  hyd_dep <- hydraul %>%
    rename(Q = q.cms) %>%
    mutate(date_num = seq(1,length(date), 1))
  
  ## format date time
  hyd_dep$DateTime<-as.POSIXct(hyd_dep$date,
                               format = "%m/%d/%Y",
                               tz = "GMT")
  head(hyd_dep)
  hyd_dep <- hyd_dep %>%
    mutate(month = month(DateTime)) %>%
    mutate(year = year(DateTime)) %>%
    mutate(day = day(DateTime)) %>%
    mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1))
  
  # # ## melt channel position data
  # hyd_shear <- hyd_dep %>%
  #   select(DateTime, Q, date_num, month, day, water_year, contains("shear") )
  
  # hyd_vel <- hyd_dep %>%
  #   select(DateTime, Q, date_num, month, day, water_year, contains("av.depth"))
  
  hyd_dep <- hyd_dep %>%
    select(DateTime, Q, date_num, month, day, water_year, contains("av.depth"))
  
  # transform m to cm
  hyd_dep<-reshape2::melt(hyd_dep, id=c("DateTime","Q",  "date_num", "month", "day", "water_year"))
  hyd_dep <- hyd_dep %>% 
    mutate(depth_cm = as.numeric(as.character(value*100))) %>%
    select(-value)
  
  # hyd_shear<-reshape2::melt(hyd_shear, id=c("DateTime","Q",  "date_num", "month", "day", "water_year"))
  # hyd_shear <- hyd_shear %>% rename(shear = value)
  
  
  ## change NAs to 0 in concrete overbanks
  # hyd_dep[is.na(hyd_dep)] <- 0
# sum(is.na(hyd_dep))
  ## use smooth spline to predict on new data set
  new_values <-smooth.spline(fitdata$depth_fit, fitdata$prob_fit)

  
  all_data <- hyd_dep %>%
    group_by(variable) %>%
    mutate(prob_fit = predict(new_values, depth_cm)$y) #%>%

  rm(hydraul)
  rm(hyd_dep)
  
  ## define positions
  positions <- unique(all_data$variable)
  positions
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

    peak <- new_data %>%
      filter(prob_fit == max(prob_fit)) #%>%

    peakQ  <- max(peak$Q)
    min_limit <- filter(new_data, Q > 0)
    min_limit <- min(min_limit$Q)

    ## find roots for each probability
    newx1 <- RootLinearInterpolant(new_data$Q, new_data$prob_fit, 0.2)
    hy_lim <- RootLinearInterpolant(new_data$depth_cm, new_data$prob_fit, 0.2)


    
    if(length(newx1)>2) {
      newx1 <- sort(newx1)[c(1,length(newx1))]
      hy_lim <- sort(hy_lim)[c(1,length(hy_lim))]
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
      mutate(season = ifelse(month %in% non_critical, "Winter", "Summer") )
    
    
    # time stats - mid channel ------------------------------------------------
    
    ## Main channel curves
    
    thresh <- expression_Q(newx1, peakQ) 
    thresh <-as.expression(do.call("substitute", list(thresh[[1]], list(limit = as.name("newx1")))))

    ###### calculate amount of time
    # sum(new_data$Q < 1)/length(new_data$DateTime)*100
    # sum(new_data$depth_cm < 3)/length(new_data$DateTime)*100
    # range(new_data$depth_cm)
    # test <- filter(new_data, depth_cm > 3)
    # unique(test$month)

    time_stats <- new_datax %>%
      dplyr::group_by(water_year) %>%
      dplyr::mutate(Annual = sum(eval(thresh))/length(DateTime)*100) %>%
      ungroup() %>%
      dplyr::group_by(water_year, season) %>%
      dplyr::mutate(Seasonal = sum(eval(thresh))/length(DateTime)*100) %>%
      distinct(water_year, Annual, Seasonal) %>%
      mutate(position= paste(PositionName), Node = NodeName)
    
    time_stats
    
    time_statsx <- rbind(time_statsx, time_stats)
    
    ### count days per month
    if(startsWith(NodeName, "J", trim=T)) {
      
      new_datax <- new_datax %>%
        ungroup() %>%
        group_by(month, day, water_year, ID01 = data.table::rleid(eval(thresh))) %>%
        mutate(Mid = if_else(eval(thresh), row_number(), 0L)) %>%
        mutate(position= paste(PositionName), Node = NodeName)
    } else { ## count days per month
      
      new_datax <- new_datax %>%
        ungroup() %>%
        group_by(month, water_year, ID01 =  data.table::rleid(eval(thresh))) %>%
        mutate(Mid = if_else(eval(thresh), row_number(), 0L)) %>%
        mutate(position= paste(PositionName), Node = NodeName)
    }
    
    
    days_data <- rbind(days_data, new_datax)
    
    
  } ## end 2nd loop
  
  limits <- rbind(limits, H_limits)

  limits <- limits %>%
    mutate(Species ="Chub", Life_Stage = "Adult", Hydraulic = "Depth", Node = NodeName)
  
  write.csv(limits, paste("output_data/04_",NodeName,"_Chub_Adult_depth_Q_limits_reference.csv", sep=""))
  ## percentage time
  melt_time<-reshape2::melt(time_statsx, id=c("season", "position", "water_year", "Node"))
  melt_time <- melt_time %>% 
    rename( Probability_Threshold = variable) %>%
    mutate(Species ="Chub", Life_Stage = "Adult", Hydraulic = "Depth", Node = NodeName)

  write.csv(melt_time, paste("output_data/04_", NodeName, "_Chub_Adult_depth_time_stats_reference.csv", sep=""))
  
  ### days per month
  days_data <- select(days_data, c(Q, month, water_year, day, ID01, Mid, position, DateTime, Node) )# all probs
  
  melt_data<-reshape2::melt(days_data, id=c("ID01", "day", "month", "water_year", "Q", "position", "Node", "DateTime"))
  melt_data <- rename(melt_data, Probability_Threshold = variable, 
                      consec_hours = value)
  

  
  ## here daily data if/else, if nodename begind=s with an L = daily, if J = hourly
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
    mutate(season = ifelse(month %in% non_critical, "Winter", "Summer") )
  
  melt_days<-reshape2::melt(total_days, id=c("month_year", "water_year", "month", "season", "position", "Node"))
  melt_days <- melt_days %>%
    rename(Probability_Threshold = variable, n_days = value) %>%
    mutate(Species ="Chub", Life_Stage = "Adult", Hydraulic = "Depth")

  ## save df
  write.csv(melt_days, paste("output_data/04_", NodeName, "_Chub_Adult_depth_total_days_long_reference.csv", sep="") )
  
  cat(paste("Finished Node", NodeName))
  
} ## end 1st loop

# Velocity ----------------------------------------------------------------
rm(all_data)

setwd("/Users/katieirving/Documents/git/SOC_tier_3")
## upload chub data
# fitdata <- read.csv("output_data/03_chub_adult_velocity_prob_curve_data.csv")
pathname <- "input_data/Hydraulics/reference"

for(n in 1: length(h)) {
  ## upload chub data
  fitdata <- read.csv("output_data/old_data/03_chub_adult_velocity_prob_curve_data.csv")
  
  # ## root function
  # load(file="models_functions/root_interpolation_function.Rdata")
  # 
  # ## define root equation
  # load(file="models_functions/expression_Q_limit_function.RData")
  # 
  # pathname <- "input_data/Hydraulics/"
  # 
  # h <- list.files(path= pathname,pattern="hydraulic")
  # length(h) ## 32

  
  hydraul <- read.csv(file=paste(pathname, h[n], sep=""))
  
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
  unique(hyd_vel$variable)
  
  # hyd_shear<-reshape2::melt(hyd_shear, id=c("DateTime","Q",  "date_num", "month", "day", "water_year"))
  # hyd_shear <- hyd_shear %>% rename(shear = value)
  
  hyd_vel<-reshape2::melt(hyd_vel, id=c("DateTime","Q",  "date_num", "month", "day", "water_year"))
  hyd_vel <- hyd_vel %>% rename(vel_m = value)
  
  ### get depth df for min limit
  hyd_dep <- hyd_dep %>%
    select(DateTime, Q, date_num, month, day, water_year, contains("av.depth"))
  
  # transform m to cm
  hyd_dep<-reshape2::melt(hyd_dep, id=c("DateTime","Q",  "date_num", "month", "day", "water_year"))
  hyd_dep <- hyd_dep %>%
    mutate(depth_cm = as.numeric(as.character(value*100)))
  hyd_dep <- select(hyd_dep, date_num, depth_cm)

  
  ## join depth data to vel df
  hyd_vel <- left_join(hyd_vel, hyd_dep, by="date_num")


  ## change NAs to 0 in concrete overbanks
  hyd_vel[is.na(hyd_vel)] <- 0

  ## use smooth spline to predict on new data set
  new_values <-smooth.spline(fitdata$velocity_fit, fitdata$prob_fit)
  
  all_data <- hyd_vel %>%
    group_by(variable) %>%
    mutate(prob_fit = predict(new_values, vel_m)$y) 
  
  rm(hydraul)
  rm(hyd_vel)
  
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
    #### fix min limit - add depth data in main df
    min_limit <- filter(new_data, Q > 0)
    min_limit <- min(min_limit$Q)
    min_limit

    ## find roots for each probability
    newx1 <- RootLinearInterpolant(new_data$Q, new_data$prob_fit, 0.2)
    hy_lim <- RootLinearInterpolant(new_data$vel_m, new_data$prob_fit, 0.2)
 

    if(length(newx1)>2) {
      newx1 <- sort(newx1)[c(1,length(newx1))]
      hy_lim <- sort(hy_lim)[c(1,length(hy_lim))]
    }
    
    
    ## MAKE DF OF Q LIMITS
    
    limits[,p] <- c(newx1)
    H_limits[,p] <- c(hy_lim)
    
    # create year_month column       
    new_data <- new_data %>% unite(month_year, water_year:month, sep="-", remove=F) 

    # dataframe for stats -----------------------------------------------------
    
    ## define critical period or season for seedling as all year is critical
    non_critical <- c(1:3,10:12) ## winter months
    critical <- c(4:9) ## summer months
    
    new_data <- new_data %>%
      mutate(season = ifelse(month %in% non_critical, "Winter", "Summer") )
    
    
    # time stats - mid channel ------------------------------------------------
    
    ## Main channel curves

    thresh <- expression_Q(newx1, peakQ) 
    thresh <-as.expression(do.call("substitute", list(thresh[[1]], list(limit = as.name("newx1")))))
    
    ###### calculate amount of time
    
    time_stats <- new_data %>%
      dplyr::group_by(water_year) %>%
      dplyr::mutate(Annual = sum(eval(thresh))/length(DateTime)*100) %>%
      ungroup() %>%
      dplyr::group_by(water_year, season) %>%
      dplyr::mutate(Seasonal = sum(eval(thresh))/length(DateTime)*100) %>%
      distinct(water_year, Annual, Seasonal) %>%
      mutate(position= paste(PositionName), Node = NodeName)
    
    
    time_statsx <- rbind(time_statsx, time_stats)
    rm(time_stats)
    ### count days per month
    if(startsWith(NodeName, "J", trim=T)) {
      
      new_data <- new_data %>%
        ungroup() %>%
        group_by(month, day, water_year, ID01 = data.table::rleid(eval(thresh))) %>%
        mutate(Mid = if_else(eval(thresh), row_number(), 0L)) %>%
        mutate(position= paste(PositionName), Node = NodeName)
    } else { ## count days per month
      
      new_data <- new_data %>%
        ungroup() %>%
        group_by(month, water_year, ID01 =  data.table::rleid(eval(thresh))) %>%
        mutate(Mid = if_else(eval(thresh), row_number(), 0L)) %>%
        mutate(position= paste(PositionName), Node = NodeName)
    }
    
    
    days_data <- rbind(days_data, new_data)

    
  } ## end 2nd loop
  rm(all_data)

  limits <- rbind(limits, H_limits)
  limits
  limits <- limits %>%
    mutate(Species ="Chub", Life_Stage = "Adult", Hydraulic = "Velocity", Node = NodeName)
  
  write.csv(limits, paste("output_data/04_",NodeName,"_Chub_Adult_Velocity_Q_limits.csv", sep=""))
  ## percentage time
  melt_time<-reshape2::melt(time_statsx, id=c("season", "position", "water_year", "Node"))
  melt_time <- melt_time %>% 
    rename( Probability_Threshold = variable) %>%
    mutate(Species ="Chub", Life_Stage = "Adult", Hydraulic = "Velocity", Node = NodeName)

  write.csv(melt_time, paste("output_data/04_", NodeName, "_Chub_Adult_Velocity_time_stats.csv", sep=""))
  rm(melt_time)
  rm(time_statsx)
  ### days per month
  days_data <- select(days_data, c(Q, month, water_year, day, ID01, Mid, position, DateTime, Node) )# all probs
  
  melt_data<-reshape2::melt(days_data, id=c("ID01", "day", "month", "water_year", "Q", "position", "Node", "DateTime"))
  melt_data <- rename(melt_data, Probability_Threshold = variable, 
                      consec_hours = value)
  rm(days_data)
  
  ## count how many full days i.e. 24 hours
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
  
  rm(melt_data)
     
  total_days$month_year <-  zoo::as.yearmon(total_days$month_year)
  
  total_days <- total_days %>%
    mutate(season = ifelse(month %in% non_critical, "Winter", "Summer") )
  
  melt_days<-reshape2::melt(total_days, id=c("month_year", "water_year", "month", "season", "position", "Node"))
  melt_days <- melt_days %>%
    rename(Probability_Threshold = variable, n_days = value) %>%
    mutate(Species ="Chub", Life_Stage = "Adult", Hydraulic = "Velocity")
  
  ## save df
  write.csv(melt_days, paste("output_data/04_", NodeName, "_Chub_Adult_Velocity_total_days_long_reference.csv", sep="") )
  
  cat(paste("Finished Node", NodeName))
  rm(list = ls())
  
} ## end 1st loop
