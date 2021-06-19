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


## upload model
setwd("/Users/katieirving/Documents/git/SOC_tier_3")
load(file = "models_functions/chub_depth_mod.RData")
summary(chub_depth_mod)

## root function
load(file="/Users/katieirving/Documents/git/flow_eco_mech/root_interpolation_function.Rdata")

## define root equation
load(file="/Users/katieirving/Documents/git/flow_eco_mech/expression_Q_limit_function.RData")


## upload hydraulic data
setwd("/Users/katieirving/Documents/git/SOC_tier_3")

## upload hydraulic data
setwd("input_data/Hydraulics/reference")

h <- list.files(pattern="hydraulic")
length(h) ## 51
h
n=2
h[n]

min_limit <- 0.1 ## define min limit for depth. can be changed

setwd("/Users/katieirving/Documents/git/SOC_tier_3")

for(n in 1: length(h)) {
  
  hydraul <- read.csv(file=paste("input_data/Hydraulics/reference/", h[n], sep=""))
  head(hydraul)
  
  ## define nodename
  NodeName <- str_split(h[n], "_", 3)[[1]]
  NodeName <- NodeName[1]
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
    mutate(Depth_cm = as.numeric(as.character(value*100)))
  # head(hyd_dep)
  
  
  ## predict values
  
  all_data <- hyd_dep %>%
    mutate(prob_fit = predict(chub_depth_mod, newdata = hyd_dep, type="response")) %>%
    mutate(prob_fit = ifelse(prob_fit<=0, 0, prob_fit)) %>%## predicts negative percentages - cut off at 0 for quick fix
   mutate(prob_fit = ifelse(Depth_cm == 0, 0, prob_fit)) ## when depth is 0, prob is 0
  
  non_critical <- c(1:3,10:12) ## winter months
  critical <- c(4:9) ## summer months
  
  all_datax <- all_data %>%
    mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") ) %>%
    separate(variable, into = c("Hydraulic", "Position"), sep="_", remove = F)

  prob_stats <- all_datax %>%
    # filter(season == "critical") %>%
    group_by(water_year,variable, Hydraulic, Position, season) %>%
    summarise(MeanProbability = mean(prob_fit), SDProbability = sd(prob_fit)) %>%
    mutate(Species = "Chub", LifeStage = "Adult", Node = NodeName)
  
  write.csv(prob_stats, paste("output_data/04_", NodeName, "_Chub_Adult_Depth_prob_stats_reference.csv", sep=""))
    
  
  head(all_data)
  ## define positions
  positions <- unique(all_data$variable)
  # positions 
  ## Q Limits
  limits <- as.data.frame(matrix(ncol=length(positions), nrow=6)) 
  limits$Type<-c("Q_limitLow", "Q_limitLow","Q_limitMed", "Q_limitMed", "Q_limitHigh", "Q_limitHigh")
  
  H_limits <- as.data.frame(matrix(ncol=length(positions), nrow=6)) 
  H_limits$Type<-c("Hydraulic_limitLow","Hydraulic_limitLow", "Hydraulic_limitMed", 
                   "Hydraulic_limitMed", "Hydraulic_limitHigh", "Hydraulic_limitHigh")
  
  ## calculation
  Q_Calc <- as.data.frame(matrix(nrow=length(positions), ncol=3 ))
  
  names(Q_Calc) <- c("Low", "Medium", "High")
  
  time_statsx <- NULL
  days_data <- NULL
  
  p=2
  for(p in 1:length(positions)) {
    
    # probability as a function of discharge -----------------------------------
    
    new_data <- all_data %>% 
      filter(variable  == positions[p])
    
    new_data <- na.omit(new_data)
    ## define position
    PositionName <- str_split(positions[p], "_", 3)[[1]]
    PositionName <- PositionName[2]
    
    # head(new_data)
    # PositionName
    peak <- new_data %>%
      filter(prob_fit == max(na.omit(prob_fit))) #%>%
    # range(new_data$Depth_cm)
    # range(new_data$prob_fit)
    # write.csv(new_data, "output_data/02_example_node_for_figures.csv")
    # peakQ
    peakQ  <- max(peak$Q)
    min_limit <- filter(new_data, Q > 0)
    min_limit <- min(min_limit$Q)
    
    ## find roots for each probability - Low
 
    
    if(max(new_data$prob_fit) < 0.25) {
      newx1Low <- NA
      hy_lim1Low <- NA
    } else {
      newx1Low <- RootLinearInterpolant(new_data$Q, new_data$prob_fit, 0.250)
      hy_lim1Low <- RootLinearInterpolant(new_data$Depth_cm, new_data$prob_fit, 0.250)
    }
    
    if(length(newx1Low)>2) {
      newx1Low <- c(newx1Low[1], newx1Low[length(newx1Low)])
      hy_lim1Low<- c(hy_lim1Low[1], hy_lim1Low[length(hy_lim1Low)])
    }
    
    ## find roots for each probability - medium

    if(max(new_data$prob_fit) < 0.5) {
      newx1Med <- NA
      hy_lim1Med <- NA
    } else {
      newx1Med <- RootLinearInterpolant(new_data$Q, new_data$prob_fit, 0.50)
      hy_lim1Med <- RootLinearInterpolant(new_data$Depth_cm, new_data$prob_fit, 0.50)
    }
    
    if(length(newx1Med)>2) {
      newx1Med <- c(newx1Med[1], newx1Med[length(newx1Med)])
      hy_lim1Med<- c(hy_lim1Med[1], hy_lim1Med[length(hy_lim1Med)])
    }
    
    ## find roots for each probability - High
   
    if(max(new_data$prob_fit) < 0.75) {
      newx1High <- NA
      hy_lim1High <- NA
    } else {
      newx1High <- RootLinearInterpolant(new_data$Q, new_data$prob_fit, 0.75)
      hy_lim1High <- RootLinearInterpolant(new_data$Depth_cm, new_data$prob_fit, 0.75)
    }
    
    if(length(newx1High)>2) {
      newx1High <- c(newx1High[1], newx1High[length(newx1High)])
      hy_lim1High<- c(hy_lim1High[1], hy_lim1High[length(hy_lim1High)])
    }
    
    ## MAKE DF OF Q LIMITS
    c(newx1Low, newx1Med, newx1High)
    limits[,p] <- c(newx1Low[1],newx1Low[2], newx1Med[1],newx1Med[2], newx1High[1], newx1High[2])
    H_limits[,p] <- c(hy_lim1Low[1],hy_lim1Low[2],hy_lim1Med[1], hy_lim1Med[2], hy_lim1High[1], hy_lim1High[2])
    # limits
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
    threshLow <- expression_Q(newx1Low, peakQ) 
    threshLow <-as.expression(do.call("substitute", list(threshLow[[1]], list(limit = as.name("newx1Low")))))
    # threshLow
    threshMed <- expression_Q(newx1Med, peakQ) 
    threshMed <-as.expression(do.call("substitute", list(threshMed[[1]], list(limit = as.name("newx1Med")))))
    # threshMed
    threshHigh <- expression_Q(newx1High, peakQ) 
    threshHigh <-as.expression(do.call("substitute", list(threshHigh[[1]], list(limit = as.name("newx1High")))))
    
    Q_Calc[p,] <- c(paste(threshLow), paste(threshMed), paste(threshHigh))
    ###### calculate amount of time
    
    time_stats <- new_datax %>%
      dplyr::group_by(water_year, season) %>%
      dplyr::mutate(Low = sum(eval(threshLow))/length(DateTime)*100) %>%
      dplyr::mutate(Medium = sum(eval(threshMed))/length(DateTime)*100) %>%
      dplyr::mutate(High = sum(eval(threshHigh))/length(DateTime)*100) %>%
      distinct(water_year, Medium, High, Low) %>%
      mutate(position= paste(PositionName), Node = NodeName)
    time_statsx
    
    time_statsx <- rbind(time_statsx, time_stats)
    # startsWith(NodeName, "J", trim=T)
    ### count hours per day
      
      new_datax <- new_datax %>%
        ungroup() %>%
        group_by(month, day, water_year, ID01 = data.table::rleid(eval(threshLow))) %>%
        mutate(Low = if_else(eval(threshLow), row_number(), 0L)) %>%
        ungroup() %>%
        group_by(month, day, water_year, ID02 = data.table::rleid(eval(threshMed))) %>%
        mutate(Medium = if_else(eval(threshMed), row_number(), 0L)) %>%
        ungroup %>%
        group_by(month, day, water_year, ID03 = data.table::rleid(eval(threshHigh))) %>%
        mutate(High = if_else(eval(threshHigh), row_number(), 0L)) %>%
        mutate(position= paste(PositionName), Node = NodeName)
      new_datax
    
    days_data <- rbind(days_data, new_datax)
    
    
  } ## end 2nd loop
  
  Q_Calc$Position <- positions
  # Q_Calc
  Q_Calc <- Q_Calc %>%
    mutate(Species ="Chub", Life_Stage = "Adult", Hydraulic = "Depth", Node = NodeName)
  
  write.csv(Q_Calc, paste("output_data/04_",NodeName,"_Chub_Adult_Depth_Q_calculation_reference.csv", sep=""))
  
  
  limits <- rbind(limits, H_limits)
  limits
  ## note that 0.1 upper/lower limit is max/min Q to adhere to 0.1 bound
  limits <- limits %>%
    mutate(Species ="Chub", Life_Stage = "Adult", Hydraulic = "Depth", Node = NodeName)
  
  
  write.csv(limits, paste("output_data/04_",NodeName,"_Chub_Adult_Depth_Q_limits_reference.csv", sep=""))
  ## percentage time
  melt_time<-reshape2::melt(time_statsx, id=c("season", "position", "water_year", "Node"))
  melt_time <- melt_time %>% 
    rename( Probability_Threshold = variable) %>%
    mutate(Species ="Chub", Life_Stage = "Adult", Hydraulic = "Depth", Node = NodeName)
  
  write.csv(melt_time, paste("output_data/04_", NodeName, "_Chub_Adult_Depth_time_stats_reference.csv", sep=""))
  
  ### days per month
  head(days_data)
  days_data <- select(days_data, -c(variable, value, Depth_cm, prob_fit, date_num, month_year, season)) #c(Q, month, water_year, day, ID01, Mid, position, DateTime, Node) )# all probs
  
  melt_data<-reshape2::melt(days_data, id=c("ID01", "ID02", "ID03", "day", "month", "water_year", "Q", "position", "Node", "DateTime"))
  melt_data <- rename(melt_data, Probability_Threshold = variable, 
                      consec_hours = value)


  ## count how many full days i.e. 24 hours
  total_days01 <- melt_data %>% 
    filter(Probability_Threshold == "Low") %>% 
    group_by(ID01, day, month, water_year, position) %>%
    summarise(n_hours = max(consec_hours))  %>%
    mutate(n_days_low = ifelse(n_hours >= 24, 1, 0)) # %>%
  
  ## count the number of days in each month
  total_days_per_month01 <- total_days01 %>%
    group_by(month, water_year, position) %>%
    summarise(days_per_month_low = sum(n_days_low))
  
  
  total_days02 <- melt_data %>% 
    filter(Probability_Threshold == "Medium") %>% 
    group_by(ID02, day, month, water_year, position) %>%
    summarise(n_hours = max(consec_hours))  %>%
    mutate(n_days_medium = ifelse(n_hours >= 24, 1, 0)) # %>%
  
  total_days_per_month02 <- total_days02 %>%
    group_by(month, water_year, position) %>%
    summarise(days_per_month_medium = sum(n_days_medium))
  
  # total_days_per_month02
  
  total_days03 <- melt_data %>% 
    filter(Probability_Threshold == "High") %>% 
    group_by(ID03, day, month, water_year, position) %>%
    summarise(n_hours = max(consec_hours))  %>%
    mutate(n_days_high = ifelse(n_hours >= 24, 1, 0)) # %>%
  
  total_days_per_month03 <- total_days03 %>%
    group_by(month, water_year, position) %>%
    summarise(days_per_month_high = sum(n_days_high))
  
  ## combine all thresholds
  total_days <- cbind( total_days_per_month01,total_days_per_month02[,4], total_days_per_month03[,4])
  
  
  # # create year_month column       
  total_days <- ungroup(total_days) %>%
    unite(month_year, water_year:month, sep="-", remove=F) %>%
    mutate(Node= paste(NodeName)) #%>%
  
  
  ## change names of columns
  total_days <- rename(total_days, Low = days_per_month_low, Medium = days_per_month_medium, High = days_per_month_high)
  
  total_days$month_year <-  zoo::as.yearmon(total_days$month_year)
  
  total_days <- total_days %>%
    mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") )
  
  melt_days<-reshape2::melt(total_days, id=c("month_year", "water_year", "month", "season", "position", "Node"))
  melt_days <- melt_days %>%
    rename(Probability_Threshold = variable, n_days = value) %>%
    mutate(Species ="Chub", Life_Stage = "Adult", Hydraulic = "Depth")
  
  ## save df
  write.csv(melt_days, paste("output_data/04_", NodeName, "_Chub_Adult_Depth_total_days_long_reference.csv", sep="") )
  
  cat(paste("Finished Node", NodeName))
  
} ## end 1st loop


# Velocity ----------------------------------------------------------------
rm(all_data)

## root function
load(file="/Users/katieirving/Documents/git/flow_eco_mech/root_interpolation_function.Rdata")

## define root equation
load(file="/Users/katieirving/Documents/git/flow_eco_mech/expression_Q_limit_function.RData")

## upload chub data
# fitdata <- read.csv("output_data/03_chub_adult_velocity_prob_curve_data.csv")
pathname <- "input_data/Hydraulics/reference/"
## upload hydraulic data
setwd("input_data/Hydraulics/reference")

h <- list.files(pattern="hydraulic")
length(h) ## 51
h
setwd("/Users/katieirving/Documents/git/SOC_tier_3")

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
  
  ## define nodename
  NodeName <- str_split(h[n], "_", 3)[[1]]
  NodeName <- NodeName[1]
  
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
  # unique(hyd_vel$variable)
  
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
  
  non_critical <- c(1:3,10:12) ## winter months
  critical <- c(4:9) ## summer months
  
  all_datax <- all_data %>%
    mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") ) %>%
    separate(variable, into = c("Hydraulic", "Position"), sep="_", remove = F)
  
  prob_stats <- all_datax %>%
    # filter(season == "critical") %>%
    group_by(water_year,variable, Hydraulic, Position, season) %>%
    summarise(MeanProbability = mean(prob_fit), SDProbability = sd(prob_fit)) %>%
    mutate(Species = "Chub", LifeStage = "Adult", Node = NodeName)
  
  write.csv(prob_stats, paste("output_data/04_", NodeName, "_Chub_Adult_Velocity_prob_stats_reference.csv", sep=""))
  

  
  ## define positions
  positions <- unique(all_data$variable)
  
  ## Q Limits
  limits <- as.data.frame(matrix(ncol=length(positions), nrow=6)) 
  limits$Type<-c("Q_limitLow", "Q_limitLow","Q_limitMed", "Q_limitMed", "Q_limitHigh", "Q_limitHigh")
  
  H_limits <- as.data.frame(matrix(ncol=length(positions), nrow=6)) 
  H_limits$Type<-c("Hydraulic_limitLow","Hydraulic_limitLow", "Hydraulic_limitMed", 
                   "Hydraulic_limitMed", "Hydraulic_limitHigh", "Hydraulic_limitHigh")

  ## calculation
  Q_Calc <- as.data.frame(matrix(nrow=length(positions), ncol=3 ))
  
  names(Q_Calc) <- c("Low", "Medium", "High")
  
  time_statsx <- NULL
  days_data <- NULL
  
  p=2
  for(p in 1:length(positions)) {
    
    # probability as a function of discharge -----------------------------------
    
    new_data <- all_data %>% 
      filter(variable  == positions[p])
    
    new_data <- na.omit(new_data)
    ## define position
    PositionName <- str_split(positions[p], "_", 3)[[1]]
    PositionName <- PositionName[2]
    
    # head(new_data)
    # PositionName
    peak <- new_data %>%
      filter(prob_fit == max(na.omit(prob_fit))) #%>%
    # range(new_data$Depth_cm)
    # range(new_data$prob_fit)
    # write.csv(new_data, "output_data/02_example_node_for_figures.csv")
    # peakQ
    peakQ  <- max(peak$Q)
    min_limit <- filter(new_data, Q > 0)
    min_limit <- min(min_limit$Q)
    
    ## find roots for each probability - Low
    
    
    if(max(new_data$prob_fit) < 0.1) {
      newx1Low <- NA
      hy_lim1Low <- NA
    } else {
      newx1Low <- RootLinearInterpolant(new_data$Q, new_data$prob_fit, 0.1)
      hy_lim1Low <- RootLinearInterpolant(new_data$vel_m, new_data$prob_fit, 0.1)
    }
    
    if(length(newx1Low)>2) {
      newx1Low <- c(newx1Low[1], newx1Low[length(newx1Low)])
      hy_lim1Low<- c(hy_lim1Low[1], hy_lim1Low[length(hy_lim1Low)])
    }
    
    ## find roots for each probability - medium
    
    if(max(new_data$prob_fit) < 0.2) {
      newx1Med <- NA
      hy_lim1Med <- NA
    } else {
      newx1Med <- RootLinearInterpolant(new_data$Q, new_data$prob_fit, 0.2)
      hy_lim1Med <- RootLinearInterpolant(new_data$vel_m, new_data$prob_fit, 0.2)
    }
    
    if(length(newx1Med)>2) {
      newx1Med <- c(newx1Med[1], newx1Med[length(newx1Med)])
      hy_lim1Med<- c(hy_lim1Med[1], hy_lim1Med[length(hy_lim1Med)])
    }
    
    ## find roots for each probability - High
    
    if(max(new_data$prob_fit) < 0.3) {
      newx1High <- NA
      hy_lim1High <- NA
    } else {
      newx1High <- RootLinearInterpolant(new_data$Q, new_data$prob_fit, 0.3)
      hy_lim1High <- RootLinearInterpolant(new_data$vel_m, new_data$prob_fit, 0.3)
    }
    
    if(length(newx1High)>2) {
      newx1High <- c(newx1High[1], newx1High[length(newx1High)])
      hy_lim1High<- c(hy_lim1High[1], hy_lim1High[length(hy_lim1High)])
    }
    
    ## MAKE DF OF Q LIMITS
    
    limits[,p] <- c(newx1Low[1],newx1Low[2], newx1Med[1],newx1Med[2], newx1High[1], newx1High[2])
    H_limits[,p] <- c(hy_lim1Low[1],hy_lim1Low[2],hy_lim1Med[1], hy_lim1Med[2], hy_lim1High[1], hy_lim1High[2])
    # limits
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
    threshLow <- expression_Q(newx1Low, peakQ) 
    threshLow <-as.expression(do.call("substitute", list(threshLow[[1]], list(limit = as.name("newx1Low")))))
    
    threshMed <- expression_Q(newx1Med, peakQ) 
    threshMed <-as.expression(do.call("substitute", list(threshMed[[1]], list(limit = as.name("newx1Med")))))
    
    threshHigh <- expression_Q(newx1High, peakQ) 
    threshHigh <-as.expression(do.call("substitute", list(threshHigh[[1]], list(limit = as.name("newx1High")))))
    
    
    Q_Calc[p,] <- c(paste(threshLow), paste(threshMed), paste(threshHigh))
    ###### calculate amount of time
    
    time_stats <- new_datax %>%
      dplyr::group_by(water_year, season) %>%
      dplyr::mutate(Low = sum(eval(threshLow))/length(DateTime)*100) %>%
      dplyr::mutate(Medium = sum(eval(threshMed))/length(DateTime)*100) %>%
      dplyr::mutate(High = sum(eval(threshHigh))/length(DateTime)*100) %>%
      distinct(water_year, Medium, High, Low) %>%
      mutate(position= paste(PositionName), Node = NodeName)
    time_statsx
    
    time_statsx <- rbind(time_statsx, time_stats)
    # startsWith(NodeName, "J", trim=T)
    ### count hours per day
    
    new_datax <- new_datax %>%
      ungroup() %>%
      group_by(month, day, water_year, ID01 = data.table::rleid(eval(threshLow))) %>%
      mutate(Low = if_else(eval(threshLow), row_number(), 0L)) %>%
      ungroup() %>%
      group_by(month, day, water_year, ID02 = data.table::rleid(eval(threshMed))) %>%
      mutate(Medium = if_else(eval(threshMed), row_number(), 0L)) %>%
      ungroup %>%
      group_by(month, day, water_year, ID03 = data.table::rleid(eval(threshHigh))) %>%
      mutate(High = if_else(eval(threshHigh), row_number(), 0L)) %>%
      mutate(position= paste(PositionName), Node = NodeName)
    new_datax
    
    days_data <- rbind(days_data, new_datax)
    
    
  } ## end 2nd loop
  
  Q_Calc$Position <- positions
  
  Q_Calc <- Q_Calc %>%
    mutate(Species ="Chub", Life_Stage = "Adult", Hydraulic = "Velocity", Node = NodeName)
  
  write.csv(Q_Calc, paste("output_data/04_",NodeName,"_Chub_Adult_Velocity_Q_calculation_reference.csv", sep=""))
  
  
  limits <- rbind(limits, H_limits)
  limits
  ## note that 0.1 upper/lower limit is max/min Q to adhere to 0.1 bound
  limits <- limits %>%
    mutate(Species ="Chub", Life_Stage = "Adult", Hydraulic = "Velocity", Node = NodeName)
  
  
  write.csv(limits, paste("output_data/04_",NodeName,"_Chub_Adult_Velocity_Q_limits_reference.csv", sep=""))
  ## percentage time
  melt_time<-reshape2::melt(time_statsx, id=c("season", "position", "water_year", "Node"))
  melt_time <- melt_time %>% 
    rename( Probability_Threshold = variable) %>%
    mutate(Species ="Chub", Life_Stage = "Adult", Hydraulic = "Velocity", Node = NodeName)
  
  write.csv(melt_time, paste("output_data/04_", NodeName, "_Chub_Adult_Velocity_time_stats_reference.csv", sep=""))
  
  ### days per month
  head(days_data)
  days_data <- select(days_data, -c(variable, depth_cm, vel_m, prob_fit, date_num, month_year, season)) #c(Q, month, water_year, day, ID01, Mid, position, DateTime, Node) )# all probs
  
  melt_data<-reshape2::melt(days_data, id=c("ID01", "ID02", "ID03", "day", "month", "water_year", "Q", "position", "Node", "DateTime"))
  melt_data <- rename(melt_data, Probability_Threshold = variable, 
                      consec_hours = value)
  
  
  ## count how many full days i.e. 24 hours
  total_days01 <- melt_data %>% 
    filter(Probability_Threshold == "Low") %>% 
    group_by(ID01, day, month, water_year, position) %>%
    summarise(n_hours = max(consec_hours))  %>%
    mutate(n_days_low = ifelse(n_hours >= 24, 1, 0)) # %>%
  
  ## count the number of days in each month
  total_days_per_month01 <- total_days01 %>%
    group_by(month, water_year, position) %>%
    summarise(days_per_month_low = sum(n_days_low))
  
  
  total_days02 <- melt_data %>% 
    filter(Probability_Threshold == "Medium") %>% 
    group_by(ID02, day, month, water_year, position) %>%
    summarise(n_hours = max(consec_hours))  %>%
    mutate(n_days_medium = ifelse(n_hours >= 24, 1, 0)) # %>%
  
  total_days_per_month02 <- total_days02 %>%
    group_by(month, water_year, position) %>%
    summarise(days_per_month_medium = sum(n_days_medium))
  
  # total_days_per_month02
  
  total_days03 <- melt_data %>% 
    filter(Probability_Threshold == "High") %>% 
    group_by(ID03, day, month, water_year, position) %>%
    summarise(n_hours = max(consec_hours))  %>%
    mutate(n_days_high = ifelse(n_hours >= 24, 1, 0)) # %>%
  
  total_days_per_month03 <- total_days03 %>%
    group_by(month, water_year, position) %>%
    summarise(days_per_month_high = sum(n_days_high))
  
  ## combine all thresholds
  total_days <- cbind( total_days_per_month01,total_days_per_month02[,4], total_days_per_month03[,4])
  
  
  # # create year_month column       
  total_days <- ungroup(total_days) %>%
    unite(month_year, water_year:month, sep="-", remove=F) %>%
    mutate(Node= paste(NodeName)) #%>%
  
  
  ## change names of columns
  total_days <- rename(total_days, Low = days_per_month_low, Medium = days_per_month_medium, High = days_per_month_high)
  
  total_days$month_year <-  zoo::as.yearmon(total_days$month_year)
  
  total_days <- total_days %>%
    mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") )
  
  
  melt_days<-reshape2::melt(total_days, id=c("month_year", "water_year", "month", "season", "position", "Node"))
  melt_days <- melt_days %>%
    rename(Probability_Threshold = variable, n_days = value) %>%
    mutate(Species ="Chub", Life_Stage = "Adult", Hydraulic = "Velocity")
  
  ## save df
  write.csv(melt_days, paste("output_data/04_", NodeName, "_Chub_Adult_Velocity_total_days_long_reference.csv", sep="") )
  
  cat(paste("Finished Node", NodeName))
  # rm(list = ls())
  
} ## end 1st loop
