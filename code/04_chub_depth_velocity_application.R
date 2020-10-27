### chub application
## depth and velocity

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

## upload chub data
fitdata <- read.csv("output_data/03_chub_adult_depth_prob_curve_data.csv")

## upload hydraulics
hydraul <- read.csv("input_data/level3_hydraulic_outputs_aliso_example_J01-020.csv")

head(hydraul)
## change some names
hydraul <- hydraul %>%
  rename(Q = q.cms)

## change names and transform ft to cm
hyd_dep <- hydraul %>%
  mutate(AV_depth_cm_Slice1 = (av.depth.m_slice1)*100,
         AV_depth_cm_Slice2 = (av.depth.m_slice2)*100,
         AV_depth_cm_Slice3 = (av.depth.m_slice3)*100,
         AV_depth_cm_Slice4 = (av.depth.m_slice4)*100,
         MAX_depth_cm_Slice1 = (max.depth.m_slice1)*100,
         MAX_depth_cm_Slice2 = (max.depth.m_slice2)*100,
         MAX_depth_cm_Slice3 = (max.depth.m_slice3)*100,
         MAX_depth_cm_Slice4 = (max.depth.m_slice4)*100) %>%
  mutate(date_num = seq(1,length(date), 1))

head(hyd_dep)


head(hyd_dep)
head(fitdata)
## format date time
hyd_dep$DateTime<-as.POSIXct(hyd_dep$date,
                             format = "%m/%d/%y",
                             tz = "GMT")

hyd_dep <- hyd_dep %>%
  mutate(month = month(DateTime)) %>%
  mutate(year = year(DateTime)) %>%
  mutate(day = day(DateTime)) %>%
  mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1))


# ## melt channel position data
hyd_vel <- hyd_dep %>%
  select(DateTime, Q, date_num, month, day, water_year, contains("vel") )

hyd_dep <- hyd_dep %>%
  select(DateTime, Q, date_num, month, day, water_year, contains("AV_depth") )


hyd_dep<-reshape2::melt(hyd_dep, id=c("DateTime","Q",  "date_num", "month", "day", "water_year"))
head(hyd_dep)
##  node rating curve
labels <- c(AV_depth_cm_Slice1 = "Slice 1", AV_depth_cm_Slice2 = "Slice 2",
            AV_depth_cm_Slice3 = "Slice 3", AV_depth_cm_Slice4 = "Slice 4")

png("figures/04_node_Depth_Q.png", width = 500, height = 600)

ggplot(hyd_dep, aes(x = Q, y=value)) +
  geom_line(aes( group = variable, lty = variable)) +
  # scale_linetype_manual(values= c("dotted", "solid", "dashed"),
  #                       breaks=c("MAX_depth_cm_Slice1", "MAX_depth_cm_Slice2", "MAX_depth_cm_Slice3", "MAX_depth_cm_Slice4"))+
  facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  labs(title = "Example Node: Depth ~ Q",
       y = "Depth (cm)",
       x = "Q (cms)") #+ theme_bw(base_size = 15)

dev.off()
# hyd_dep <- hyd_dep %>% rename(depth_cm = value)

## use smooth spline to predict on new data set
new_values <-smooth.spline(fitdata$depth_cm, fitdata$prob_fit)

## filter data by cross section position

hyd_dep1 <- filter(hyd_dep, variable == "AV_depth_cm_Slice1")
hyd_dep2 <- filter(hyd_dep, variable == "AV_depth_cm_Slice2")
hyd_dep3 <- filter(hyd_dep, variable == "AV_depth_cm_Slice3")
hyd_dep4 <- filter(hyd_dep, variable == "AV_depth_cm_Slice4")

## predict values

new_data1 <- hyd_dep1 %>%
  mutate(prob_fit = predict(new_values, value)$y) %>%
  rename(depth_cm = value) %>%
  mutate(prob_fit = ifelse(prob_fit<=0, 0, prob_fit)) ## predicts negative percentages - cut off at 0 for quick fix

new_data2 <- hyd_dep2 %>%
  mutate(prob_fit = predict(new_values, value)$y) %>%
  rename(depth_cm = value) %>%
  mutate(prob_fit = ifelse(prob_fit<=0, 0, prob_fit)) ## predicts negative percentages - cut off at 0 for quick fix

new_data3 <- hyd_dep3 %>%
  mutate(prob_fit = predict(new_values, value)$y) %>%
  rename(depth_cm = value) %>%
  mutate(prob_fit = ifelse(prob_fit<=0, 0, prob_fit)) ## predicts negative percentages - cut off at 0 for quick fix

new_data4 <- hyd_dep4 %>%
  mutate(prob_fit = predict(new_values, value)$y) %>%
  rename(depth_cm = value) %>%
  mutate(prob_fit = ifelse(prob_fit<=0, 0, prob_fit)) ## predicts negative percentages - cut off at 0 for quick fix

## bind back together
new_data <- rbind(new_data1, new_data2, new_data3, new_data4)
range(new_data$prob_fit)
head(new_data)

# probability as a function of discharge -----------------------------------

## plot
range(new_data$Q) ##0.01980962 73.14141301
range(new_data$prob_fit) ## 10.47175 2239.70470

peak <- new_data %>%
  group_by(variable) %>%
  filter(prob_fit == max(prob_fit)) #%>%


peakQ1 <- filter(peak, variable=="AV_depth_cm_Slice1")
peakQ1  <- max(peakQ1$Q)

peakQ2 <- filter(peak, variable=="AV_depth_cm_Slice2")
peakQ2  <- max(peakQ2$Q) ## 

peakQ3 <- filter(peak, variable=="AV_depth_cm_Slice3")
peakQ3  <- max(peakQ3$Q) ## 

peakQ4 <- filter(peak, variable=="AV_depth_cm_Slice4")
peakQ4  <- max(peakQ4$Q) ## 

## filter data by cross section position

new_data1 <- filter(new_data, variable == "AV_depth_cm_Slice1")
new_data2 <- filter(new_data, variable == "AV_depth_cm_Slice2")
new_data3 <- filter(new_data, variable == "AV_depth_cm_Slice3")
new_data4 <- filter(new_data, variable == "AV_depth_cm_Slice4")

## Slice 1

load(file="models_functions/root_interpolation_function.Rdata")

newx1  <- RootLinearInterpolant(new_data1$Q, new_data1$prob_fit, 0.2)
newx1

### if only an upper limit, we need a lower limit to consider when the channel is dry



## Slice 2
newx2  <- RootLinearInterpolant(new_data2$Q, new_data2$prob_fit, 0.2)
newx2

## Slice 3
newx3  <- RootLinearInterpolant(new_data3$Q, new_data3$prob_fit, 0.2)
newx3

## Slice 4
newx4  <- RootLinearInterpolant(new_data4$Q, new_data4$prob_fit, 0.2)
newx4

### df for limits
limits <- as.data.frame(matrix(ncol=4, nrow=2)) %>%
  rename(slice1 = V1, slice2 = V2, slice3 = V3, slice4 = V4) 


limits$slice1 <-c(newx1[1], newx1[2])
limits$slice2 <-c(newx2[1], newx2[2])
limits$slice3 <-c(newx3[1], newx3[2])
limits$slice4 <-c(newx4[1], newx4[2])
limits

write.csv(limits, "output_data/04_chub_adult_depth_Q_limits.csv")

## plot with thresholds
labels <- c(AV_depth_cm_Slice1 = "Slice 1", AV_depth_cm_Slice2 = "Slice 2",
            AV_depth_cm_Slice3 = "Slice 3", AV_depth_cm_Slice4 = "Slice 4")

png("figures/04_chub_adult_Depth_Q.png", width = 500, height = 600)

ggplot(new_data, aes(x = Q, y=prob_fit)) +
  geom_line(aes( group = variable, lty = variable)) +
  # scale_linetype_manual(values= c("dotted", "solid", "dashed"),
  #                       breaks=c("MAX_depth_cm_Slice1", "MAX_depth_cm_Slice2", "MAX_depth_cm_Slice3", "MAX_depth_cm_Slice4"))+
  facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
  
  geom_point(data = subset(new_data, variable =="AV_depth_cm_Slice1"), aes(y=0.2, x=newx1[1]), color="green") +
  geom_point(data = subset(new_data, variable =="AV_depth_cm_Slice1"), aes(y=0.2, x=newx1[2]), color="green") +
  
  geom_point(data = subset(new_data, variable =="AV_depth_cm_Slice2"), aes(y=0.2, x=newx2[1]), color="green") +
  geom_point(data = subset(new_data, variable =="AV_depth_cm_Slice2"), aes(y=0.2, x=newx2[2]), color="green") +
  
  geom_point(data = subset(new_data, variable =="AV_depth_cm_Slice3"), aes(y=0.2, x=newx3[1]), color="green") +
  geom_point(data = subset(new_data, variable =="AV_depth_cm_Slice3"), aes(y=0.2, x=newx3[2]), color="green") +
  
  geom_point(data = subset(new_data, variable =="AV_depth_cm_Slice4"), aes(y=0.2, x=newx4[1]), color="green") +
  geom_point(data = subset(new_data, variable =="AV_depth_cm_Slice4"), aes(y=0.2, x=newx4[2]), color="green") +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  labs(title = "Chub Adult: Depth ~ Q",
       y = "Probability",
       x = "Q (cms)") #+ theme_bw(base_size = 15)

dev.off()
### plot discharge over time
## bind dfs back together

new_data <- rbind(new_data1, new_data2, new_data3, new_data4)

# create year_month column       
new_datax <- new_data %>% unite(month_year, water_year:month, sep="-", remove=F) 
head(new_datax)

non_critical <- c(1,2,8:12) 
critical <- c(3:7) 

new_datax <- new_datax %>%
  mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") )


## find equation

load(file="models_functions/expression_Q_limit_function.RData")

thresh1 <- expression_Q(newx1, peakQ1) 
thresh1 <-as.expression(do.call("substitute", list(thresh1[[1]], list(limit = as.name("newx1")))))
thresh1

thresh2 <- expression_Q(newx2, peakQ2) 
thresh2 <-as.expression(do.call("substitute", list(thresh2[[1]], list(limit = as.name("newx2")))))
thresh2

thresh3 <- expression_Q(newx3, peakQ3) 
thresh3 <-as.expression(do.call("substitute", list(thresh3[[1]], list(limit = as.name("newx3")))))
thresh3

thresh4 <- expression_Q(newx4, peakQ4) 
thresh4 <-as.expression(do.call("substitute", list(thresh4[[1]], list(limit = as.name("newx4")))))
thresh4

###### calculate amount of time

time_stats1 <- new_datax %>%
  filter(variable == "AV_depth_cm_Slice1") %>%
  dplyr::group_by(water_year) %>%
  dplyr::mutate(Mid = sum(eval(thresh1))/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(water_year, season) %>%
  dplyr::mutate(Mid.Seasonal = sum(eval(thresh1))/length(DateTime)*100) %>%
  distinct(water_year, Mid, Mid.Seasonal) %>%
  mutate(position="Slice 1")

time_stats1


time_stats2 <- new_datax %>%
  filter(variable == "AV_depth_cm_Slice2") %>%
  dplyr::group_by(water_year) %>%
  dplyr::mutate(Mid = sum(eval(thresh2))/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(water_year, season) %>%
  dplyr::mutate(Mid.Seasonal = sum(eval(thresh2))/length(DateTime)*100) %>%
  distinct(water_year, Mid, Mid.Seasonal) %>%
  mutate(position="Slice 2")

time_stats2


time_stats3 <- new_datax %>%
  filter(variable == "AV_depth_cm_Slice3") %>%
  dplyr::group_by(water_year) %>%
  dplyr::mutate(Mid = sum(eval(thresh3))/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(water_year, season) %>%
  dplyr::mutate(Mid.Seasonal = sum(eval(thresh3))/length(DateTime)*100) %>%
  distinct(water_year, Mid, Mid.Seasonal) %>%
  mutate(position="Slice 3")

time_stats3
## how much time below limit?
time_stats3_under <- new_datax %>%
  filter(variable == "AV_depth_cm_Slice3") %>%
  dplyr::group_by(water_year) %>%
  dplyr::mutate(Mid = sum(Q<newx3[1])/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(water_year, season) %>%
  dplyr::mutate(Mid.Seasonal = sum(Q<newx3[1])/length(DateTime)*100) %>%
  distinct(water_year, Mid, Mid.Seasonal) %>%
  mutate(position="Slice 3")

time_stats3_under
mean(time_stats3_under$Mid) ## 96.07545
sum_stats <- hyd_dep %>%
  filter(variable == "AV_depth_cm_Slice3") %>%
  group_by(water_year) %>%
  summarise(meanQ = mean(Q), medianQ = median(Q), minQ = min(Q), maxQ = max(Q))
sum_stats
sum_stats_all

sum_stats_all <- hyd_dep %>%
  filter(variable == "AV_depth_cm_Slice3") %>%
  summarise(meanQ = mean(Q), medianQ = median(Q), minQ = min(Q), maxQ = max(Q))

sum_stats_dep <- hyd_dep %>%
  filter(variable == "AV_depth_cm_Slice3") %>%
  summarise(meandepth = mean(value), mediandepth = median(value), mindepth = min(value), maxdepth = max(value))
sum_stats_dep

time_stats3_over <- new_datax %>%
  filter(variable == "AV_depth_cm_Slice3") %>%
  dplyr::group_by(water_year) %>%
  dplyr::mutate(Mid = sum(Q>newx3[2])/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(water_year, season) %>%
  dplyr::mutate(Mid.Seasonal = sum(Q>newx3[2])/length(DateTime)*100) %>%
  distinct(water_year, Mid, Mid.Seasonal) %>%
  mutate(position="Slice 3")

time_stats3_over


time_stats4 <- new_datax %>%
  filter(variable == "AV_depth_cm_Slice4") %>%
  dplyr::group_by(water_year) %>%
  dplyr::mutate(Mid = sum(eval(thresh4))/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(water_year, season) %>%
  dplyr::mutate(Mid.Seasonal = sum(eval(thresh4))/length(DateTime)*100) %>%
  distinct(water_year, Mid, Mid.Seasonal) %>%
  mutate(position="Slice 4")

time_stats4


time_stats <- rbind(time_stats1, time_stats2, time_stats3, time_stats4)

## melt
head(time_stats)

# time_stats <- time_stats %>%
#   ungroup() %>%
#   select(-season)

melt_time<-reshape2::melt(time_stats, id=c("water_year", "position", "season"))
melt_time <- rename(melt_time, Time_Period = variable) %>%
  distinct()
head(melt_time)
length(unique(melt_time$Time_Period))

write.csv(melt_time, "output_data/04_chub_adult_depth_time_stats.csv")

## plot for annual stats - need probs in order

png("figures/04_chub_adult_depth_perc_time_above_threshold.png", width = 500, height = 600)

ggplot(melt_time, aes(x = water_year, y=value)) +
  geom_line(aes( group =c(), color = Time_Period)) +
  scale_color_manual(name = "Time_Period", breaks = c("Mid", "Mid.Seasonal"),
                     values=c( "green", "red"),
                     labels = c("Annual", "Critical Period")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(total_days$month_year), labels=format(total_days$month_year,"%b %Y")) +
  facet_wrap(~position, scales="free_x", nrow=3) +
  scale_y_continuous(limits=c(0,100)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Chub Adult: Time within discharge limit in relation to Depth",
       y = "Time (%)",
       x = "Water Year") #+ theme_bw(base_size = 15)

dev.off()

# Number of days above discharge ------------------------------------------
# need number of days discharge is above the limits outlined above - counted per month

## count number events within each threshold with a running total - max total is the number of consequative 
## order by datetime

## change year to water year and count hours within Q range
new_data1  <- new_datax %>% 
  dplyr::filter(variable == "AV_depth_cm_Slice1") %>%
  group_by(month, day, water_year, ID = data.table::rleid(eval(thresh1))) %>%
  mutate(threshold = if_else(eval(thresh1),  row_number(), 0L))%>%
  mutate(position="Slice1")

new_data2  <- new_datax %>% 
  dplyr::filter(variable == "AV_depth_cm_Slice2") %>%
  group_by(month, day, water_year, ID = data.table::rleid(eval(thresh2))) %>%
  mutate(threshold = if_else(eval(thresh2),  row_number(), 0L))%>%
  mutate(position="Slice2")

new_data3  <- new_datax %>% 
  dplyr::filter(variable == "AV_depth_cm_Slice1") %>%
  group_by(month, day, water_year, ID = data.table::rleid(eval(thresh3))) %>%
  mutate(threshold = if_else(eval(thresh3),  row_number(), 0L))%>%
  mutate(position="Slice3")

new_data4  <- new_datax %>% 
  dplyr::filter(variable == "AV_depth_cm_Slice4") %>%
  group_by(month, day, water_year, ID = data.table::rleid(eval(thresh4))) %>%
  mutate(threshold = if_else(eval(thresh4),  row_number(), 0L))%>%
  mutate(position="Slice4")

## melt data frame so that each probability column are all in one row 
## select only columns needed - Q, month, year, day all IDs and probs
# names(new_data)

new_data1x <- select(new_data1, c(Q, month, water_year,  day, ID, threshold, position, season)) # all probs
new_data2x <- select(new_data2, c(Q, month, water_year,  day, ID, threshold, position, season))
new_data3x <- select(new_data3, c(Q, month, water_year,  day, ID, threshold, position, season))
new_data4x <- select(new_data4, c(Q, month, water_year,  day, ID, threshold, position, season))

new_datax <- rbind(new_data1x, new_data2x, new_data3x, new_data4x)
new_datax
## melt
melt_data<-reshape2::melt(new_datax, id=c("ID", "day", "month", "Q", "water_year", "position", "season"))
melt_data <- melt_data %>% rename(n_days = value) %>%
  select(-variable)
head(melt_data)

## count the number of days in each month
total_days_per_month01 <- melt_data %>%
  group_by(month, water_year, position) %>%
  summarise(days_per_month = sum(n_days))

total_days <- total_days_per_month01

write.csv(total_days, "output_data/04_chub_adult_depth_total_days.csv")

# # create year_month column       
total_days <- ungroup(total_days) %>%
  unite(month_year, water_year:month, sep="-", remove=F)


## convert month year to date format
library(zoo)
total_days$month_year <-  zoo::as.yearmon(total_days$month_year)
total_days$month_year <- as.Date(total_days$month_year)

# total_hours <- rename(total_hours, Low = n_days_low, Medium = n_days_medium, High = n_days_high)

## define seasons/critical period

total_days <- total_days %>%
  mutate(season = ifelse(month %in% non_critical, "Annual", "Seasonal") )

# total_hours <- total_hours %>%
#   mutate(season = ifelse(month %in% winter, "winter", "summer") ) %>%
#   select(-day)

# ## melt data

head(total_days)

melt_days<-reshape2::melt(total_days, id=c("month_year", "water_year", "month", "season", "position"))
melt_days <- rename(melt_days, Probability_Threshold = variable,
                    n_days = value)

head(melt_days)

## save df
write.csv(melt_days, "output_data/04_chub_adult_depth_total_days_long.csv")

library(scales)

## plot all ts

png("figures/04_chub_adult_depth_no_days_within_Q.png", width = 500, height = 600)

ggplot(melt_days, aes(x =month_year, y=n_days)) +
  geom_line(aes( group = season, color = season)) +
  scale_color_manual(name = "season", breaks = c("Annual", "Seasonal"),
                     values=c( "green", "red"),
                     labels = c("Annual", "Critical Period")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  scale_x_date(breaks=pretty_breaks(), labels = date_format("%b %Y")) +
  scale_y_continuous(limits= c(0, 31)) +
  # scale_x_continuous(breaks=as.numeric(melt_days$month_year), labels=format(melt_days$month_year,"%b %Y")) +
  facet_wrap(~position, nrow=3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Chub Adult: Number of days within discharge limit in relation to Depth",
       y = "Number of days per Month",
       x = "Year") #+ theme_bw(base_size = 15)

dev.off()


# Velocity ----------------------------------------------------------------

fitdata <- read.csv("output_data/03_chub_adult_velocity_prob_curve_data.csv")
head(hyd_vel)
head(fitdata)


hyd_vel<-reshape2::melt(hyd_vel, id=c("DateTime","Q",  "date_num", "month", "day", "water_year"))
# hyd_vel <- hyd_vel %>% rename(velth_cm = value)


## use smooth spline to predict on new data set
new_values <-smooth.spline(fitdata$velocity_fit, fitdata$prob_fit)

## filter data by cross section position

hyd_vel1 <- filter(hyd_vel, variable == "av.vel.ms_slice1")
hyd_vel2 <- filter(hyd_vel, variable == "av.vel.ms_slice2")
hyd_vel3 <- filter(hyd_vel, variable == "av.vel.ms_slice3")
hyd_vel4 <- filter(hyd_vel, variable == "av.vel.ms_slice4")

## predict values

new_data1 <- hyd_vel1 %>%
  mutate(prob_fit = predict(new_values, value)$y) %>%
  rename(velth_cm = value) %>%
  mutate(prob_fit = ifelse(prob_fit<=0, 0, prob_fit)) ## predicts negative percentages - cut off at 0 for quick fix

new_data2 <- hyd_vel2 %>%
  mutate(prob_fit = predict(new_values, value)$y) %>%
  rename(velth_cm = value) %>%
  mutate(prob_fit = ifelse(prob_fit<=0, 0, prob_fit)) ## predicts negative percentages - cut off at 0 for quick fix

new_data3 <- hyd_vel3 %>%
  mutate(prob_fit = predict(new_values, value)$y) %>%
  rename(velth_cm = value) %>%
  mutate(prob_fit = ifelse(prob_fit<=0, 0, prob_fit)) ## predicts negative percentages - cut off at 0 for quick fix

new_data4 <- hyd_vel4 %>%
  mutate(prob_fit = predict(new_values, value)$y) %>%
  rename(velth_cm = value) %>%
  mutate(prob_fit = ifelse(prob_fit<=0, 0, prob_fit)) ## predicts negative percentages - cut off at 0 for quick fix

## bind back together
new_data <- rbind(new_data1, new_data2, new_data3, new_data4)
range(new_data$prob_fit)
head(new_data)

# probability as a function of discharge -----------------------------------

## plot
range(new_data$Q) ##0.01980962 73.14141301
range(new_data$prob_fit) ##0.0000000 0.3989423

peak <- new_data %>%
  group_by(variable) %>%
  filter(prob_fit == max(prob_fit)) #%>%


peakQ1 <- filter(peak, variable=="av.vel.ms_slice1")
peakQ1  <- max(peakQ1$Q)

peakQ2 <- filter(peak, variable=="av.vel.ms_slice2")
peakQ2  <- max(peakQ2$Q) ## 

peakQ3 <- filter(peak, variable=="av.vel.ms_slice3")
peakQ3  <- max(peakQ3$Q) ## 

peakQ4 <- filter(peak, variable=="av.vel.ms_slice4")
peakQ4  <- max(peakQ4$Q) ## 

## filter data by cross section position

new_data1 <- filter(new_data, variable == "av.vel.ms_slice1")
new_data2 <- filter(new_data, variable == "av.vel.ms_slice2")
new_data3 <- filter(new_data, variable == "av.vel.ms_slice3")
new_data4 <- filter(new_data, variable == "av.vel.ms_slice4")

## Slice 1

load(file="models_functions/root_interpolation_function.Rdata")


newx1 <- RootLinearInterpolant(new_data1$Q, new_data1$prob_fit, 0.2)
newx1

if(length(newx1) > 2) {
  newx1 <- c(newx1[1], newx1[length(newx1)])
} else {
  newx1 <- newx1
}
newx1

## Slice 2
newx2  <- RootLinearInterpolant(new_data2$Q, new_data2$prob_fit, 0.2)
newx2

if(length(newx2) > 2) {
  newx2 <- c(newx2[1], newx2[length(newx2)])
} else {
  newx2 <- newx2
}
new_data3
## Slice 3
newx3  <- RootLinearInterpolant(new_data3$Q, new_data3$prob_fit, 0.2)
newx3

newx3_Lower <- RootLinearInterpolant(new_data3$Q, new_data3$velth_cm, 0.33)
newx3_Lower
min(new_data3$velth_cm)
min(new_data3$Q)

if(length(newx3) > 2) {
  newx3 <- c(newx3[1], newx3[length(newx3)])
} else {
  newx3 <- newx3
}

## Slice 4
newx4  <- RootLinearInterpolant(new_data4$Q, new_data4$prob_fit, 0.2)
newx4

if(length(newx4) > 2) {
  newx4 <- c(newx4[1], newx4[length(newx4)])
} else {
  newx4 <- newx4
}

### df for limits
limits <- as.data.frame(matrix(ncol=4, nrow=2)) %>%
  rename(slice1 = V1, slice2 = V2, slice3 = V3, slice4 = V4) 


limits$slice1 <-c(newx1[1], newx1[2])
limits$slice2 <-c(newx2[1], newx2[2])
limits$slice3 <-c(newx3[1], newx3[2])
limits$slice4 <-c(newx4[1], newx4[2])
limits

write.csv(limits, "output_data/04_chub_adult_velocity_Q_limits.csv")

## plot with thresholds
labels <- c(av.vel.ms_slice1 = "Slice 1", av.vel.ms_slice2 = "Slice 2",
            av.vel.ms_slice3 = "Slice 3", av.vel.ms_slice4 = "Slice 4")

png("figures/04_chub_adult_Velocity_Q.png", width = 500, height = 600)

ggplot(new_data, aes(x = Q, y=prob_fit)) +
  geom_line(aes( group = variable, lty = variable)) +
  # scale_linetype_manual(values= c("dotted", "solid", "dashed"),
  #                       breaks=c("MAX_velth_cm_Slice1", "MAX_velth_cm_Slice2", "MAX_velth_cm_Slice3", "MAX_velth_cm_Slice4"))+
  facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
  
  geom_point(data = subset(new_data, variable =="av.vel.ms_slice1"), aes(y=0.2, x=newx1[1]), color="green") +
  geom_point(data = subset(new_data, variable =="av.vel.ms_slice1"), aes(y=0.2, x=newx1[2]), color="green") +

  geom_point(data = subset(new_data, variable =="av.vel.ms_slice2"), aes(y=0.2, x=newx2[1]), color="green") +
  geom_point(data = subset(new_data, variable =="av.vel.ms_slice2"), aes(y=0.2, x=newx2[2]), color="green") +

  geom_point(data = subset(new_data, variable =="av.vel.ms_slice3"), aes(y=0.2, x=newx3[1]), color="green") +
  geom_point(data = subset(new_data, variable =="av.vel.ms_slice3"), aes(y=0.2, x=newx3[2]), color="green") +

  geom_point(data = subset(new_data, variable =="av.vel.ms_slice4"), aes(y=0.2, x=newx4[1]), color="green") +
  geom_point(data = subset(new_data, variable =="av.vel.ms_slice4"), aes(y=0.2, x=newx4[2]), color="green") +

  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  labs(title = "Chub Adult: Velocity ~ Q",
       y = "Probability",
       x = "Q (cfs)") #+ theme_bw(base_size = 15)

dev.off()
### plot discharge over time
## bind dfs back together

new_data <- rbind(new_data1, new_data2, new_data3, new_data4)

# create year_month column       
new_datax <- new_data %>% unite(month_year, water_year:month, sep="-", remove=F) 
head(new_datax)

non_critical <- c(1,2,8:12) 
critical <- c(3:7) 

new_datax <- new_datax %>%
  mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") )


## find equation

load(file="models_functions/expression_Q_limit_function.RData")

thresh1 <- expression_Q(newx1, peakQ1) 
thresh1 <-as.expression(do.call("substitute", list(thresh1[[1]], list(limit = as.name("newx1")))))
thresh1

thresh2 <- expression_Q(newx2, peakQ2) 
thresh2 <-as.expression(do.call("substitute", list(thresh2[[1]], list(limit = as.name("newx2")))))
thresh2

thresh3 <- expression_Q(newx3, peakQ3) 
thresh3 <-as.expression(do.call("substitute", list(thresh3[[1]], list(limit = as.name("newx3")))))
thresh3

thresh4 <- expression_Q(newx4, peakQ4) 
thresh4 <-as.expression(do.call("substitute", list(thresh4[[1]], list(limit = as.name("newx4")))))
thresh4

###### calculate amount of time

time_stats1 <- new_datax %>%
  filter(variable == "av.vel.ms_slice1") %>%
  dplyr::group_by(water_year) %>%
  dplyr::mutate(Mid = sum(eval(thresh1))/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(water_year, season) %>%
  dplyr::mutate(Mid.Seasonal = sum(eval(thresh1))/length(DateTime)*100) %>%
  distinct(water_year, Mid, Mid.Seasonal) %>%
  mutate(position="Slice 1")

time_stats1


time_stats2 <- new_datax %>%
  filter(variable == "av.vel.ms_slice2") %>%
  dplyr::group_by(water_year) %>%
  dplyr::mutate(Mid = sum(eval(thresh2))/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(water_year, season) %>%
  dplyr::mutate(Mid.Seasonal = sum(eval(thresh2))/length(DateTime)*100) %>%
  distinct(water_year, Mid, Mid.Seasonal) %>%
  mutate(position="Slice 2")

time_stats2


time_stats3 <- new_datax %>%
  filter(variable == "av.vel.ms_slice3") %>%
  dplyr::group_by(water_year) %>%
  dplyr::mutate(Mid = sum(Q > newx3_Lower & Q <= newx3)/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(water_year, season) %>%
  dplyr::mutate(Mid.Seasonal = sum(Q > newx3_Lower & Q <= newx3)/length(DateTime)*100) %>%
  distinct(water_year, Mid, Mid.Seasonal) %>%
  mutate(position="Slice 3")

time_stats3
thresh3

## how much time below limit?
time_stats3_under <- new_datax %>%
  filter(variable == "av.vel.ms_slice3") %>%
  dplyr::group_by(water_year) %>%
  dplyr::mutate(Mid = sum(Q<newx3[1])/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(water_year, season) %>%
  dplyr::mutate(Mid.Seasonal = sum(Q<newx3[1])/length(DateTime)*100) %>%
  distinct(water_year, Mid, Mid.Seasonal) %>%
  mutate(position="Slice 3")

time_stats3_under
mean(time_stats3_under$Mid) ## 93.56238

time_stats3_under01 <- new_datax %>%
  filter(variable == "av.vel.ms_slice3") %>%
  dplyr::group_by(water_year) %>%
  dplyr::mutate(Mid = sum(Q<0.1)/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(water_year, season) %>%
  dplyr::mutate(Mid.Seasonal = sum(Q<0.1)/length(DateTime)*100) %>%
  distinct(water_year, Mid, Mid.Seasonal) %>%
  mutate(position="Slice 3")

time_stats3_under01
mean(time_stats3_under01$Mid) ##63.77912

time_stats3_over <- new_datax %>%
  filter(variable == "av.vel.ms_slice3") %>%
  dplyr::group_by(water_year) %>%
  dplyr::mutate(Mid = sum(Q>newx3[1])/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(water_year, season) %>%
  dplyr::mutate(Mid.Seasonal = sum(Q>newx3[1])/length(DateTime)*100) %>%
  distinct(water_year, Mid, Mid.Seasonal) %>%
  mutate(position="Slice 3")

time_stats3_over
mean(time_stats3_over$Mid) ## 6.437616


time_stats4 <- new_datax %>%
  filter(variable == "av.vel.ms_slice4") %>%
  dplyr::group_by(water_year) %>%
  dplyr::mutate(Mid = sum(eval(thresh4))/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(water_year, season) %>%
  dplyr::mutate(Mid.Seasonal = sum(eval(thresh4))/length(DateTime)*100) %>%
  distinct(water_year, Mid, Mid.Seasonal) %>%
  mutate(position="Slice 4")

time_stats4


time_stats <- rbind(time_stats1, time_stats2, time_stats3, time_stats4)

## melt
head(time_stats)
time_stats
# time_stats <- time_stats %>%
#   ungroup() %>%
#   select(-season)

melt_time<-reshape2::melt(time_stats, id=c("water_year", "position", "season"))
melt_time <- rename(melt_time, Time_Period = variable) %>%
  distinct()
head(melt_time)
length(unique(melt_time$Time_Period))

write.csv(melt_time, "output_data/04_chub_adult_velocity_time_stats.csv")

## plot for annual stats - need probs in order

png("figures/04_chub_adult_velocity_perc_time_above_threshold.png", width = 500, height = 600)

ggplot(melt_time, aes(x = water_year, y=value)) +
  geom_line(aes( group =c(), color = Time_Period)) +
  scale_color_manual(name = "Time_Period", breaks = c("Mid", "Mid.Seasonal"),
                     values=c( "green", "red"),
                     labels = c("Annual", "Critical Period")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(total_days$month_year), labels=format(total_days$month_year,"%b %Y")) +
  facet_wrap(~position, scales="free_x", nrow=3) +
  scale_y_continuous(limits=c(0,100)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Chub Adult: Time within discharge limit in relation to Velocity",
       y = "Time (%)",
       x = "Water Year") #+ theme_bw(base_size = 15)

dev.off()

# Number of days above discharge ------------------------------------------
# need number of days discharge is above the limits outlined above - counted per month

## count number events within each threshold with a running total - max total is the number of consequative 
## order by datetime

## change year to water year and count hours within Q range
new_data1  <- new_datax %>% 
  dplyr::filter(variable == "av.vel.ms_slice1") %>%
  group_by(month, day, water_year, ID = data.table::rleid(eval(thresh1))) %>%
  mutate(threshold = if_else(eval(thresh1),  row_number(), 0L))%>%
  mutate(position="Slice1")


new_data2  <- new_datax %>% 
  dplyr::filter(variable == "av.vel.ms_slice2") %>%
  group_by(month, day, water_year, ID = data.table::rleid(eval(thresh2))) %>%
  mutate(threshold = if_else(eval(thresh2),  row_number(), 0L))%>%
  mutate(position="Slice2")

new_data3  <- new_datax %>% 
  dplyr::filter(variable == "av.vel.ms_slice1") %>%
  group_by(month, day, water_year, ID = data.table::rleid(Q > newx3_Lower & Q <= newx3)) %>%
  mutate(threshold = if_else(Q > newx3_Lower & Q <= newx3,  row_number(), 0L))%>%
  mutate(position="Slice3")
new_data3

new_data4  <- new_datax %>% 
  dplyr::filter(variable == "av.vel.ms_slice4") %>%
  group_by(month, day, water_year, ID = data.table::rleid(eval(thresh4))) %>%
  mutate(threshold = if_else(eval(thresh4),  row_number(), 0L))%>%
  mutate(position="Slice4")

## melt data frame so that each probability column are all in one row 
## select only columns needed - Q, month, year, day all IDs and probs
# names(new_data)

new_data1x <- select(new_data1, c(Q, month, water_year,  day, ID, threshold, position, season)) # all probs
new_data2x <- select(new_data2, c(Q, month, water_year,  day, ID, threshold, position, season))
new_data3x <- select(new_data3, c(Q, month, water_year,  day, ID, threshold, position, season))
new_data4x <- select(new_data4, c(Q, month, water_year,  day, ID, threshold, position, season))

new_datax <- rbind(new_data1x, new_data2x, new_data3x, new_data4x)
new_datax
## melt
melt_data<-reshape2::melt(new_datax, id=c("ID", "day", "month", "Q", "water_year", "position", "season"))
melt_data <- melt_data %>% rename(n_days = value) %>%
  select(-variable)
head(melt_data)

## count the number of days in each month
total_days_per_month01 <- melt_data %>%
  group_by(month, water_year, position) %>%
  summarise(days_per_month = sum(n_days))

total_days <- total_days_per_month01
total_days
write.csv(total_days, "output_data/04_chub_adult_velocity_total_days.csv")

# # create year_month column       
total_days <- ungroup(total_days) %>%
  unite(month_year, water_year:month, sep="-", remove=F)


## convert month year to date format
library(zoo)
total_days$month_year <-  zoo::as.yearmon(total_days$month_year)
total_days$month_year <- as.Date(total_days$month_year)

# total_hours <- rename(total_hours, Low = n_days_low, Medium = n_days_medium, High = n_days_high)

## define seasons/critical period

total_days <- total_days %>%
  mutate(season = ifelse(month %in% non_critical, "Annual", "Seasonal") )

# total_hours <- total_hours %>%
#   mutate(season = ifelse(month %in% winter, "winter", "summer") ) %>%
#   select(-day)

# ## melt data

head(total_days)

melt_days<-reshape2::melt(total_days, id=c("month_year", "water_year", "month", "season", "position"))
melt_days <- rename(melt_days, Probability_Threshold = variable,
                    n_days = value)

head(melt_days)

## save df
write.csv(melt_days, "output_data/04_chub_adult_velocity_total_days_long.csv")

library(scales)

## plot all ts

png("figures/04_chub_adult_velocity_no_days_within_Q.png", width = 500, height = 600)

ggplot(melt_days, aes(x =month_year, y=n_days)) +
  geom_line(aes( group = season, color = season)) +
  scale_color_manual(name = "season", breaks = c("Annual", "Seasonal"),
                     values=c( "green", "red"),
                     labels = c("Annual", "Critical Period")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  scale_x_date(breaks=pretty_breaks(), labels = date_format("%b %Y")) +
  scale_y_continuous(limits= c(0, 31)) +
  # scale_x_continuous(breaks=as.numeric(melt_days$month_year), labels=format(melt_days$month_year,"%b %Y")) +
  facet_wrap(~position, nrow=3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Chub Adult: Number of days within discharge limit in relation to Velocity",
       y = "Number of days per Month",
       x = "Year") #+ theme_bw(base_size = 15)

dev.off()

