## willow adult stream power

library(sf)
library(tidyverse)
library(tidyr)
library(sm)
library(lubridate) # work with dates
library(dplyr)     # data manipulation (filter, summarize, mutate)
library(ggplot2)   # graphics
library(gridExtra) # tile several plots next to each other
library(scales)
library(data.table)


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

str(hyd_dep)
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

hyd_dep <- hyd_dep %>%
  select(DateTime, Q, date_num, month, day, water_year, contains("depth"), contains("power") )

hyd_dep<-reshape2::melt(hyd_dep, id=c("DateTime","Q", "date_num", "month", "day", "water_year"))
head(hyd_dep)


# stream power -------------------------------------------------------------------


## smooth spline the curve to get exact value of discharge at a given probability

## filter data by cross section position
new_data1 <- filter(hyd_dep, variable == "total.power.watt.ms_slice1")
new_data2 <- filter(hyd_dep, variable == "total.power.watt.ms_slice2")
new_data3 <- filter(hyd_dep, variable == "total.power.watt.ms_slice3")
new_data4 <- filter(hyd_dep, variable == "total.power.watt.ms_slice4")

range(new_data1$value)
## Slice 1
curve1 <- spline(new_data1$Q, new_data1$value,
                   xmin = min(new_data1$Q), xmax = max(new_data1$Q), ties = mean)
## main channel values
if(min(curve1$y)>20) {
  newx1a <- min(curve1$x)
} else {
  newx1a1 <- approx(x = curve1$y, y = curve1$x, xout = 20)$y
}
newx1a1

if(max(curve1$y)<4000) {
  newx2a1<- max(curve1$x)
} else {
  newx2a1 <- approx(x = curve1e$y, y = curve1$x, xout = 4000)$y
}
newx2a1

## Slice 2
curve2 <- spline(new_data2$Q, new_data2$value,
                 xmin = min(new_data2$Q), xmax = max(new_data2$Q), ties = mean)
## main channel values
if(min(curve2$y)>20) {
  newx1a2 <- min(curve2$x)
} else {
  newx1a2 <- approx(x = curve2$y, y = curve2$x, xout = 20)$y
}
newx1a2

if(max(curve2$y)<4000) {
  newx2a2 <- max(curve2$x)
} else {
  newx2a2 <- approx(x = curve2$y, y = curve2$x, xout = 4000)$y
}
newx2a2

## Curve 3
curve3 <- spline(new_data3$Q, new_data3$value,
                    xmin = min(new_data3$Q), xmax = max(new_data3$Q), ties = mean)
## curve 3
if(min(curve3$y)>20) {
  newx1a3 <- min(curve3$x)
} else {
  newx1a3 <- approx(x = curve3$y, y = curve3$x, xout = 20)$y
}


if(max(curve3$y)<4000) {
  newx2a3 <- max(curve3$x)
} else {
  newx2a3 <- approx(x = curve3$y, y =curve3$x, xout = 4000)$y
}

## Curve 4
curve4 <- spline(new_data4$Q, new_data4$value,
                 xmin = min(new_data4$Q), xmax = max(new_data4$Q), ties = mean)

## curve 4
if(min(curve4$y)>20) {
  newx1a4 <- min(curve4$x)
} else {
  newx1a4 <- approx(x = curve4$y, y = curve4$x, xout = 20)$y
}


if(max(curve3$y)<4000) {
  newx2a4 <- max(curve4$x)
} else {
  newx2a4 <- approx(x = curve4$y, y =curve4$x, xout = 4000)$y
}


### df for limits
limits <- as.data.frame(matrix(ncol=4, nrow=2)) %>%
  rename(slice1 = V1, slice2 = V2, slice3 = V3, slice4 = V4) 
rownames(limits)<-c("thresh_1", "thresh_2")

limits$slice1 <-c(newx1a1[1], 
               newx2a1[1])

limits$slice2 <-c(newx1a2[1], 
                  newx2a2[1])
limits$slice3 <-c(newx1a3[1], 
                  newx2a3[1])
limits$slice4 <-c(newx1a4[1], 
                  newx2a4[1])
limits

write.csv(limits, "output_data/01_willow_adult_stream_power_Q_limits.csv")
### percentage of time above threshold

## bind dfs back together

new_data <- rbind(new_data1, new_data2, new_data3, new_data4)

# create year_month column       
new_datax <- new_data %>% unite(month_year, water_year:month, sep="-", remove=F) 
head(new_datax)

# dataframe for stats -----------------------------------------------------

## make dataframe for all years 

## define critical period or season for adult as all year is critical

non_critical <- c(1:3,10:12) 
critical <- c(4:9) 

new_datax <- new_datax %>%
  mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") )
head(new_datax)
# time stats ------------------------------------------------
### time stats

time_stats1 <- new_datax %>%
  dplyr::filter(variable == "total.power.watt.ms_slice1") %>%
  dplyr::group_by(water_year) %>%
  dplyr::mutate(Annual = sum( Q <= newx2a1)/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(water_year, season) %>%
  dplyr::mutate(Seasonal = sum( Q <= newx2a1)/length(DateTime)*100) %>%
  distinct(water_year, Annual, Seasonal) %>%
  mutate(position="Slice1")

time_stats1

time_stats2 <- new_datax %>%
  dplyr::filter(variable == "total.power.watt.ms_slice2") %>%
  dplyr::group_by(water_year) %>%
  dplyr::mutate(Annual = sum(Q <= newx2a2)/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(water_year, season) %>%
  dplyr::mutate(Seasonal = sum(Q <= newx2a2)/length(DateTime)*100) %>%
  distinct(water_year, Annual, Seasonal) %>%
  mutate(position="Slice2")

time_stats3 <- new_datax %>%
  dplyr::filter(variable == "total.power.watt.ms_slice3") %>%
  dplyr::group_by(water_year) %>%
  dplyr::mutate(Annual = sum(Q <= newx2a3)/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(water_year, season) %>%
  dplyr::mutate(Seasonal = sum(Q <= newx2a3)/length(DateTime)*100) %>%
  distinct(water_year, Annual, Seasonal) %>%
  mutate(position="Slice3")

time_stats4 <- new_datax %>%
  dplyr::filter(variable == "total.power.watt.ms_slice4") %>%
  dplyr::group_by(water_year) %>%
  dplyr::mutate(Annual = sum(Q <= newx2a4)/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(water_year, season) %>%
  dplyr::mutate(Seasonal = sum(Q <= newx2a4)/length(DateTime)*100) %>%
  distinct(water_year, Annual, Seasonal) %>%
  mutate(position="Slice4")

time_stats <- rbind(time_stats1, time_stats2, time_stats3, time_stats4)

time_stats
write.csv(time_stats, "output_data/01_time_stats_willow_adult_stream_power.csv")


# Number of days above discharge ------------------------------------------
# need number of days discharge is above the limits outlined above - counted per month
## packages

## change year to water year and count hours within Q range
new_data1  <- new_datax %>% 
  dplyr::filter(variable == "total.power.watt.ms_slice1") %>%
  group_by(month, day, water_year, ID = data.table::rleid(Q <= newx2a1)) %>%
  mutate(threshold = if_else(Q <= newx2a1,  row_number(), 0L))%>%
  mutate(position="Slice1")

new_data2  <- new_datax %>% 
  dplyr::filter(variable == "total.power.watt.ms_slice2") %>%
  group_by(month, day, water_year, ID = data.table::rleid(Q <= newx2a2)) %>%
  mutate(threshold = if_else(Q <= newx2a2,  row_number(), 0L))%>%
  mutate(position="Slice2")

new_data3  <- new_datax %>% 
  dplyr::filter(variable == "total.power.watt.ms_slice3") %>%
  group_by(month, day, water_year, ID = data.table::rleid(Q <= newx2a3)) %>%
  mutate(threshold = if_else(Q <= newx2a3,  row_number(), 0L))%>%
  mutate(position="Slice3")

new_data4  <- new_datax %>% 
  dplyr::filter(variable == "total.power.watt.ms_slice4") %>%
  group_by(month, day, water_year, ID = data.table::rleid(Q <= newx2a4)) %>%
  mutate(threshold = if_else(Q <= newx2a4,  row_number(), 0L))%>%
  mutate(position="Slice4")


# select columns needed and combine dfs

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

total_days_per_year

write.csv(total_days_per_month01, "output_data/01_number_of_days_willow_adult_stream_power.csv")


# Depth -------------------------------------------------------------------

## plot
range(hyd_dep$Q) ## 26.22926 41750.16797
# nas <- which(complete.cases(hyd_dep) == FALSE)
# nas #0


## filter data by cross section position
new_data1 <- filter(hyd_dep, variable == "MAX_depth_cm_Slice1")
new_data2 <- filter(hyd_dep, variable == "MAX_depth_cm_Slice2")
new_data3 <- filter(hyd_dep, variable == "MAX_depth_cm_Slice3")
new_data4 <- filter(hyd_dep, variable == "MAX_depth_cm_Slice4")

## Slice 1
curve1 <- spline(new_data1$Q, new_data1$value,
                 xmin = min(new_data1$Q), xmax = max(new_data1$Q), ties = mean)
## main channel values
if(min(curve1$y)>5) {
  newx1a <- min(curve1$x)
} else {
  newx1a1 <- approx(x = curve1$y, y = curve1$x, xout = 5)$y
}
newx1a1


## Slice 2
curve2 <- spline(new_data2$Q, new_data2$value,
                 xmin = min(new_data2$Q), xmax = max(new_data2$Q), ties = mean)
## main channel values
if(min(curve2$y)>5) {
  newx1a2 <- min(curve2$x)
} else {
  newx1a2 <- approx(x = curve2$y, y = curve2$x, xout = 5)$y
}
newx1a2



## Curve 3
curve3 <- spline(new_data3$Q, new_data3$value,
                 xmin = min(new_data3$Q), xmax = max(new_data3$Q), ties = mean)
## curve 3
if(min(curve3$y)>5) {
  newx1a3 <- min(curve3$x)
} else {
  newx1a3 <- approx(x = curve3$y, y = curve3$x, xout = 5)$y
}


## Curve 4
curve4 <- spline(new_data4$Q, new_data4$value,
                 xmin = min(new_data4$Q), xmax = max(new_data4$Q), ties = mean)

## curve 4
if(min(curve4$y)>5) {
  newx1a4 <- min(curve4$x)
} else {
  newx1a4 <- approx(x = curve4$y, y = curve4$x, xout = 5)$y
}


### df for limits
limits <- as.data.frame(matrix(ncol=4, nrow=1)) %>%
  rename(slice1 = V1, slice2 = V2, slice3 = V3, slice4 = V4) 
rownames(limits)<-c("thresh_1", "thresh_2")

limits$slice1 <-c(newx1a1[1])
limits$slice2 <-c(newx1a2[1])
limits$slice3 <-c(newx1a3[1])
limits$slice4 <-c(newx1a4[1])

limits

write.csv(limits, "output_data/01_willow_germ_depth_Q_limits.csv")
### percentage of time above threshold

## bind dfs back together

new_data <- rbind(new_data1, new_data2, new_data3, new_data4)

# create year_month column       
new_datax <- new_data %>% unite(month_year, water_year:month, sep="-", remove=F) 
head(new_datax)

# dataframe for stats -----------------------------------------------------

## make dataframe for all years 

## define critical period or season for adult as all year is critical

non_critical <- c(1:3,10:12) 
critical <- c(4:9) 

new_datax <- new_datax %>%
  mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") )
head(new_datax)
# time stats ------------------------------------------------
### time stats

time_stats1 <- new_datax %>%
  dplyr::filter(variable == "MAX_depth_cm_Slice1") %>%
  dplyr::group_by(water_year) %>%
  dplyr::mutate(Annual = sum(Q >= newx1a1)/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(water_year, season) %>%
  dplyr::mutate(Seasonal = sum(Q >= newx1a1)/length(DateTime)*100) %>%
  distinct(water_year, Annual, Seasonal) %>%
  mutate(position="Slice1")

time_stats1

time_stats2 <- new_datax %>%
  dplyr::filter(variable == "MAX_depth_cm_Slice2") %>%
  dplyr::group_by(water_year) %>%
  dplyr::mutate(Annual = sum(Q >= newx1a2)/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(water_year, season) %>%
  dplyr::mutate(Seasonal = sum(Q >= newx1a2)/length(DateTime)*100) %>%
  distinct(water_year, Annual, Seasonal) %>%
  mutate(position="Slice2")

time_stats3 <- new_datax %>%
  dplyr::filter(variable == "MAX_depth_cm_Slice3") %>%
  dplyr::group_by(water_year) %>%
  dplyr::mutate(Annual = sum(Q >= newx1a3)/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(water_year, season) %>%
  dplyr::mutate(Seasonal = sum(Q >= newx1a3)/length(DateTime)*100) %>%
  distinct(water_year, Annual, Seasonal) %>%
  mutate(position="Slice3")

time_stats4 <- new_datax %>%
  dplyr::filter(variable == "MAX_depth_cm_Slice4") %>%
  dplyr::group_by(water_year) %>%
  dplyr::mutate(Annual = sum(Q >= newx1a4)/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(water_year, season) %>%
  dplyr::mutate(Seasonal = sum(Q >= newx1a4)/length(DateTime)*100) %>%
  distinct(water_year, Annual, Seasonal) %>%
  mutate(position="Slice4")

time_stats <- rbind(time_stats1, time_stats2, time_stats3, time_stats4)

time_stats
write.csv(time_stats, "output_data/01_time_stats_willow_germ_depth.csv")

### percent

## plot with thresholds
labels <- c(MAX_depth_cm_Slice1 = "Slice 1", MAX_depth_cm_Slice4 = "Slice 2",
            MAX_depth_cm_Slice3 = "Slice 3", MAX_depth_cm_Slice4 = "Slice 4")

png("figures/01_willow_germ_Depth_Q.png", width = 500, height = 600)

ggplot(new_datax, aes(x = Q, y=value)) +
  geom_line(aes( group = variable, lty = variable)) +
  # scale_linetype_manual(values= c("dotted", "solid", "dashed"),
  #                       breaks=c("MAX_depth_cm_Slice1", "MAX_depth_cm_Slice2", "MAX_depth_cm_Slice3", "MAX_depth_cm_Slice4"))+
  facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
  
  geom_point(data = subset(hyd_dep, variable =="MAX_depth_cm_Slice1"), aes(y=5, x=newx1a1), color="green") +
  geom_point(data = subset(hyd_dep, variable =="MAX_depth_cm_Slice2"), aes(y=5, x=newx1a2), color="green") +
  geom_point(data = subset(hyd_dep, variable =="MAX_depth_cm_Slice3"), aes(y=5, x=newx1a3), color="green") +
  geom_point(data = subset(hyd_dep, variable =="MAX_depth_cm_Slice4"), aes(y=5, x=newx1a4), color="green") +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  labs(title = "Willow Germination: Depth ~ Q",
       y = "Depth (cm)",
       x = "Q (cfs)") #+ theme_bw(base_size = 15)

dev.off()

head(time_stats)
## melt
melt_time<-reshape2::melt(time_stats, id=c("water_year", "position", "season"))
melt_time <- rename(melt_time, Time_Period = variable) %>%
  distinct()
head(melt_time)
write.csv(melt_time, "output_data/01_willow_germ_depth_time_stats_long.csv")

# Number of days above discharge ------------------------------------------

## count number events within each threshold with a running total - max total is the number of consequative 
# events (hours) per day. if else statements to consider the thresholds newx1a/b etc
## order by datetime

## change year to water year and count hours within Q range
new_data1  <- new_datax %>% 
  dplyr::filter(variable == "MAX_depth_cm_Slice1") %>%
  group_by(month, day, water_year, ID = data.table::rleid(Q >= newx1a1)) %>%
  mutate(threshold = if_else(Q >= newx1a1,  row_number(), 0L))%>%
  mutate(position="Slice1")

new_data2  <- new_datax %>% 
  dplyr::filter(variable == "MAX_depth_cm_Slice2") %>%
  group_by(month, day, water_year, ID = data.table::rleid(Q >= newx1a2)) %>%
  mutate(threshold = if_else(Q >= newx1a2,  row_number(), 0L))%>%
  mutate(position="Slice2")

new_data3  <- new_datax %>% 
  dplyr::filter(variable == "MAX_depth_cm_Slice1") %>%
  group_by(month, day, water_year, ID = data.table::rleid(Q >= newx1a3)) %>%
  mutate(threshold = if_else(Q >= newx1a3,  row_number(), 0L))%>%
  mutate(position="Slice3")

new_data4  <- new_datax %>% 
  dplyr::filter(variable == "MAX_depth_cm_Slice4") %>%
  group_by(month, day, water_year, ID = data.table::rleid(Q >= newx1a4)) %>%
  mutate(threshold = if_else(Q >= newx1a4,  row_number(), 0L))%>%
  mutate(position="Slice4")

## melt data frame so that each probability column are all in one row 
## select only columns needed - Q, month, water_year, day all IDs and probs
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

# ## count days in each year for time thing
# total_days_per_year01 <- total_days_per_month01 %>%
#   group_by(water_year) %>%
#   summarise(days_per_water_year = sum(n_days_low)) %>%
#   mutate(suitablility = ifelse(days_per_water_year >= 85 & days_per_water_year <= 280, "Yes", "No"))

total_days <- total_days_per_month01
head(total_days)
total_days_year <- total_days_per_year01
total_days_year
write.csv(total_days, "output_data/01_willow_germ_depth_total_days.csv")
write.csv(total_days_year, "output_data/01_willow_germ_depth_total_days_year.csv")

# # create water_year_month column       
total_days <- ungroup(total_days) %>%
  unite(month_year, water_year:month, sep="-", remove=F)


## convert month water_year to date format
library(zoo)
total_days$month_year <-  zoo::as.yearmon(total_days$month_year)
total_days$month_year <- as.Date(total_days$month_year)


## define seasons/critical period
non_critical <- c(7:11) 
critical <- c(12, 1:6) 

total_days <- total_days %>%
  mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") ) #%>%
# pivot_wider(names_from = season, values_from = days_per_month)



# ## melt data

melt_days<-reshape2::melt(total_days, id=c("month_year", "water_year", "month", "season", "position"))
melt_days <- rename(melt_days, Annual = variable,
                    n_days = value)


head(melt_days)
melt_days

## save df
write.csv(melt_days, "output_data/01_willow_germ_depth_total_days_long.csv")



