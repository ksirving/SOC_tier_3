### Arroyo Chub

library(tidyverse)
library(tidyr)
library(sm)
library(lubridate) # work with dates
library(dplyr)     # data manipulation (filter, summarize, mutate)
library(ggplot2)   # graphics
library(gridExtra) # tile several plots next to each other
library(scales)
library(data.table)

depth_chub <- read.csv("input_data/00_Wulff_Chub_depth_velocity_abundance.csv")
depth_chub <- depth_chub[,-1]

head(depth_chub)


## make curve

depth_freq_chub <- depth_chub %>% 
  uncount(Abundance)
hist(depth_freq_chub$Depth)
hist(depth_freq_chub$Velocity_0.6_ms)


## probability curve - histogram scaled and centered depth, then transformed back to raw depth

## chub
depth_freq_chub$Scaled_Depth <-scale(depth_freq_chub$Depth, scale=T, center=T)
scaled_x <- depth_freq_chub$Scaled_Depth
h <- hist(scaled_x, plot=F)
xfit_c<-seq(min(scaled_x),max(scaled_x),length=200)
yfit_c<-dnorm(xfit_c,mean=mean(scaled_x),sd=sd(scaled_x))
## x axis with raw depth values
xfit_r_c <- seq(min(depth_freq_chub$Depth), max(depth_freq_chub$Depth), length=200)


## data frame with probabilities and depth - to combine with hydraulic data

fitdata_c <- data.frame(matrix(ncol=2, nrow=length(yfit_c)))
fitdata_c[,1] <- xfit_r_c
fitdata_c[,2] <- yfit_c
colnames(fitdata_c) <- c("depth_fit", "prob_fit")
head(fitdata_c)

## use smooth spline to predict on new data set
new_values_chub <-smooth.spline(fitdata_c$depth_fit, fitdata_c$prob_fit)


## make DF of new data to predict
new_data <- as.data.frame(seq(0,140, 0.5))
colnames(new_data) <- "value"

all_data <- new_data %>%
  dplyr::mutate(prob_fit = predict(new_values_chub, value)$y) %>%
  rename(depth_cm = value) 

head(all_data)

write.csv(all_data, "output_data/03_chub_adult_depth_prob_curve_data.csv")

#### velocity
head(depth_freq_chub)
vel_freq <- rename(depth_freq_chub, Velocity = Velocity_0.6_ms)
vel_freq <- na.omit(vel_freq)
## probability curve
vel_freq$Scaled_Vel <-scale(vel_freq$Velocity, scale=T, center=T)
scaled_x <- vel_freq$Scaled_Vel
h <- hist(scaled_x, plot=F)
xfit<-seq(min(scaled_x),max(scaled_x),length=1000)
yfit<-dnorm(xfit,mean=mean(scaled_x),sd=sd(scaled_x))

## x axis with raw velocity values
xfit_r <- seq(min(vel_freq$Velocity), max(vel_freq$Velocity), length=1000)

## plot curve with raw depth axis
png("figures/03_chub_Adult_velocity_Prob_curve.png", width = 700, height = 700)

plot(xfit_r, yfit, axes=FALSE, xlab='', ylab='', type='l', col='', main = "" )
axis(1, at=pretty(xfit_r), cex.axis=2)
par(new=TRUE)
#plot the line with no axes or labels
plot(xfit, yfit, axes=FALSE, xlab='Velocity (m/s)', ylab='Probability', type='l', col='red', main = "Adult/Velocity",
     cex.main = 2, cex.axis=2, cex.lab=2)
## add 1sd shift
par(new=TRUE)

#add these now with axis

axis(2, at=pretty(range(yfit)), cex.axis=2)
dev.off()
## data frame with probabilities and depth

fitdata <- data.frame(matrix(ncol=2, nrow=length(yfit)))
fitdata[,1] <- xfit_r
fitdata[,2] <- yfit
colnames(fitdata) <- c("velocity_fit", "prob_fit")

write.csv(fitdata, "output_data/03_chub_adult_velocity_prob_curve_data.csv")

