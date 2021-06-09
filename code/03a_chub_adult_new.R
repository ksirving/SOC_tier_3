## adding extra chub data and new model

getwd()
library(tidyverse)
library(tidyr)
library(sm)
library(lubridate) # work with dates
library(dplyr)     # data manipulation (filter, summarize, mutate)
library(ggplot2)   # graphics
library(gridExtra) # tile several plots next to each other
library(scales)
library(data.table)

# SAR 2015 
library(tidyverse)
# upload data

micro_avail <- read.csv("/Users/katieirving/Documents/git/flow_eco_mech/input_data/SAR 2015 Microhabitat Availability.csv")
micro_use <- read.csv("/Users/katieirving/Documents/git/flow_eco_mech/input_data/SAR 2015 Microhabitat Use.csv")
fish_data <- read.csv("/Users/katieirving/Documents/git/flow_eco_mech/input_data/SAR 2015 Reach Fish Data.csv")

colnames(micro_avail)[8:13] <- c("Transect", "ch_width_m", "Depth_cm", "Velocity_0.6_ms","Velocity_0.2_ms", "Velocity_0.8_ms")
micro_avail <- micro_avail[,-c(4, 19,20)]
names(micro_avail)
## make coord code
micro_avail$coord_code <- paste(micro_avail$Latitude, "_", micro_avail$Longitude, sep="")
unique(micro_avail$coord_code) ## 17
head(micro_avail)


names(micro_use) [14:20] <- c("length_mm", "Distance_fish_abv_bottom_cm", "Velocity_at_fish_ms", "Depth_cm", "Velocity_0.6_ms", "Velocity_0.2_ms", "Velocity_0.8_ms")
head(micro_use)
micro_use$coord_code <- paste(micro_use$Latitude, "_", micro_use$Longitude, sep="")
unique(micro_use$coord_code) # 11 sites in total
dim(micro_use)

unique(micro_avail$coord_code) %in% unique(micro_use$coord_code) 

#  make sure species name is homogenous
# micro_usex
micro_use$Species <- gsub("Santa Ana sucker", "SantaAnaSucker", micro_use$Species)
micro_use$Species <- gsub("Santa Ana Sucker", "SantaAnaSucker", micro_use$Species)
micro_use$Species <- gsub("Arroyo Chub", "ArroyoChub", micro_use$Species)
micro_use$Species <- gsub("Yellow bullhead", "Yellowbullhead", micro_use$Species)
micro_use$Species <- gsub("Western Misquitofish", "WesternMisquitofish", micro_use$Species)
fish <- unique(na.omit(micro_use$Species))
fish


micro_suck <- subset(micro_use, Species %in% fish)
dim(micro_suck) # 334 27

unique(micro_suck$coord_code) 
unique(micro_avail$coord_code) 

### create df pf presence/absence (abundance/absence)
## format each df - availability and use
head(micro_avail)
head(micro_suck)
names(micro_avail)
micro_availx <- micro_avail %>%
  select(Section, Latitude, Longitude, Date,  Depth_cm, Velocity_0.6_ms:Velocity_0.8_ms, coord_code) %>%
  mutate("SantaAnaSucker" = 0, "ArroyoChub" = 0, "Yellowbullhead" = 0, "WesternMisquitofish" = 0)

names(micro_suck)
micro_suckx <- micro_suck %>%
  pivot_wider(names_from = "Species", values_from = "Number") %>%
  select(Section, Latitude, Longitude, Date, Depth_cm, Velocity_0.6_ms:Velocity_0.8_ms, SantaAnaSucker:WesternMisquitofish, coord_code)

## change all NAs to 0

micro_suckx$SantaAnaSucker[is.na(micro_suckx$SantaAnaSucker)] <- 0
micro_suckx$ArroyoChub[is.na(micro_suckx$ArroyoChub)] <- 0
micro_suckx$Yellowbullhead[is.na(micro_suckx$Yellowbullhead)] <- 0
micro_suckx$WesternMisquitofish[is.na(micro_suckx$WesternMisquitofish)] <- 0

## find locations not in use df

presentlocations <- which(unique(micro_availx$coord_code) %in% unique(micro_suckx$coord_code))
removetheselocations <- unique(micro_suckx$coord_code)
removetheselocations
micro_avail_abs <- micro_availx %>%
  filter(!coord_code %in% removetheselocations)

## check
unique(micro_avail_abs$coord_code) %in% unique(micro_suckx$coord_code)
unique(micro_suckx$coord_code) %in% unique(micro_avail_abs$coord_code) ## no matches

## combine dfs
head(micro_avail_abs)
head(micro_suckx)

names(micro_avail_abs)
names(micro_suckx)

str(micro_avail_abs)
str(micro_suckx)

## format 
micro_avail_abs$Velocity_0.2_ms <- as.numeric(micro_avail_abs$Velocity_0.2_ms)
micro_avail_abs$Velocity_0.8_ms <- as.numeric(micro_avail_abs$Velocity_0.8_ms)

micro_suckx$SantaAnaSucker <- as.numeric(micro_suckx$SantaAnaSucker)
micro_suckx$ArroyoChub <- as.numeric(micro_suckx$ArroyoChub)
micro_suckx$Yellowbullhead <- as.numeric(micro_suckx$Yellowbullhead)
micro_suckx$WesternMisquitofish <- as.numeric(micro_suckx$WesternMisquitofish)

data_pa <- bind_rows(micro_avail_abs, micro_suckx)

#  as all adults, only micro_suck needed

# save 

write.csv(data_pa, "output_data/03a_all_species_pres_abs_wulff_2015.csv")

#  SAR 2016 

# upload data

micro_avail <- read.csv("/Users/katieirving/Documents/git/flow_eco_mech/input_data/SAR 2016 Microhabitat Availability Data.csv")
micro_use <- read.csv("/Users/katieirving/Documents/git/flow_eco_mech/input_data/SAR 2016 Microhabitat Use Data.csv")
fish_data <- read.csv("/Users/katieirving/Documents/git/flow_eco_mech/input_data/SAR 2016 Reach Fish Data_v3_2.05.2019.csv")
reach_data <- read.csv("/Users/katieirving/Documents/git/flow_eco_mech/input_data/SAR 2016 Reach Habitat Data.csv")

head(micro_avail)

colnames(micro_avail)[7:9] <- c("ch_width_m", "Depth_cm", "Velocity_0.6_ms")
# micro_avail <- micro_avail[,-c(4, 19,20)]
names(micro_avail)
names(micro_use)
## make coord code
micro_avail$coord_code <- paste(micro_avail$Latitude, "_", micro_avail$Longitude,  sep="")
unique(micro_avail$coord_code) ## 40
head(micro_avail)

names(micro_use) [14:20] <- c("length_mm", "Distance_fish_abv_bottom_cm", "Velocity_at_fish_ms", "Depth_cm", "Velocity_0.6_ms", "Velocity_0.2_ms", "Velocity_0.8_ms")
names(micro_use)
micro_use$coord_code_e <- paste(micro_use$Ending.Latitude, "_", micro_use$Ending.Longitude, sep="")
# "34.03493_-117.35665" 
# "34.03561_-117.35677" 
# "34.03776_-117.35596" ## 33

micro_use$coord_code_s <- paste(micro_use$Starting.Latitude, "_", micro_use$Starting.Longitude, sep="")
#"34.03299_-117.3561" 
# "34.03493_-117.35665" 
#"34.03662_-117.35625" number 22
unique(micro_use$coord_code) # 3 sites in total
dim(micro_use)

unique(micro_avail$coord_code) %in% unique(micro_use$coord_code_e) 
unique(micro_avail$coord_code) %in% unique(micro_use$coord_code_s) 
av_codes <- paste(unique(micro_avail$coord_code)[22], unique(micro_avail$coord_code)[33])
av_codes
## find locations to check if they are the same

av_test <- micro_avail %>%
  filter(coord_code %in% c("34.03776_-117.35596", "34.03662_-117.35625"))

use_test <- micro_use %>%
  filter(coord_code_e == "34.03776_-117.35596", 
         coord_code_s == "34.03662_-117.35625")

### start and end point relates to one location in availability. just use presence for this df

#  make sure species name is homogenous
# micro_usex
# micro_use$Species <- gsub("Santa Ana sucker", "SantaAnaSucker", micro_use$Species)
micro_use$Species <- gsub("Santa Ana Sucker", "SantaAnaSucker", micro_use$Species)
micro_use$Species <- gsub("Arroyo Chub", "ArroyoChub", micro_use$Species)
# micro_use$Species <- gsub("Yellow bullhead", "Yellowbullhead", micro_use$Species)
# micro_use$Species <- gsub("Western Misquitofish", "WesternMisquitofish", micro_use$Species)
fish <- unique(na.omit(micro_use$Species))
fish


micro_suck <- subset(micro_use, Species %in% fish)
dim(micro_suck) # 144 27

unique(micro_suck$coord_code) 
unique(micro_avail$coord_code) 

### create df pf presence/absence (abundance/absence)
## format each df - availability and use
# head(micro_avail)
# head(micro_suck)
# names(micro_avail)
micro_availx <- micro_avail %>%
  select(Section, Latitude, Longitude, Date,  Depth_cm, Velocity_0.6_ms, coord_code) %>%
  mutate("SantaAnaSucker" = 0, "ArroyoChub" = 0)

names(micro_suck)
micro_suckx <- micro_suck %>%
  pivot_wider(names_from = "Species", values_from = "Count") %>%
  select(Section, Starting.Latitude, Starting.Longitude, Date, Depth_cm, Velocity_0.6_ms, 
         SantaAnaSucker:ArroyoChub, coord_code_s) %>%
  rename(Latitude = Starting.Latitude, Longitude = Starting.Longitude, coord_code = coord_code_s)

## change all NAs to 0

micro_suckx$SantaAnaSucker[is.na(micro_suckx$SantaAnaSucker)] <- 0
micro_suckx$ArroyoChub[is.na(micro_suckx$ArroyoChub)] <- 0
# micro_suckx$Yellowbullhead[is.na(micro_suckx$Yellowbullhead)] <- 0
# micro_suckx$WesternMisquitofish[is.na(micro_suckx$WesternMisquitofish)] <- 0

## find locations not in use df

presentlocations <- which(unique(micro_availx$coord_code) %in% unique(micro_suckx$coord_code))
removetheselocations <- unique(micro_suckx$coord_code)
removetheselocations
micro_avail_abs <- micro_availx %>%
  filter(!coord_code %in% removetheselocations)

## check
unique(micro_avail_abs$coord_code) %in% unique(micro_suckx$coord_code)
unique(micro_suckx$coord_code) %in% unique(micro_avail_abs$coord_code) ## no matches

## combine dfs
head(micro_avail_abs)
head(micro_suckx)

names(micro_avail_abs)
names(micro_suckx)

str(micro_avail_abs)
str(micro_suckx)

## format 
# micro_avail_abs$Velocity_0.2_ms <- as.numeric(micro_avail_abs$Velocity_0.2_ms)
# micro_avail_abs$Velocity_0.8_ms <- as.numeric(micro_avail_abs$Velocity_0.8_ms)

# micro_suckx$SantaAnaSucker <- as.numeric(micro_suckx$SantaAnaSucker)
# micro_suckx$ArroyoChub <- as.numeric(micro_suckx$ArroyoChub)
# micro_suckx$Yellowbullhead <- as.numeric(micro_suckx$Yellowbullhead)
# micro_suckx$WesternMisquitofish <- as.numeric(micro_suckx$WesternMisquitofish)

data_pa <- bind_rows(micro_avail_abs, micro_suckx)
# save 

write.csv(data_pa, "output_data/03a_all_species_pres_abs_wulff_2016.csv")

## upload data and merge

depth2015 <- read.csv("output_data/03a_all_species_pres_abs_wulff_2015.csv")
depth2016 <- read.csv("output_data/03a_all_species_pres_abs_wulff_2016.csv")

names(depth2015)
names(depth2016)
head(depth2015)
head(depth2016)

depth2015 <- depth2015 %>%
  pivot_longer(SantaAnaSucker:WesternMisquitofish, names_to = "Species", values_to = "Abundance") %>%
  select(Section:Velocity_0.6_ms, coord_code:Abundance) %>%
  mutate(Year = 2015, Source = "Wulff_etal")
  

depth2016 <- depth2016 %>%
  pivot_longer(SantaAnaSucker:ArroyoChub, names_to = "Species", values_to = "Abundance") %>%
  select(-X) %>%
  mutate(Year = 2016, Source = "Wulff_etal")

str(depth2015)
str(depth2016)

depth2016 <- depth2016 %>%
  mutate(Section = as.character(Section), Latitude = as.character(Latitude), Longitude = as.character(Longitude),
         Depth_cm = as.numeric(Depth_cm))

depth <- bind_rows(depth2016, depth2015)

write.csv(depth, "output_data/03a_Wulff_ALL_depth_vel_abundance.csv")


# Model -------------------------------------------------------------------

library(tidyverse)

data <- read.csv("output_data/03a_Wulff_ALL_depth_vel_abundance.csv")
head(data)

dataSum <- data %>%
  group_by(coord_code, Date, Species, Year) %>%
  mutate(TotalAbundance = sum(Abundance)) %>%
  group_by(Depth_cm) %>%
  mutate(RelAbundance = (Abundance/TotalAbundance)*100)

dataSum$RelAbundance[is.na(dataSum$RelAbundance)] <- 0
  
  
depth_chub <- dataSum %>% filter(Species == "ArroyoChub") 

## get relative abundance - try for weighting and/or lm

dim(depth_chub) ## 1286
head(depth_chub)
str(chub)
## make presence absence column

chub <- depth_chub %>%
  ungroup() %>%
  mutate(presence_absence = ifelse(Abundance == 0,0,1)) %>%
  # filter(Year == 2015) %>% ## remove 2016 to check
  mutate(Depth_m = Depth_cm/100) ## change to m

names(chub)
## model
?glm
glmmod1 <- glm(presence_absence~Depth_cm, family=binomial(link="logit"), data=chub)
summary(glmmod1)

new_data <- chub$Depth_cm

## add it to the data set
chub$PredictedProbability <- predict(glmmod1,list(Depth_cm = new_data),type="resp")
## here you apply the points and curve manually
ggplot(chub, aes(x=Depth_cm, y=PredictedProbability))+ ## x here is your probability from the model
  geom_path()+
  scale_y_continuous(limits=c(0,1))+
  scale_x_continuous() +
  geom_point(aes(y=presence_absence, x=Depth_cm), colour = 'black', size = 1) 

ggplot(chub, aes(x=Depth_m, y=presence_absence))+ 
  stat_smooth(method = "glm")+
  geom_point(aes(y=presence_absence, x=Depth_m), colour = 'black', size = 1) +
  scale_y_continuous(limits=c(0,1)) 

### linear model with quadratic term


ggplot(data = chub, mapping = aes(x = Depth_cm, y = RelAbundance))+
  geom_point(alpha = 0.2, size = 3)+
  labs(x = "Depth (cm)", y = "Occurrence")+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2))+
  scale_y_continuous(limits=c(0,1))+
  theme(text = element_text(size=20), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.5))



summary(depth_chub_mod <- lm(RelAbundance ~ Depth_cm + I(Depth_cm^2), data = chub))

ggplot(data = chub, mapping = aes(x = Depth_cm, y = RelAbundance))+
  geom_point(alpha = 0.2, size = 3)+
  labs(x = "Depth (cm)", y = "Occurrence")+
  geom_smooth(method = "lm")+
  scale_y_continuous(limits=c(0,1))+
  theme(text = element_text(size=20), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.5))

summary(depth_chub_mod2 <- lm(Abundance ~ Depth_cm, data = chub))

##### model to use

glmmod1 <- glm(presence_absence~Depth_cm, family=binomial(link="logit"), data=chub)
summary(glmmod1)
range(chub$Depth_cm)
xdepth <- seq(0, 120, 0.01)

ydepth <- predict(glmmod1, list(Depth_cm = xdepth),type="response")

plot(chub$Depth_cm, chub$presence_absence, pch = 16, xlab = "Depth (cm)", ylab = "Prob of Occurrence")
lines(xdepth, ydepth)

## velocity
names(chub)

# chub2 <- chub %>% drop_na(Velocity_0.6_ms)

glmmod2 <- glm(presence_absence~Velocity_0.6_ms, family=binomial(link="logit"), data=chub2)
summary(glmmod2)
range(chub2$Velocity_0.6_ms)
xvel <- seq(0, 1.72, 0.01)

yvel <- predict(glmmod2, list(Velocity_0.6_ms = xvel),type="response")
yvel
plot(chub2$Velocity_0.6_ms, chub2$presence_absence, pch = 16, xlab = "Velocity (ms)", ylab = "Prob of Occurrence")
lines(xvel, yvel)

summary(vel_chub_mod2 <- lm(RelAbundance ~ Velocity_0.6_ms, data = chub))
yvel <- predict(vel_chub_mod2, list(Velocity_0.6_ms = xvel),type="response")
yvel
plot(chub2$Velocity_0.6_ms, chub2$RelAbundance, pch = 16, xlab = "Velocity (ms)", ylab = "Relative Abundance")
lines(xvel, yvel)

### jenny's chub data

all_data  <- read.csv("/Users/katieirving/Documents/Projects/LA_River/Bio/Species_Occurrence_unique.csv")
head(all_data)
names(all_data)

chub_jenny <- all_data %>%
  filter(name == "arroyo chub") %>%
  select(date:count, depth.cm, source) %>%
  rename(Date = date, Depth_cm = depth.cm, Species = name, presence_absence = occurrence,
         Abundance = count, Source = source) %>%
  mutate(Latitude = as.character(latitude), Longitude = as.character(longitude),
         presence_absence = as.numeric(presence_absence)) %>%
  select(-latitude, -longitude)


names(chub_jenny)
names(chub)

chub_depth <- chub %>%
  select(-X,-Section,  - Velocity_0.6_ms, -coord_code, -Year, -PredictedProbability)

dim(chub_jenny)
names(chub_jenny)
names(chub_depth)

str(chub_jenny)
str(chub_depth)

depth <- bind_rows(chub_jenny, chub_depth)
head(depth)

depth <- depth %>% drop_na(Depth_cm)

### model with all data

glmmod1 <- glm(presence_absence~Depth_cm, family=binomial(link="logit"), weights=Abundance, data=depth)
summary(glmmod1)
range(depth$Depth_cm)
xdepth <- seq(0, 670.56, 0.01)


ydepth <- predict(glmmod1, list(Depth_cm = xdepth),type="response")

plot(depth$Depth_cm, depth$presence_absence, pch = 16, xlab = "Depth (cm)", ylab = "Prob of Occurrence")
lines(xdepth, ydepth)

new_data <- depth$Depth_cm

## add it to the data set
depth$PredictedProbability <- predict(glmmod1,list(Depth_cm = new_data),type="resp")
## here you apply the points and curve manually
ggplot(depth, aes(x=Depth_cm, y=PredictedProbability))+ ## x here is your probability from the model
  geom_path()+
  scale_y_continuous(limits=c(0,1))+
  scale_x_continuous() +
  geom_point(aes(y=presence_absence, x=Depth_cm), colour = 'black', size = 1) 

ggplot(depth, aes(x=Depth_cm, y=presence_absence))+ 
  geom_smooth(method = "glm")+
  geom_point(aes(y=presence_absence, x=Depth_cm), colour = 'black', size = 1) +
  scale_y_continuous(limits=c(0,1)) 

### linear model with quadratic term

#patch occurrence
ggplot(data = depth, mapping = aes(x = Depth_cm, y = presence_absence))+
  geom_point(alpha = 0.2, size = 3)+
  labs(x = "Depth (cm)", y = "Occurrence")+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2))+
  scale_y_continuous(limits=c(0,1))+
  theme(text = element_text(size=20), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.5))



summary(depth_chub_mod <- lm(presence_absence ~ Depth_cm + I(Depth_cm^2), data = depth))

ggplot(data = depth, mapping = aes(x = Depth_cm, y = presence_absence))+
  geom_point(alpha = 0.2, size = 3)+
  labs(x = "Depth (cm)", y = "Occurrence")+
  geom_smooth(method = "lm")+
  scale_y_continuous(limits=c(0,1))+
  theme(text = element_text(size=20), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.5))

summary(depth_chub_mod2 <- lm(Abundance ~ Depth_cm, data = depth))

sum(depth$Depth_cm >10)

range(chub$Depth_cm)

## remove anything above 300 - check range of soc study area

depth <- depth %>%
  filter(Depth_cm <= 300)
  

glmmod1 <- glm(presence_absence~Depth_cm, family=binomial(link="logit"), data=depth)
summary(glmmod1)

new_data <- depth$Depth_cm

## add it to the data set
depth$PredictedProbability <- predict(glmmod1,list(Depth_cm = new_data),type="resp")
## here you apply the points and curve manually
ggplot(depth, aes(x=Depth_cm, y=PredictedProbability))+ ## x here is your probability from the model
  geom_path()+
  scale_y_continuous(limits=c(0,1))+
  scale_x_continuous() +
  geom_point(aes(y=presence_absence, x=Depth_cm), colour = 'black', size = 1) 

ggplot(depth, aes(x=Depth_cm, y=presence_absence))+ 
  geom_smooth(method = "glm")+
  geom_point(aes(y=presence_absence, x=Depth_cm), colour = 'black', size = 1) +
  scale_y_continuous(limits=c(0,1)) 

### linear model with quadratic term

#patch occurrence
ggplot(data = depth, mapping = aes(x = Depth_cm, y = presence_absence))+
  geom_point(alpha = 0.2, size = 3)+
  labs(x = "Depth (cm)", y = "Occurrence")+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2))+
  scale_y_continuous(limits=c(0,1))+
  theme(text = element_text(size=20), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.5))



summary(depth_chub_mod <- lm(presence_absence ~ Depth_cm + I(Depth_cm^2), data = depth))

ggplot(data = depth, mapping = aes(x = Depth_cm, y = presence_absence))+
  geom_point(alpha = 0.2, size = 3)+
  labs(x = "Depth (cm)", y = "Occurrence")+
  geom_smooth(method = "lm")+
  scale_y_continuous(limits=c(0,1))+
  theme(text = element_text(size=20), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.5))

summary(depth_chub_mod2 <- lm(Abundance ~ Depth_cm, data = depth))


