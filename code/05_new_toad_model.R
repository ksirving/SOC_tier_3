### Toad and flow from Jenny's model

## Katie Irving

library(tidyverse)
library(sf)
## bio data 

load(file = "/Users/katieirving/Documents/git/flowecology-master/data/biodat.RData") ## biodat
head(biodat)
str(biodat)

## format to dataframe

biodat <- st_drop_geometry(biodat)
  

## filter to toad only

toad <- biodat %>%
  filter(spp == "toad")

head(toad)
dim(toad)

range(toad$yr) ## 1991 2017
sort(unique(toad$yr))
## flow data

load(file= "/Users/katieirving/Documents/git/flowecology-master/data/bsflowmetest.RData") ## bsflowmetest
head(bsflowmetest)
str(bsflowmetest)
unique(bsflowmetest$met)

flowmets <- bsflowmetest %>%
  filter(met %in% c("all_Hydroperiod",
                    "all_MedianNoFlowDays",
                    "all_FracYearsNoFlow"))

range(flowmets$dts) #1993 2014

head(flowmets)
unique(flowmets$dts)

flowmets_wide <- flowmets %>%
  pivot_wider(names_from = met, values_from = val)


### combine dfs

unique(toad$COMID) %in% unique(flowmets_wide$COMID) ## all toad comids are included in flow

all_data <- merge(toad, flowmets_wide, by="COMID")
view(all_data)
head(all_data)
unique(all_data$occurrence)

all_data <- all_data %>%
  mutate(occurrencex = ifelse(occurrence == "present", 1, 0)) 

summary(hp_mod <- glm(occurrencex~all_Hydroperiod, family=binomial(link="logit"), data=all_data))
summary(hp_mod2 <- lm(occurrencex~all_Hydroperiod + I(all_Hydroperiod^2), data=all_data))

summary(fy_mod <- glm(occurrencex~all_FracYearsNoFlow, family=binomial(link="logit"), data=all_data))
summary(mf_mod <- glm(occurrence~all_MedianNoFlowDays, family=binomial(link="logit"), data=all_data))

all_datax <- all_data %>%
  mutate(HydroperiodPredict = predict(hp_mod, newdata = all_data, type="response"))


ggplot(all_datax, aes(x=all_Hydroperiod, y=occurrencex))+
  geom_smooth(method = "glm")+
  scale_y_continuous(limits=c(0,1))

ggplot(all_datax, aes(x=all_Hydroperiod, y=occurrencex))+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2))+
  scale_y_continuous(limits=c(0,1))

xvals <- seq(0, 1, 0.01) ## or your range of x values

yvals <- predict(hp_mod2, list(all_Hydroperiod = xvals), type="response")
yvals
# yvalsScaled = (yvals-min(yvals))/
#   (max(yvals)-min(yvals))
# yvalsScaled ## scaling may help
plot(x=all_data$all_Hydroperiod,y=all_data$occurrencex)
# lines(xvals, yvalsScaled)
lines(xvals, yvals)


####### Jenny model individual models

load(file = "/Users/katieirving/Documents/git/flowecology-master/data/obsbiomet.RData")
head(obsbiomet)

flowmets <- obsbiomet %>%
  filter(met %in% c("all_Hydroperiod",
                    "all_MedianNoFlowDays",
                    "all_FracYearsNoFlow"),
         spp =="toad")


hp_data <- flowmets %>%
  filter(met == "all_Hydroperiod") %>%
  rename(all_Hydroperiod = val)
head(hp_data)

library(ResourceSelection)
install.packages("ResourceSelection")

summary(hp_mod <- glm(occurrence~all_Hydroperiod, family=binomial(link="logit"), data=hp_data))
with(summary(hp_mod), 1 - deviance/null.deviance)
hoslem.test(hp_data$occurrence, fitted(hp_mod))


# data:  hp_data$occurrence, fitted(hp_mod)
# X-squared = 1.9323, df = 8, p-value = 0.983

ggplot(hp_data, aes(x=all_Hydroperiod, y=occurrence))+
  geom_smooth(method = "glm")#+
  scale_y_continuous(limits=c(0,1))


fd_data <- flowmets %>%
  filter(met == "all_MedianNoFlowDays") %>% 
  rename(all_MedianNoFlowDays = val)

summary(fd_mod <- glm(occurrence~all_MedianNoFlowDays, family=binomial(link="logit"), data=fd_data))
with(summary(fd_mod), 1 - deviance/null.deviance)
hoslem.test(fd_data$occurrence, fitted(fd_mod))

# data:  fd_data$occurrence, fitted(fd_mod)
# X-squared = 0.77195, df = 8, p-value = 0.9993

ggplot(fd_data, aes(x=all_MedianNoFlowDays, y=occurrence))+
  geom_smooth(method = "glm")#+
  # scale_y_continuous(limits=c(0,1))

fy_data <- flowmets %>%
  filter(met == "all_FracYearsNoFlow") %>% ## not significant
  rename(all_FracYearsNoFlow = val)

summary(fy_mod <- glm(occurrence~all_FracYearsNoFlow, family=binomial(link="logit"), data=fy_data))
with(summary(fy_mod), 1 - deviance/null.deviance)
hoslem.test(fy_data$occurrence, fitted(fy_mod))

# data:  fy_data$occurrence, fitted(fy_mod)
# X-squared = 1.3732, df = 8, p-value = 0.9946
