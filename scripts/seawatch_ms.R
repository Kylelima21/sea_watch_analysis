#------------------------------------------------#
####           Packages and Source            ####
#------------------------------------------------#

## Packages
library(tidyverse)
library(lubridate)
library(scales)
library(lmerTest)
library(ggeffects)
library(DHARMa)
library(AICcmodavg)
library(lmtest)
library(glmmTMB)
library(MuMIn)

## Source custom functions
source("scripts/sea_watch_functions.R")




#------------------------------------------------#
####     Read and clean manipulated data      ####
#------------------------------------------------#

## Clean
swdat <- tibble(read.csv("data/sea_watch_cleaned_data_20241216.csv")) %>% 
  select(-c(time.of.obs, start.time.2, wind.dir.speed, visibility)) %>% 
  rename(observer = principle.observer,
         count = day.total) %>% 
  mutate(date = as.Date(date),
         start.datetime = paste(date, start.time),
         start.datetime = gsub("\\d*\\-\\d*\\-\\d*\\s\\NA", "NA", start.datetime),
         start.datetime = ifelse(start.datetime == "NA", NA, start.datetime),
         start.datetime = ymd_hm(start.datetime, tz = "America/New_York"),
         observer = ifelse(observer == "SB/ZK", "SB/RZK", observer),
         observer = ifelse(observer == "RZK/SB", "SB/RZK", observer),
         observer = ifelse(observer == "STB/RZK", "SB/RZK", observer),
         observer = ifelse(observer == "ZK", "RZK", observer),
         species = ifelse(species == "SUSC/BLSC", "BLSC/SUSC", species)) %>% 
  filter(year != 2014 & year != 2015,
         species != "GBBG",
         species != "HERG") %>% 
  select(-c(wind.direction, visibility.score))




#------------------------------------------------#
####           Add in weather data            ####
#------------------------------------------------#

## Read in weather data from the Schoodic D2258 weather station
schweather <- read.csv("data/schoodic_weather_data_D2258_20241119.csv") %>% 
  filter(altimeter_set_1 != "Pascals") %>% 
  as_tibble() %>% 
  rename_with(., ~str_remove(., "_set_1d"), .cols = everything()) %>% 
  rename_with(., ~str_remove(., "_set_1"), .cols = everything()) %>% 
  rename_with(., tolower, .cols = everything()) %>% 
  rename_with(., ~str_replace_all(., "_", "."), .cols = everything()) %>% 
  select(date.time, wind.speed, wind.direction, pressure) %>% 
  mutate(datetime = as_datetime(date.time, tz = "America/New_York"),
         #datetime = as.POSIXct(format(datetime, tz="America/New_York", usetz = TRUE)),
         #datetime = as_datetime(datetime, tz = "America/New_York"),
         date = date(datetime),
         wind.speed = as.numeric(wind.speed),
         wind.direction = as.numeric(wind.direction) - 1,
         pressure = round(as.numeric(pressure), digits = 3))


## Get a list of the dates of survey from the seawatch data
dates <- swdat %>% 
  select(date, total.obs.mins, start.datetime) %>% 
  distinct() %>% 
  mutate(total.obs.mins2 = total.obs.mins*60,
         end.datetime = start.datetime + total.obs.mins2) %>% 
  select(date, start.datetime, end.datetime)


## Run a pmap function for each date and time combination, looping it through
## the avg_weather() function and compiling all into one long dataframe
avgweatherdat <- pmap_dfr(list(dates$date, dates$start.datetime, 
                               dates$end.datetime), avg_weather)


## Join this onto seawatch data for modelling
swdat2 <- swdat %>% 
  left_join(., avgweatherdat, by = "date")




#------------------------------------------------#
####              Summary Stats               ####
#------------------------------------------------#

## Number of days surveyed from 2016 - 2024 -- 655
swdat2 %>% 
  mutate(filt = ifelse(count == 0 & obs.hours == 0, "rm", "keep")) %>% 
  filter(filt == "keep") %>% 
  distinct(date) %>% 
  nrow()
  
# swdat2 %>% 
#   group_by(year, date) %>% 
#   distinct(year, date) %>% 
#   group_by(year)


## Total hours surveyed -- 2,563 hours
swdat2 %>% 
  select(date, obs.hours) %>% 
  distinct() %>% 
  summarise(time = sum(obs.hours))


## Number of observers -- 29 levels but 19 human observers
swdat2 %>% 
  filter(!is.na(observer)) %>% 
  distinct(observer) %>% 
  arrange(observer) %>% 
  print(n = nrow(.))


## Total hours by season and mean hours per season
hps <- swdat2 %>% 
  select(year, date, obs.hours) %>% 
  distinct() %>% 
  group_by(year) %>% 
  summarise(total.effort = sum(obs.hours))
hps

hps %>% 
  summarise(mean = mean(total.effort))


## Mean hours of effort per day for each season
swdat2 %>% 
  select(year, date, obs.hours) %>% 
  distinct() %>% 
  group_by(year) %>% 
  summarise(mean.daily.effort = mean(obs.hours))

swdat2 %>% 
  select(year, date, obs.hours) %>% 
  distinct() %>% 
  group_by(year) %>% 
  summarise(mean.daily.effort = mean(obs.hours)) %>% 
  summarise(mean = mean(mean.daily.effort))


## Total individual birds recorded -- 490,208 individuals
swdat2 %>%
  summarise(count = sum(count))


## Total individual birds and birds per hour of effort by year
swdat2 %>% 
  group_by(year) %>% 
  summarise(total.indiv = sum(count)) %>% 
  left_join(., hps, by = "year") %>% 
  mutate(bph = total.indiv/total.effort)


## Total individual birds by observer
# swdat2 %>% 
#   group_by(observer) %>% 
#   summarise(total.effort = sum(obs.hours),
#             total.indiv = sum(count)) %>% 
#   mutate(bph = total.indiv/total.effort) %>% 
#   arrange(-bph)


## Totals by species
tbs <- swdat2 %>%
  group_by(species) %>%
  summarise(total.indiv = sum(count)) %>%
  arrange(species)
tbs %>% 
  arrange(-total.indiv) %>% 
  print(n = 20)

# write.csv(tbs, "outputs/species_totals.csv", row.names = F)
  



#------------------------------------------------#
####        Format data for modelling         ####
#------------------------------------------------#

## Read in species groupings file
sp.groups <- read.csv("outputs/species_groupings.csv") 


## Format data, add in the species groupings, and clean covariate columns
twd <- swdat2 %>% 
  left_join(., sp.groups, by = "species") %>% 
  mutate(grouping2 = grouping,
         grouping2 = ifelse(species == "DCCO", "DC Cormorant", grouping2),
         grouping2 = ifelse(species == "COLO", "Common Loon", grouping2),
         grouping2 = ifelse(species == "LTDU", "LT Duck", grouping2),
         grouping2 = ifelse(species == "RBME", "RB Merganser", grouping2),
         grouping2 = ifelse(species == "WWSC", "WW Scoter", grouping2),
         grouping2 = ifelse(species == "BLSC", "Black Scoter", grouping2),
         grouping2 = ifelse(species == "SUSC", "Surf Scoter", grouping2),
         grouping2 = ifelse(species == "RTLO", "RT Loon", grouping2)) %>% 
  select(species, grouping, grouping2, date:count, wind.speed, wind.dir, pressure, 
         start.time, observer) %>% 
  mutate(filt = ifelse(count == 0 & obs.hours == 0, "rm", "keep"),
         start.time = str_replace(start.time, "^(\\d)[:punct:]", "0\\1\\:"),
         start.time = ifelse(!is.na(start.time), paste0(start.time, ":00"), start.time),
         start.time = ifelse(!is.na(start.time), paste0("2000-01-01 ", start.time),
                              start.time),
         start.time = as_datetime(start.time),
         midnight = as_datetime("2000-01-01 00:00:00"),
         tsm = as.numeric(difftime(start.time, midnight, units = "hours")),
         doy = yday(date)) %>% 
  filter(filt == "keep") %>% 
  select(-c(filt, midnight)) %>% 
  ungroup()


  

#------------------------------------------------#
####        Waterbird Trend Modelling         ####
#------------------------------------------------#

wbd <- twd %>% 
  mutate(grouping2 = ifelse(grouping2 == "Alcids" | grouping2 == "Black Scoter" | 
                              grouping2 == "Common Eider" | grouping2 == "Common Loon" |
                              grouping2 == "Cormorants" | grouping2 == "DC Cormorant" |
                              grouping2 == "Grebes" | grouping2 == "LT Duck" |
                              grouping2 == "Loons" | grouping2 == "Northern Gannet" |
                              grouping2 == "RB Merganser" | grouping2 == "RT Loon" |
                              grouping2 == "Surf Scoter" | grouping2 == "Scoters" |
                              grouping2 == "WW Scoter", "Waterbirds", grouping2))


### Run model selection steps for all waterbirds
distribution_test(wbd, group = "Waterbirds")
model_tests(wbd, group = "Waterbirds", distrib = "nbinom+zi") ## Temporal Autocorrelation****
dredge_count_models(wbd, group = "Waterbirds", distrib = "nbinom+zi") 
summary(waterbirds_topmod)




#------------------------------------------------#
####         Species Trend Modelling          ####
#------------------------------------------------#

source("scripts/sea_watch_functions.R")

## See list of groupings for models
twd %>% 
  distinct(grouping2) %>% 
  print(n = 29)

## All species should have quadratic doy, see plot
twd %>% 
  filter(species == "COEI") %>% 
  ggplot(aes(x = year, y = count)) +
  geom_point()

### Run model selection steps for each grouping
## Common Eider
distribution_test(twd, group = "Common Eider")
model_tests(twd, group = "Common Eider", distrib = "nbinom+zi")
dredge_count_models(twd, group = "Common Eider", distrib = "nbinom+zi")
summary(commoneider_topmod)

## White-winged Scoter
distribution_test(twd, group = "WW Scoter")
model_tests(twd, group = "WW Scoter", distrib = "nbinom")
dredge_count_models(twd, group = "WW Scoter", distrib = "nbinom")
summary(wwscoter_topmod)

## Black Scoter
distribution_test(twd, group = "Black Scoter")
model_tests(twd, group = "Black Scoter", distrib = "nbinom")
dredge_count_models(twd, group = "Black Scoter", distrib = "nbinom")
summary(blackscoter_topmod)

## Surf Scoter
distribution_test(twd, group = "Surf Scoter")
model_tests(twd, group = "Surf Scoter", distrib = "nbinom")
dredge_count_models(twd, group = "Surf Scoter", distrib = "nbinom")
summary(surfscoter_topmod)

## Northern Gannet
distribution_test(twd, group = "Northern Gannet")
model_tests(twd, group = "Northern Gannet", distrib = "nbinom+zi")
dredge_count_models(twd, group = "Northern Gannet", distrib = "nbinom+zi")
summary(northerngannet_topmod)

## Double-crested Cormorant
distribution_test(twd, group = "DC Cormorant")
model_tests(twd, group = "DC Cormorant", distrib = "nbinom") #**Autocorrelated
dredge_count_models(twd, group = "DC Cormorant", distrib = "nbinom")
summary(dccormorant_topmod)

## Common Loon
distribution_test(twd, group = "Common Loon")
model_tests(twd, group = "Common Loon", distrib = "nbinom")
dredge_count_models(twd, group = "Common Loon", distrib = "nbinom")
summary(commonloon_topmod)

## Red-throated Loon
distribution_test(twd, group = "RT Loon")
model_tests(twd, group = "RT Loon", distrib = "nbinom")
dredge_count_models(twd, group = "RT Loon", distrib = "nbinom")
summary(rtloon_topmod)

## Long-tailed Duck
distribution_test(twd, group = "LT Duck")
model_tests(twd, group = "LT Duck", distrib = "nbinom")
dredge_count_models(twd, group = "LT Duck", distrib = "nbinom")
summary(ltduck_topmod)

## Red-breasted Merganser
distribution_test(twd, group = "RB Merganser")
model_tests(twd, group = "RB Merganser", distrib = "nbinom")
dredge_count_models(twd, group = "RB Merganser", distrib = "nbinom")
summary(rbmerganser_topmod)




#------------------------------------------------#
####          Model Visualization             ####
#------------------------------------------------#

source("scripts/sea_watch_functions.R")

### TRENDS

summary(commoneider_topmod)
summary(wwscoter_topmod)
summary(blackscoter_topmod)
summary(surfscoter_topmod) #*
summary(northerngannet_topmod)
summary(dccormorant_topmod) #*
summary(commonloon_topmod)
summary(rtloon_topmod) #*
summary(ltduck_topmod) #*
summary(rbmerganser_topmod) #*


## Create a list of models to enter into the ggpredict looping function
modlist.y <- list(surfscoter_topmod, dccormorant_topmod, rtloon_topmod, 
                  ltduck_topmod, rbmerganser_topmod)


## List of model names for the looping function
spnames.y = c("Surf Scoter", "Double-crested Cormorant", "Red-throated Loon",
              "Long-tailed Duck", "Red-breasted Merganser")


## List the parameter input for the looping function
param.y <- rep(list("year"), 5)


#t <- ggpredict(rbmerganser_nowinddr, terms = c("year [all]", "doy [all]"))


## Looping the models through a custom ggpredict function to return a single df
modelpreds.y <- pmap_dfr(list(modlist.y, spnames.y, param.y), model_predictions)


## Plot the trends for each species using facet wrap 
modelpreds.y %>% 
  ggplot(aes(x = param, y = predicted, group = group)) + 
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.3) + 
  facet_wrap(vars(group), scales = "free_y") +
  theme_bw() +
  labs(x = "", y = "Mean individuals / day") +
  theme(panel.border = element_rect(linewidth = 1),
        panel.grid = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        strip.text = element_text(size = 10)
  )


## View model summaries
emmeans









