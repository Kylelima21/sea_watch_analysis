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
  # rename(wind.dir = wind.direction,
  #        visibility = visibility.score)




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
         grouping2 = ifelse(species == "RBME", "RB Merganser", grouping2)) %>% 
  select(species, grouping, grouping2, date:count, wind.speed, wind.dir, pressure, 
         start.time, observer) %>% 
  mutate(filt = ifelse(count == 0 & obs.hours == 0, "rm", "keep"),
         start.time = str_replace(start.time, "^(\\d)[:punct:]", "0\\1\\:"),
         start.time = ifelse(!is.na(start.time), paste0(start.time, ":00"), start.time),
         start.time = as_datetime(paste0("2000-01-01", start.time)),
         midnight = as_datetime("2000-01-01 00:00:00"),
         tsm = as.numeric(difftime(start.time, midnight, units = "hours")),
         doy = yday(date)) %>% 
  filter(filt == "keep") %>% 
  select(-c(filt, midnight)) %>% 
  ungroup()




#------------------------------------------------#
####         Species Trend Modelling          ####
#------------------------------------------------#

source("scripts/sea_watch_functions.R")

twd %>% 
  distinct(grouping2) %>% 
  print(n = 25)


distribution_test(group = "Common Eider")
model_tests(group = "Common Eider", distrib = "nbinom+zi")
run_count_models(group = "Common Eider", distrib = "nbinom+zi")
summary(commoneider_global)

distribution_test(group = "Scoters")
model_tests(group = "Scoters", distrib = "nbinom")
run_count_models(group = "Scoters", distrib = "nbinom")
summary(scoters_yeardwind)

distribution_test(group = "Alcids")
model_tests(group = "Alcids", distrib = "nbinom")
run_count_models(group = "Alcids", distrib = "nbinom")
summary(alcids_nowinddr)

distribution_test(group = "Northern Gannet")
model_tests(group = "Northern Gannet", distrib = "nbinom+zi")
run_count_models(group = "Northern Gannet", distrib = "nbinom+zi")
summary(northerngannet_nowinddr)

distribution_test(group = "DC Cormorant")
model_tests(group = "DC Cormorant", distrib = "nbinom+zi")
run_count_models(group = "DC Cormorant", distrib = "nbinom+zi")
summary(dccormorant_yeardoy)

distribution_test(group = "Common Loon")
model_tests(group = "Common Loon", distrib = "nbinom")
run_count_models(group = "Common Loon", distrib = "nbinom")
summary(commonloon_nowindsp)

distribution_test(group = "LT Duck")
model_tests(group = "LT Duck", distrib = "nbinom")
run_count_models(group = "LT Duck", distrib = "nbinom")
summary(ltduck_nowindsp)

distribution_test(group = "RB Merganser")
model_tests(group = "RB Merganser", distrib = "nbinom")
run_count_models(group = "RB Merganser", distrib = "nbinom")
summary(rbmerganser_yeardwind)










#------------------------------------------------#
####            Modelling Tests               ####
#------------------------------------------------#

# # ## Define waterbirds
# # waterbirds <- sp.groups %>% 
# #   as_tibble() %>% 
# #   filter(grouping %in% c("Other ducks and geese", "Alcids", "Larids",
# #                          "Scoters", "Common Eider", "Loons", "Cormorants",
# #                          "Grebes", "Northern Gannet", "Waterbird sp.")) %>% 
# #   select(species)
# 
# 
# ## Fine tune data for model input
# coeidt <- twd %>% 
#   filter(grouping == "Common Eider") %>% 
#   select(-species) %>% 
#   group_by(date, year, tsm, total.obs.mins, obs.hours, wind.dir,
#            visibility, observer) %>% 
#   summarise(count = sum(count)) %>% 
#   na.omit(.)
# 
# 
# ## Create distribution model comparison to determine best fit
# ## Testing Poisson, Negative Binomial, and a Zero-inflated Negative Binomial
# scot.p <- glmmTMB(count ~ scale(year) + scale(tsm) + scale(obs.hours) +
#                           wind.dir + visibility + (1 | observer), data = coeidt,
#                           family = poisson)
# scot.b <- glmmTMB(count ~ scale(year) + scale(tsm) + scale(obs.hours) +
#                           wind.dir + visibility + (1 | observer), data = coeidt,
#                           family = nbinom2)
# scot.zi <- glmmTMB(count ~ scale(year) + scale(tsm) + scale(obs.hours) +
#                            wind.dir + visibility + (1 | observer), data = coeidt,
#                            family = nbinom2, ziformula = ~1)
# 
# ## Define list of models
# models <- list(scot.p, scot.b, scot.zi)
# 
# ## Specify model names
# mod.names <- c('poisson', 'nb', 'nb+zi')
# 
# ## Calculate AIC of each model
# aictab(cand.set = models, modnames = mod.names)
# 
# ## Zero-inflated Negative Binomial model fits best
# ## Test for overdispersion and zero-inflation
# sim.mod = simulateResiduals(scot.zi)
# plot(sim.mod)
# testDispersion(sim.mod)
# testZeroInflation(sim.mod)
# 
# ## Not overdispersed or zero-inflated
# ## Also need to test for temporal autocorrelation
# ## Recalculate residuals because we have many obs/time interval
# sim.mod2 = recalculateResiduals(sim.mod, group = unique(coeidt$year))
# testTemporalAutocorrelation(sim.mod2, time = unique(coeidt$year), plot = TRUE)
# 
# ## Not auto-correlated
# 
# 
# ## Now we can build the models for AICc model comparison
# ## All models will use the zero-inflated negative binomial model
# scot.null <- glmmTMB(count ~ (1 | observer), data = coeidt, family = nbinom2, 
#                      ziformula = ~1)
# scot.year <- glmmTMB(count ~ scale(year) + (1 | observer), data = coeidt, 
#                      family = nbinom2, ziformula = ~1)
# scot.yt <- glmmTMB(count ~ scale(year) + scale(tsm) + (1 | observer),
#                    data = coeidt, family = nbinom2, ziformula = ~1)
# scot.yh <- glmmTMB(count ~ scale(year) + scale(obs.hours) + (1 | observer),
#                    data = coeidt, family = nbinom2, ziformula = ~1)
# scot.yw <- glmmTMB(count ~ scale(year) + wind.dir + (1 | observer), 
#                    data = coeidt, family = nbinom2, ziformula = ~1)
# scot.yv <- glmmTMB(count ~ scale(year) + visibility + (1 | observer), 
#                    data = coeidt, family = nbinom2, ziformula = ~1)
# scot.most <- glmmTMB(count ~ scale(year) + scale(tsm) + scale(obs.hours) + 
#                      visibility + (1 | observer), data = coeidt, 
#                      family = nbinom2, ziformula = ~1)
# scot.most2 <- glmmTMB(count ~ scale(year) + scale(tsm) + scale(obs.hours) + 
#                       wind.dir + (1 | observer), data = coeidt, 
#                       family = nbinom2, ziformula = ~1)
# scot.global <- glmmTMB(count ~ scale(year) + scale(tsm) + scale(obs.hours) +
#                        wind.dir + visibility + (1 | observer), data = coeidt,
#                        family = nbinom2, ziformula = ~1)
# 
# ## Define list of models
# models <- list(scot.null, scot.year, scot.yt, scot.yh, scot.yw, scot.yv, 
#                scot.most, scot.most2, scot.global)
# 
# ## Specify model names
# mod.names <- c('null', 'year', 'year + start time', 'year + hours', 'year + wind',
#                'year + visibility', 'all - wind', 'all - visibility', 'global')
# 
# ## Calculate AIC of each model
# aictab(cand.set = models, modnames = mod.names)
# 
# 
# # sim.mod = simulateResiduals(scot.global)
# # plot(sim.mod)
# # testDispersion(sim.mod)
# # testZeroInflation(sim.mod)
# 
# summary(scot.global)




# swdat %>%
#   filter(species %in% waterbirds$species) %>%
#   select(species:day.total, wind.direction:observer) %>%
#   group_by(date, year, start.time, total.obs.mins, obs.hours, wind.direction,
#            visibility.score, observer) %>%
#   summarise(count = sum(day.total)) %>%
#   rename(wind.dir = wind.direction,
#          visibility = visibility.score) %>%
#   mutate(filt = ifelse(count == 0 & obs.hours == 0, "rm", "keep")) %>%
#   filter(filt == "keep", ) %>%
#   select(-filt)



# scot.null <- glmmTMB(count ~ (1 | observer), data = twd)
# #scot.null <- insight::null_model(scot.most)
# scot.year <- glmmTMB(count ~ scale(year) + (1 | observer), data = twd)
# scot.yt <- glmer.nb(count ~ scale(year) + scale(tsm) + (1 | observer), 
#                     data = twd)
# scot.yh <- glmer.nb(count ~ scale(year) + scale(obs.hours) + (1 | observer),
#                  data = twd)
# scot.yw <- glmer.nb(count ~ scale(year) + wind.dir + (1 | observer), 
#                     data = twd)
# scot.yv <- glmer.nb(count ~ scale(year) + visibility + (1 | observer), 
#                     data = twd)
# scot.most <- glmer.nb(count ~ scale(year) + scale(tsm) + scale(obs.hours) + 
#                             visibility + (1 | observer), data = twd)
# scot.most2 <- glmer.nb(count ~ scale(year) + scale(tsm) + scale(obs.hours) + 
#                         wind.dir + (1 | observer), data = twd)
# # scot.global <- glmer.nb(count ~ scale(year) + scale(tsm) + scale(obs.hours) + 
# #                           wind.dir + visibility + (1 | observer), data = twd)
# 
# 
# 
# ## Define list of models
# models <- list(scot.null, scot.yt, scot.yh, scot.yw, scot.yv, scot.most, scot.most2)
# 
# ## Specify model names
# mod.names <- c('null', 'year + start time', 'year + hours', 'year + wind',
#                'year + visibility', 'all - wind', 'all - visibility')
# 
# ## Calculate AIC of each model
# aictab(cand.set = models, modnames = mod.names)
# 
# 
# sim.mod = simulateResiduals(scot.most, re.form = NULL)
# plot(sim.mod)
# testDispersion(sim.mod)
# testZeroInflation(sim.mod)




# scot.null <- glmmTMB(count ~ (1 | observer), data = twd, family = poisson)
# scot.year <- glmmTMB(count ~ scale(year) + (1 | observer), data = twd, family = poisson)
# scot.yt <- glmmTMB(count ~ scale(year) + scale(tsm) + (1 | observer),
#                    data = twd, family = poisson)
# scot.yh <- glmmTMB(count ~ scale(year) + scale(obs.hours) + (1 | observer),
#                     data = twd, family = poisson)
# scot.yw <- glmmTMB(count ~ scale(year) + wind.dir + (1 | observer), 
#                    data = twd, family = poisson)
# scot.yv <- glmmTMB(count ~ scale(year) + visibility + (1 | observer), 
#                    data = twd, family = poisson)
# scot.most <- glmmTMB(count ~ scale(year) + scale(tsm) + scale(obs.hours) + 
#                      visibility + (1 | observer), data = twd, family = poisson)
# scot.most2 <- glmmTMB(count ~ scale(year) + scale(tsm) + scale(obs.hours) + 
#                       wind.dir + (1 | observer), data = twd, family = poisson)
# scot.global <- glmmTMB(count ~ scale(year) + scale(tsm) + scale(obs.hours) +
#                         wind.dir + visibility + (1 | observer), data = twd,
#                         family = poisson)
# scot.global.nb1 <- update(scot.global, family = nbinom1)
# scot.global.nb2 <- update(scot.global, family = nbinom2)
# 
# 
# ## Define list of models
# models <- list(scot.null, scot.year, scot.yt, scot.yh, scot.yw, scot.yv, 
#                scot.most, scot.most2, scot.global, scot.global.nb1, 
#                scot.global.nb2)
# 
# ## Specify model names
# mod.names <- c('null', 'year', 'year + start time', 'year + hours', 'year + wind',
#                'year + visibility', 'all - wind', 'all - visibility', 'global',
#                'global.nb1', 'global.nb2')
# 
# ## Calculate AIC of each model
# aictab(cand.set = models, modnames = mod.names)
# 
# 
# 
# sim.mod = simulateResiduals(scot.global.nb2, re.form = NULL)
# plot(sim.mod)
# testDispersion(sim.mod)
# testZeroInflation(sim.mod)
# 
# 
# summary(scot.global.nb2)




