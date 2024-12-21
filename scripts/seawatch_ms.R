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

swdat <- tibble(read.csv("data/sea_watch_cleaned_data_20241216.csv")) %>% 
  select(-c(time.of.obs, start.time.2, wind.dir.speed, visibility)) %>% 
  rename(observer = principle.observer,
         count = day.total) %>% 
  mutate(date = as.Date(date),
         start.datetime = paste(date, start.time),
         start.datetime = gsub("\\d*\\-\\d*\\-\\d*\\s\\NA", "NA", start.datetime),
         start.datetime = ifelse(start.datetime == "NA", NA, start.datetime),
         start.datetime = ymd_hm(start.datetime),
         observer = ifelse(observer == "SB/ZK", "SB/RZK", observer),
         observer = ifelse(observer == "RZK/SB", "SB/RZK", observer),
         observer = ifelse(observer == "STB/RZK", "SB/RZK", observer),
         observer = ifelse(observer == "ZK", "RZK", observer)) %>% 
  filter(year != 2014 & year != 2015)




#------------------------------------------------#
####              Summary Stats               ####
#------------------------------------------------#

## Number of days surveyed from 2016 - 2024 -- 655
swdat %>% 
  mutate(filt = ifelse(count == 0 & obs.hours == 0, "rm", "keep")) %>% 
  filter(filt == "keep") %>% 
  distinct(date) %>% 
  nrow()
  
# swdat %>% 
#   group_by(year, date) %>% 
#   distinct(year, date) %>% 
#   group_by(year)


## Total hours surveyed -- 2,563 hours
swdat %>% 
  select(date, obs.hours) %>% 
  distinct() %>% 
  summarise(time = sum(obs.hours))


## Number of observers -- 29 levels but 19 human observers
swdat %>% 
  filter(!is.na(observer)) %>% 
  distinct(observer) %>% 
  arrange(observer) %>% 
  print(n = nrow(.))


## Total hours by season
hps <- swdat %>% 
  select(year, date, obs.hours) %>% 
  distinct() %>% 
  group_by(year) %>% 
  summarise(total.effort = sum(obs.hours))
hps


## Mean hours of effort per day for each season
swdat %>% 
  select(year, date, obs.hours) %>% 
  distinct() %>% 
  group_by(year) %>% 
  summarise(mean.daily.effort = mean(obs.hours))

swdat %>% 
  select(year, date, obs.hours) %>% 
  distinct() %>% 
  group_by(year) %>% 
  summarise(mean.daily.effort = mean(obs.hours)) %>% 
  summarise(mean = mean(mean.daily.effort))


## Total individual birds recorded -- 490,208 individuals
swdat %>% 
  summarise(count = sum(count))


## Total individual birds and birds per hour of effort by year
swdat %>% 
  group_by(year) %>% 
  summarise(total.indiv = sum(count)) %>% 
  left_join(., hps, by = "year") %>% 
  mutate(bph = total.indiv/total.effort)#############################################


## Total individual birds by observer
# swdat %>% 
#   group_by(observer) %>% 
#   summarise(total.effort = sum(obs.hours),
#             total.indiv = sum(count)) %>% 
#   mutate(bph = total.indiv/total.effort) %>% 
#   arrange(-bph)


## Totals by species
# test <- swdat %>% 
#   group_by(species) %>% 
#   summarise(total.effort = 1879.1500,
#             total.indiv = sum(day.total)) %>% 
#   mutate(bph = total.indiv/total.effort) %>% 
#   arrange(-bph)


## Most prominent wind direction
swdat %>% 
  filter(!is.na(wind.direction)) %>% 
  group_by(date) %>% 
  distinct(wind.direction) %>% 
  ungroup() %>% 
  group_by(wind.direction) %>% 
  tally() %>% 
  arrange(-n)


## Most common visibility score
swdat %>% 
  filter(!is.na(visibility.score)) %>% 
  group_by(date) %>% 
  distinct(visibility.score) %>% 
  ungroup() %>% 
  group_by(visibility.score) %>% 
  tally() %>% 
  arrange(-n)




#------------------------------------------------#
####          Total Waterbird Trends          ####
#------------------------------------------------#


sp.groups <- read.csv("outputs/species_groupings.csv") 

waterbirds <- sp.groups %>% 
  as_tibble() %>% 
  filter(grouping %in% c("Other ducks and geese", "Alcids", "Larids",
                       "Scoters", "Common Eider", "Loons", "Cormorants",
                       "Grebes", "Northern Gannet")) %>% 
  select(species)


twd <- swdat %>% 
  filter(species %in% waterbirds$species) %>% 
  left_join(., sp.groups, by = "species") %>% 
  select(species, grouping, date:day.total, wind.direction:observer) %>% 
  #filter(grouping == "Scoters") %>% 
  group_by(date, year, start.time, total.obs.mins, obs.hours, wind.direction,
           visibility.score, observer) %>% 
  summarise(count = sum(day.total)) %>% 
  rename(wind.dir = wind.direction,
         visibility = visibility.score) %>% 
  mutate(filt = ifelse(count == 0 & obs.hours == 0, "rm", "keep"),
         midnight = as_datetime("2000-01-01 00:00:00"),
         start.time = str_replace(start.time, "^(\\d)[:punct:]", "0\\1\\:"),
         start.time = ifelse(!is.na(start.time), paste0(start.time, ":00"), start.time), 
         start.time = as_datetime(paste0("2000-01-01", start.time)),
         tsm = as.numeric(difftime(start.time, midnight, units = "hours"))) %>% 
  filter(filt == "keep") %>% 
  select(-filt) #%>% 
  #filter(count < 5000)
  # swdat %>% 
  # filter(species %in% waterbirds$species) %>% 
  # select(species:day.total, wind.direction:observer) %>% 
  # group_by(date, year, start.time, total.obs.mins, obs.hours, wind.direction,
  #          visibility.score, observer) %>% 
  # summarise(count = sum(day.total)) %>% 
  # rename(wind.dir = wind.direction,
  #        visibility = visibility.score) %>% 
  # mutate(filt = ifelse(count == 0 & obs.hours == 0, "rm", "keep")) %>% 
  # filter(filt == "keep", ) %>% 
  # select(-filt)



scot.null <- glmer.nb(count ~ 1 + (1 | observer), data = twd)
scot.year <- glmer.nb(count ~ scale(year) + (1 | observer), data = twd)
scot.yt <- glmer.nb(count ~ scale(year) + tsm + (1 | observer), 
                    data = twd)
scot.yh <- glmer.nb(count ~ scale(year) + obs.hours + (1 | observer),
                 data = twd)
scot.yw <- glmer.nb(count ~ scale(year) + wind.dir + (1 | observer), 
                    data = twd)
scot.yv <- glmer.nb(count ~ scale(year) + visibility + (1 | observer), 
                    data = twd)
scot.global.c <- glmer.nb(count ~ scale(year) + tsm + obs.hours + visibility + (1 | observer), 
                          data = twd)
scot.global <- glmer.nb(count ~ scale(year) + tsm + obs.hours + wind.dir + visibility + (1 | observer), 
                        data = twd)


sim.mod = simulateResiduals(mod.1, re.form = NULL)
plot(sim.mod)
testDispersion(sim.mod)
testZeroInflation(sim.mod)






scoterdat <- swdat %>% 
  filter(species %in% waterbirds$species) %>% 
  left_join(., sp.groups, by = "species") %>% 
  select(species, grouping, date:day.total, wind.direction:observer) %>% 
  filter(grouping == "Scoters") %>% 
  group_by(date, year, start.time, total.obs.mins, obs.hours, wind.direction,
           visibility.score, observer) %>% 
  summarise(count = sum(day.total)) %>% 
  rename(wind.dir = wind.direction,
         visibility = visibility.score) %>% 
  mutate(filt = ifelse(count == 0 & obs.hours == 0, "rm", "keep"),
         midnight = as_datetime("2000-01-01 00:00:00"),
         start.time = str_replace(start.time, "^(\\d)[:punct:]", "0\\1\\:"),
         start.time = ifelse(!is.na(start.time), paste0(start.time, ":00"), start.time), 
         start.time = as_datetime(paste0("2000-01-01", start.time)),
         tsm = as.numeric(difftime(start.time, midnight, units = "hours"))) %>% 
  filter(filt == "keep") %>% 
  select(-filt) %>% 
  filter(count < 5000) ## Removing two super high count days for now


# scot.null <- glmer(count ~ 1 + (1 | observer), 
#                    data = scoterdat, family = "poisson")
# scot.year <- glmer(count ~ scale(year) + (1 | observer), 
#                    data = scoterdat, family = "poisson")
# scot.yt <- glmer(count ~ scale(year) + tsm + (1 | observer), 
#                  data = scoterdat, family = "poisson")
# scot.yh <- glmer(count ~ scale(year) + obs.hours + (1 | observer), 
#                  data = scoterdat, family = "poisson")
# #scot.yw <- glmer(count ~ scale(year) + wind.dir + (1 | observer), data = scoterdat, family = "poisson")
# scot.yv <- glmer(count ~ scale(year) + visibility + (1 | observer), 
#                  data = scoterdat, family = "poisson")
# scot.global <- glmer(count ~ scale(year) + tsm + obs.hours + visibility + (1 | observer), 
#                      data = scoterdat, family = "poisson")
# 
# ## Define list of models
# models <- list(scot.null, scot.year, scot.yt, scot.yh, scot.yv, scot.global)
# 
# ## Specify model names
# mod.names <- c('null', 'year', 'year + start.time', 'year + hours', 
#                'year + visibility', 'global')
# 
# ## Calculate AIC of each model
# aictab(cand.set = models, modnames = mod.names)



# scot.null <- glmer.nb(count ~ 1 + (1 | observer), data = scoterdat)
scot.year <- glmer.nb(count ~ scale(year) + (1 | observer), data = scoterdat)
scot.yt <- glmer.nb(count ~ scale(year) + tsm + (1 | observer), 
                    data = scoterdat)
# scot.yh <- glmer.nb(count ~ scale(year) + obs.hours + (1 | observer),
#                  data = scoterdat)
# scot.yw <- glmer.nb(count ~ scale(year) + wind.dir + (1 | observer), 
#                     data = scoterdat)
scot.yv <- glmer.nb(count ~ scale(year) + visibility + (1 | observer), 
                    data = scoterdat)
scot.global.c <- glmer.nb(count ~ scale(year) + tsm + obs.hours + visibility + (1 | observer), 
                          data = scoterdat)
# scot.global <- glmer.nb(count ~ scale(year) + tsm + obs.hours + wind.dir + visibility + (1 | observer), 
#                         data = scoterdat)


## Define list of models
models <- list(scot.year, scot.yt, scot.yv, scot.global.c)

## Specify model names
mod.names <- c('year', 'year + start.time', 
               'year + visibility', 'global')

## Calculate AIC of each model
aictab(cand.set = models, modnames = mod.names)


summary(scot.global.c)


sim.mod.sc = simulateResiduals(scot.global.nb, re.form = NULL)
plot(sim.mod.sc)
testDispersion(sim.mod.sc)
testZeroInflation(sim.mod.sc)




