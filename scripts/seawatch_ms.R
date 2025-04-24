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
library(marginaleffects)

## Source custom functions
source("scripts/sea_watch_functions.R")




#------------------------------------------------#
####     Read and Clean Manipulated Data      ####
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
####           Add in Weather Data            ####
#------------------------------------------------#

## Read in weather data from the Schoodic D2258 weather station
schweather <- read.csv("data/schoodic_weather_data_D2258_20250417.csv") %>% 
  filter(altimeter_set_1 != "Pascals") %>% 
  as_tibble() %>% 
  rename_with(., ~str_remove(., "_set_1d"), .cols = everything()) %>% 
  rename_with(., ~str_remove(., "_set_1"), .cols = everything()) %>% 
  rename_with(., tolower, .cols = everything()) %>% 
  rename_with(., ~str_replace_all(., "_", "."), .cols = everything()) %>% 
  select(date.time, wind.speed, wind.direction, pressure) %>% 
  mutate(datetime = as_datetime(date.time, tz = "America/New_York"),
         date = date(datetime),
         wind.speed = as.numeric(wind.speed),
         wind.direction = as.numeric(wind.direction),
         pressure = round(as.numeric(pressure), digits = 3))


## Get a list of the dates of survey from the seawatch data
dates <- swdat %>% 
  select(date, total.obs.mins) %>% 
  distinct() %>% 
  filter(total.obs.mins > 0) %>% 
  select(date)


## Run a pmap function for each date and time combination, looping it through
## the avg_weather() function and compiling all into one long dataframe
avgweatherdat <- map_dfr(dates$date, avg_windp)


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


## Average days surveyed per year 2016 - 2024 -- 73
swdat2 %>% 
  mutate(filt = ifelse(count == 0 & obs.hours == 0, "rm", "keep")) %>% 
  filter(filt == "keep") %>% 
  select(date, year) %>% 
  distinct() %>% 
  mutate(daycount = 1) %>% 
  group_by(year) %>% 
  summarise(days = sum(daycount)) %>% 
  summarise(mean = mean(days))


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
####        Format Data for Modelling         ####
#------------------------------------------------#

## Read in species groupings file
sp.groups <- read.csv("outputs/species_groupings2.csv") 


## Format data, add in the species groupings, and clean covariate columns
twd <- swdat2 %>% 
  left_join(., sp.groups, by = "species") %>% 
  mutate(grouping2 = grouping,
         grouping2 = ifelse(species == "DCCO", "DC Cormorant", grouping2),
         grouping2 = ifelse(species == "COEI", "Common Eider", grouping2),
         grouping2 = ifelse(species == "COLO", "Common Loon", grouping2),
         grouping2 = ifelse(species == "LTDU", "LT Duck", grouping2),
         grouping2 = ifelse(species == "RBME", "RB Merganser", grouping2),
         grouping2 = ifelse(species == "WWSC", "WW Scoter", grouping2),
         grouping2 = ifelse(species == "BLSC", "Black Scoter", grouping2),
         grouping2 = ifelse(species == "SUSC", "Surf Scoter", grouping2),
         grouping2 = ifelse(species == "RTLO", "RT Loon", grouping2)) %>% 
  select(species, grouping, grouping2, grouping3, date:count, wind.speed, wind.dir, pressure, 
         start.time, observer) %>% 
  mutate(filt = ifelse(count == 0 & obs.hours == 0, "rm", "keep"),
         start.time = str_replace(start.time, "^(\\d)[:punct:]", "0\\1\\:"),
         start.time = ifelse(!is.na(start.time), paste0(start.time, ":00"), start.time),
         start.time = ifelse(!is.na(start.time), paste0("2000-01-01 ", start.time),
                              start.time),
         start.time = as_datetime(start.time),
         midnight = as_datetime("2000-01-01 00:00:00"),
         tsm = as.numeric(difftime(start.time, midnight, units = "hours")),
         doy = yday(date),
         wind.dir.cos = cos(wind.dir),
         wind.dir.sin = sin(wind.dir)) %>% 
  filter(filt == "keep") %>% 
  select(-c(filt, midnight)) %>% 
  ungroup()


  

#------------------------------------------------#
####         Seabird Trend Modelling          ####
#------------------------------------------------#
             
wbd <- twd %>% 
  filter(grouping3 == "Seabirds") %>% 
  filter(species != "Scoter sp." & 
           species != "Cormorant sp." &
           species != "BLSC/SUSC" &
           species != "Loon sp." &
           species != "Alcid sp." &
           species != "Shearwater sp." &
           species != "Grebe sp." &
           species != "TBMU/COMU")
  # mutate(grouping2 = ifelse(grouping2 == "Alcids" | grouping2 == "Black Scoter" |
  #                             grouping2 == "Common Eider" | grouping2 == "Common Loon" |
  #                             grouping2 == "Cormorants" | grouping2 == "DC Cormorant" |
  #                             grouping2 == "Grebes" | grouping2 == "LT Duck" |
  #                             grouping2 == "Loons" | grouping2 == "Northern Gannet" |
  #                             grouping2 == "RB Merganser" | grouping2 == "RT Loon" |
  #                             grouping2 == "Surf Scoter" | grouping2 == "Scoters" |
  #                             grouping2 == "WW Scoter", "Waterbirds", grouping2))
  
spyears <- wbd %>% 
  group_by(species, year) %>% 
  summarise(count = sum(count)) %>% 
  filter(count > 0) %>% 
  mutate(count = 1) %>% 
  group_by(species) %>% 
  summarise(years.detected = sum(count)) %>% 
  arrange(-years.detected)


tsplist <- spyears %>% 
  filter(years.detected == 9) %>% 
  select(species) %>% 
  as.list()


seabird.dat <- wbd %>% 
  filter(species %in% tsplist$species) %>% 
  select(-c(grouping, grouping2, grouping3))



## Summary stats for results
seabird.dat %>% 
  group_by(species) %>% 
  summarise(count = sum(count)) %>% 
  arrange(-count)

seabird.dat %>% 
  group_by(species) %>% 
  summarise(count = sum(count)) %>% 
  arrange(-count) %>% 
  summarise(sum = sum(count))

seabird.dat %>% 
  group_by(species, year) %>% 
  summarise(count = sum(count)) %>%
  arrange(-count) %>% 
  pivot_wider(names_from = year, values_from = count)

seabird.dat %>% 
  group_by(species, date) %>% 
  summarise(count = sum(count)) %>% 
  filter(count > 0) %>% 
  mutate(dd = 1) %>% 
  group_by(species) %>% 
  summarise(detection.days = sum(dd)) %>% 
  arrange(-detection.days)


## Finalizing data for models by removing NAs
sbmd <- seabird.dat %>% 
  group_by(date, year, doy, tsm, species, total.obs.mins, obs.hours, wind.speed, 
           wind.dir.cos, wind.dir.sin, pressure, observer) %>% 
  summarise(count = sum(count)) %>% 
  na.omit(.)


### Run model selection steps for all seabirds
distribution_test(sbmd, group = "Seabirds")
model_tests(sbmd, group = "Seabirds", distrib = "nbinom")
dredge_count_models(sbmd, group = "Seabirds", distrib = "nbinom") 
summary(seabirds_topmod)


## Check model residuals
sim.modallt = simulateResiduals(seabirds_topmod, plot = T)
testOutliers(sim.modallt, type = "bootstrap")


## Get prediction data for plotting
# predict.allt <- ggpredict(seabirds_topmod, terms = "doy [all]") %>% 
#   rename(year = x)


## Plot
# predict.allt %>% 
#   ggplot(aes(year, predicted)) + 
#   geom_line() +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.3) +
#   labs(y = "Count", x = "Year") +
#   theme_bw() +
#   theme(panel.grid = element_blank(),
#         axis.title.x = element_text(margin = unit(c(5, 0, 1, 0), "mm")),
#         axis.title.y = element_text(margin = unit(c(0, 5, 0, 1), "mm")))


## Export plot
# ggsave("outputs/forpub/seabird_trends.png", height = 5.7, width = 7,
#        units = "in", dpi = 1200)




#------------------------------------------------#
####         Species Trend Modelling          ####
#------------------------------------------------#

## All species should have quadratic doy, see plot
sbmd %>% 
  filter(species == "COEI") %>% 
  ggplot(aes(x = doy, y = count)) +
  geom_point()

source("scripts/sea_watch_functions.R")
### Run model selection steps for each grouping
## Common Eider
distribution_test(sbmd, group = "COEI")
model_tests(sbmd, group = "COEI", distrib = "nbinom+zi")
dredge_count_models(sbmd, group = "COEI", distrib = "nbinom+zi")
test_final_mod(coei_topmod)
summary(coei_topmod)

## Common Loon
distribution_test(sbmd, group = "COLO", autocorrelated = T)
model_tests(sbmd, group = "COLO", distrib = "nbinom", autocorrelated = T)
dredge_count_models(sbmd, group = "COLO", distrib = "nbinom", autocorrelated = T)
test_final_mod(colo_topmod)
summary(colo_topmod)

## Double-crested Cormorant
distribution_test(sbmd, group = "DCCO", autocorrelated = T) #*Zero-inflated doesn't converge, but just use nbinom
model_tests(sbmd, group = "DCCO", distrib = "nbinom", autocorrelated = T) #**Autocorrelated
dredge_count_models(sbmd, group = "DCCO", distrib = "nbinom", autocorrelated = T)
test_final_mod(dcco_topmod)
summary(dcco_topmod)

## Northern Gannet
distribution_test(sbmd, group = "NOGA")
model_tests(sbmd, group = "NOGA", distrib = "nbinom+zi")
dredge_count_models(sbmd, group = "NOGA", distrib = "nbinom+zi")
test_final_mod(noga_topmod)
summary(noga_topmod)

## Surf Scoter
distribution_test(sbmd, group = "SUSC")
model_tests(sbmd, group = "SUSC", distrib = "nbinom")
dredge_count_models(sbmd, group = "SUSC", distrib = "nbinom")
test_final_mod(susc_topmod)
summary(susc_topmod)

## White-winged Scoter
distribution_test(sbmd, group = "WWSC")
model_tests(sbmd, group = "WWSC", distrib = "nbinom")
dredge_count_models(sbmd, group = "WWSC", distrib = "nbinom")
test_final_mod(wwsc_topmod, outlieryes = T)
summary(wwsc_topmod)

## Black Scoter
distribution_test(sbmd, group = "BLSC")
model_tests(sbmd, group = "BLSC", distrib = "nbinom")
dredge_count_models(sbmd, group = "BLSC", distrib = "nbinom")
test_final_mod(blsc_topmod)
summary(blsc_topmod)

## Great Cormorant
distribution_test(sbmd, group = "GRCO")
model_tests(sbmd, group = "GRCO", distrib = "nbinom")
dredge_count_models(sbmd, group = "GRCO", distrib = "nbinom")
test_final_mod(grco_topmod)
summary(grco_topmod)

## Red-breasted Merganser
distribution_test(sbmd, group = "RBME")
model_tests(sbmd, group = "RBME", distrib = "nbinom")
dredge_count_models(sbmd, group = "RBME", distrib = "nbinom")
test_final_mod(rbme_topmod)
summary(rbme_topmod)

## Red-throated Loon
distribution_test(sbmd, group = "RTLO")
model_tests(sbmd, group = "RTLO", distrib = "nbinom")
dredge_count_models(sbmd, group = "RTLO", distrib = "nbinom")
test_final_mod(rtlo_topmod)
summary(rtlo_topmod)

## Long-tailed Duck
distribution_test(sbmd, group = "LTDU")
model_tests(sbmd, group = "LTDU", distrib = "nbinom")
dredge_count_models(sbmd, group = "LTDU", distrib = "nbinom")
test_final_mod(ltdu_topmod)
summary(ltdu_topmod)

## Black Guillemot
distribution_test(sbmd, group = "BLGU")
model_tests(sbmd, group = "BLGU", distrib = "nbinom")
dredge_count_models(sbmd, group = "BLGU", distrib = "nbinom")
test_final_mod(blgu_topmod)
summary(blgu_topmod)

## Red-necked Grebe
distribution_test(sbmd, group = "RNGR")
model_tests(sbmd, group = "RNGR", distrib = "nbinom")
dredge_count_models(sbmd, group = "RNGR", distrib = "nbinom")
test_final_mod(rngr_topmod)
summary(rngr_topmod)

## Razorbill
distribution_test(sbmd, group = "RAZO")
model_tests(sbmd, group = "RAZO", distrib = "nbinom")
dredge_count_models(sbmd, group = "RAZO", distrib = "nbinom")
test_final_mod(razo_topmod)
summary(razo_topmod)




#------------------------------------------------#
####          Trend Visualization             ####
#------------------------------------------------#

### All seabirds trend
summary(seabirds_topmod)


## Calculate average marginal effects for each parameter
ame.all <- avg_slopes(seabirds_topmod)
ame.all


## Format into table output
ame.tab.all <- ame.all %>% 
  mutate(ame = paste0(round(estimate, 2), " (", round(conf.low, 2), " to ", 
                      round(conf.high, 2), ")")) %>% 
  select(term, ame) %>% 
  pivot_wider(names_from = term, values_from = ame) %>% 
  mutate(species = "All seabirds",
         year = NA,
         tsm = NA,
         wind.speed = NA,
         wind.dir.cos = NA,
         wind.dir.sin = NA) %>% 
  select(species, year, doy, obs.hours, tsm, wind.speed, wind.dir.cos, 
         wind.dir.sin, pressure)


### Species specific trends
summary(coei_topmod)
summary(colo_topmod)
summary(dcco_topmod)
summary(noga_topmod)
summary(susc_topmod)
summary(wwsc_topmod)
summary(blsc_topmod)
summary(grco_topmod) #*+
summary(rbme_topmod) #*+
summary(rtlo_topmod) #*+
summary(ltdu_topmod)
summary(blgu_topmod) #*-
summary(rngr_topmod) #*-
summary(razo_topmod)


## Create a list of models to enter into the ggpredict looping function
modlist.y <- list(grco_topmod, rbme_topmod, rtlo_topmod,
                  blgu_topmod, rngr_topmod)


## List of model names for the looping function
spnames.y = c("GRCO", "RBME", "RTLO",
              "BLGU", "RNGR")


## List the parameter input for the looping function
param.y <- rep(list("year"), 5)


## Looping the models through a custom ggpredict function to return a single df
modelpreds.y <- pmap_dfr(list(modlist.y, spnames.y, param.y), model_predictions)


## Plot the trends for each species using facet wrap 
modelpreds.y %>% 
  mutate(sciname = ifelse(group == "GRCO", "Phalacrocorax carbo", 
                          ifelse(group == "RBME", "Mergus serrator",
                                 ifelse(group == "RTLO", "Gavia stellata",
                                        ifelse(group == "BLGU", "Cepphus grylle",
                                               ifelse(group == "RNGR", "Podiceps grisegena", "error"))))),
         sciname = factor(sciname, levels = c("Phalacrocorax carbo", "Mergus serrator", 
                                            "Gavia stellata", "Cepphus grylle", 
                                            "Podiceps grisegena"))) %>%
  ggplot(aes(x = param, y = predicted, group = sciname)) + 
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.3) + 
  facet_wrap(vars(sciname), scales = "free_y") +
  theme_bw() +
  labs(x = "Year", y = "Average daily count") +
  theme(panel.border = element_rect(linewidth = 0.5),
        panel.grid = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 11),
        strip.text = element_text(size = 10, face = "italic"),
        axis.title.x = element_text(margin = unit(c(5, 0, 1, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 1), "mm")),
  )


## Export plot
# ggsave("outputs/forpub/seabird_sig_trend.png", height = 5.8, width = 7,
#        units = "in", dpi = 1200)


### Calculate marginal effects at the mean
## Create model list
tsum <- list(coei_topmod, colo_topmod, dcco_topmod, noga_topmod, susc_topmod,
             wwsc_topmod, blsc_topmod, grco_topmod, rbme_topmod, rtlo_topmod,
             ltdu_topmod, blgu_topmod, rngr_topmod, razo_topmod)


## Build species name list
groups <- list("Common Eider", "Common Loon", "Double-crested Cormorant", 
               "Northern Gannet", "Surf Scoter", "White-winged Scoter", 
               "Black Scoter", "Great Cormorant", "Red-breasted Merganser", 
               "Red-throated Loon", "Long-tailed Duck", "Black Guillemot", 
               "Red-necked Grebe", "Razorbill")


## Input into calc_MEM function to get marginal effects at the mean for each
## parameter from each species model
ames <- map2_dfr(tsum, groups, calc_AME)
ames


## Format into table output
ame.tab <- ames %>% 
  mutate(ame = paste0(round(estimate, 2), " (", round(conf.low, 2), " to ", 
                      round(conf.high, 2), ")")) %>% 
  select(species, term, ame) %>% 
  pivot_wider(names_from = term, values_from = ame) %>% 
  select(species, year, doy, obs.hours, tsm, wind.speed, wind.dir.cos, 
         wind.dir.sin, pressure) %>% 
  bind_rows(ame.tab.all)


## Save table
# write.csv(ame.tab, "outputs/forpub/average_marginal_effects_table.csv", row.names = F)




#------------------------------------------------#
####            Migration Timing              ####
#------------------------------------------------#

## Calculate abundance-weighted mean date for each year for each species
phendat <- sbmd %>% 
  select(species, year, doy, count) %>% 
  group_by(species, year) %>% 
  summarize(w.mean.doy = weighted.mean(doy, count)) %>% 
  ungroup()

phen_dat <- read.csv("outputs/seabird_species_stats.csv") %>% 
  as_tibble() %>% 
  select(sci.name, species) %>% 
  left_join(phendat, ., by = "species") %>% 
  select(sci.name, species, year, w.mean.doy) %>% 
  rename(spcode = species,
         species = sci.name)


#------------------------------------------------#

### Produce model and outputs for all waterbirds
## Create model with a random effect of species to account for psuedoreplication
m.all <- glmmTMB(w.mean.doy ~ scale(year) + (1 | species), data = phen_dat)
summary(m.all)


## Check model residuals
sim.all = simulateResiduals(m.all, plot = T)


## Predict peak migration date from the model
predict.all <- ggpredict(m.all, terms = c("year")) %>% 
  rename(year = x)


## Plot
predict.all %>% 
  ggplot(aes(year, predicted)) + 
  geom_point(aes(year, w.mean.doy), data = phendat) + 
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.3) +
  labs(y = "Date", x = "Year") +
  scale_y_continuous(labels = label_md, breaks = c(274, 288, 305, 319)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title.x = element_text(margin = unit(c(5, 0, 1, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 1), "mm")))


## Export plot
# ggsave("outputs/forpub/seabird_mig_timing_trend.png", height = 5.7, width = 7,
#        units = "in", dpi = 1200)



#------------------------------------------------#

### Produce model and outputs for each focal species
## Create model with an interaction term of year*species
m.spp <- glmmTMB(w.mean.doy ~ scale(year)*species, data = phen_dat)
summary(m.spp)


## Check model residuals
sim.spp <- simulateResiduals(m.spp, plot = T)


## Test for differences in variance among groups
testCategorical(sim.spp, phen_dat$species)


## Now test how each species trend compares to a slope of 0
emms <- emtrends(m.spp, specs = "species", var = "year") %>% 
  test(.)
emms


## Write out emms table for supplement
# write.csv(emms, "outputs/forpub/species_emms_table.csv", row.names = F)


## Predict peak migration date for each species from the model
predict.spp <- ggpredict(m.spp, terms = c("year", "species")) %>% 
  rename(year = x, species = group)


## Plot
predict.spp %>% 
  ggplot(aes(year, predicted)) + 
  geom_point(aes(year, w.mean.doy), data = phen_dat) + 
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.3) +
  facet_wrap("species") +
  labs(y = "Date", x = "Year") +
  scale_y_continuous(labels = label_md, breaks = c(274, 288, 305, 319)) +
  scale_x_continuous(breaks = c(2017, 2019, 2021, 2023)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.text = element_text(face = "italic"),
        axis.title.x = element_text(margin = unit(c(5, 0, 1, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 1), "mm")))


## Export plot
# ggsave("outputs/forpub/species_mig_timing_trend.png", height = 6.5, width = 7,
#        units = "in", dpi = 1200)




