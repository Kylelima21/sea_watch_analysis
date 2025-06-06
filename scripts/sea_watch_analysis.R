#------------------------------------------------#
####           Packages and Source            ####
#------------------------------------------------#

## Packages
library(tidyverse)
library(lubridate)
library(scales)
library(lmerTest)
library(ggeffects)

## Source custom functions
source("scripts/sea_watch_functions.R")




#------------------------------------------------#
####     Read and clean manipulated data      ####
#------------------------------------------------#

swdat <- tibble(read.csv("data/sea_watch_cleaned_data_20240118.csv")) %>% 
  select(-c(time.of.obs, start.time.2, wind.dir.speed, visibility)) %>% 
  rename(observer = principle.observer) %>% 
  mutate(date = as.Date(date, format = "%m/%d/%y"),
         start.datetime = paste(date, start.time),
         start.datetime = gsub("\\d*\\-\\d*\\-\\d*\\s\\NA", "NA", start.datetime),
         start.datetime = ymd_hm(start.datetime),
         observer = ifelse(observer == "SB/ZK", "SB/RZK", observer),
         observer = ifelse(observer == "RZK/SB", "SB/RZK", observer),
         observer = ifelse(observer == "STB/RZK", "SB/RZK", observer),
         observer = ifelse(observer == "ZK", "RZK", observer)) %>% 
  filter(year != 2014 & year != 2015)




#------------------------------------------------#
####              Summary Stats               ####
#------------------------------------------------#

## Number of days surveyed from 2016 - 2023 -- 626
swdat %>% 
  distinct(date) %>% 
  nrow() 


## Total hours surveyed -- 119,247 hours
swdat %>% 
  summarise(time = sum(obs.hours))


## Number of observers -- 28 levels but 18 human observers
swdat %>% 
  filter(!is.na(observer)) %>% 
  distinct(observer) %>% 
  arrange(observer) %>% 
  print(n = nrow(.))


## Total hours by season
swdat %>% 
  group_by(year) %>% 
  summarise(total.effort = sum(obs.hours))


## Mean hours of effort per day for each season
swdat %>% 
  group_by(year) %>% 
  summarise(mean.daily.effort = mean(obs.hours))


## Total individual birds recorded -- 449,707 individuals
swdat %>% 
  summarise(count = sum(day.total))


## Total individual birds and birds per hour of effort by year
swdat %>% 
  group_by(year) %>% 
  summarise(total.effort = sum(obs.hours),
            total.indiv = sum(day.total)) %>% 
  mutate(bph = total.indiv/total.effort)


## Total individual birds by observer
swdat %>% 
  group_by(observer) %>% 
  summarise(total.effort = sum(obs.hours),
            total.indiv = sum(day.total)) %>% 
  mutate(bph = total.indiv/total.effort) %>% 
  arrange(-bph)


## Totals by species
test <- swdat %>% 
  group_by(species) %>% 
  summarise(total.effort = 1879.1500,
            total.indiv = sum(day.total)) %>% 
  mutate(bph = total.indiv/total.effort) %>% 
  arrange(-bph)


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
####         Total Bird Populations           ####
#------------------------------------------------#

## Format data for modeling
total.dat <- swdat %>% 
  rename(visibility = visibility.score, wind.dir = wind.direction) %>% 
  group_by(date, year, obs.hours, wind.dir, visibility, observer) %>% 
  summarise(total.individs = sum(day.total)) %>% 
  filter(obs.hours != 0) %>% 
  mutate(bph = total.individs / obs.hours)


## Explore histogram of bph data -- heavily right skewed
hist(total.dat$bph)


## Exploratory plot
total.dat %>% 
  mutate(plot.date = as.Date(paste(2000, month(date), day(date), sep = "-")),
         year = factor(year, levels = c(2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023))) %>% 
  ggplot(aes(x = plot.date, y = bph, color = year)) +
  geom_line()


## Think about models
mod.tb <- lmerTest::lmer(log(bph + 1) ~ year + wind.dir + visibility + (1 | observer), 
                         data = total.dat)

explore_model(mod.tb)


mod.predtb <- ggpredict(mod.tb, terms = c("year"))


ggplot(mod.predtb, aes(x, predicted)) +
  geom_point(data = total.dat, aes(year, bph),
             color = "grey40", size = 0.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(linewidth = 0.8, lineend = "round") +
  scale_y_continuous(limits = c(0, 500)) +
  scale_x_continuous(limits = c(2016, 2023)) +
  labs(x = "Year", y = "Birds per hour") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = "12"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(color = "black", size = "12"),
        axis.title.x = element_text(margin = margin(0.6, 0, 0, 0, "cm")),
        axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, "cm")),
        panel.background = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.border = element_rect(color = 'black', fill = NA)) 




#------------------------------------------------#
####         Species Trend Models             ####
#------------------------------------------------#

## Number of days with detection per species
sp.count <- swdat %>% 
  filter(day.total > 0) %>% 
  select(species) %>% 
  group_by(species) %>% 
  tally() %>% 
  arrange(-n)


## Format data for modeling
sp.dat <- swdat %>% 
  rename(visibility = visibility.score, wind.dir = wind.direction, bph = count.p.hour) %>% 
  filter(obs.hours != 0) %>% 
  left_join(., sp.count, by = "species") %>% 
  filter(n > 30 & !grepl("sp.", species, ignore.case = TRUE))


## Explore histogram of bph data -- heavily right skewed
hist(sp.dat$bph)


## 
coei <- sp.dat %>% 
  filter(species == "COEI")

mod.coei <- lmerTest::lmer(log(bph + 1) ~ year + visibility + (1 | observer), 
     data = coei)

explore_model(mod.coei)


mod.pred.coei <- ggpredict(mod.coei, terms = c("year"))


ggplot(mod.pred.coei, aes(x, predicted)) +
  geom_point(data = coei, aes(year, bph),
             color = "grey40", size = 0.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(linewidth = 0.8, lineend = "round") +
  scale_y_continuous(limits = c(0, 200)) +
  labs(x = "Year", y = "Birds per hour") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = "12"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(color = "black", size = "12"),
        axis.title.x = element_text(margin = margin(0.6, 0, 0, 0, "cm")),
        axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, "cm")),
        panel.background = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.border = element_rect(color = 'black', fill = NA)) 








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






         