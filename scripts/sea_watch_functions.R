## Functions for use in the seawatch_ms.R script



#------------------------------------------------#
####           Wind calc. function            ####
#------------------------------------------------#

## Function to calculate average wind speed, wind direction, and pressure
## over the observation period
avg_weather <- function(dateinput, starttime, endtime) {
  
  ## Filter weather data to the specific date and times
  winddat <- schweather %>% 
    filter(date %in% dateinput) %>% 
    filter(between(datetime, starttime, endtime))
  
  V_u <- vector()
  V_v <- vector()
  
  ## For loop for each data point in the observation period to calculate U and 
  ## V components
  for(i in 1:nrow(winddat)) {
    
    V_u[i] = mean(winddat$wind.speed[i] * sin(winddat$wind.direction[i] * pi/180))
    V_v[i] = mean(winddat$wind.speed[i] * cos(winddat$wind.direction[i] * pi/180))
    
  }
  
  ## Calculate mean U and V components
  mV_u = mean(V_u, na.rm = T)
  mV_v = mean(V_v, na.rm = T)
  
  ## Now calculate actual mean
  mean_WD <- 180 + (180/pi) * atan2(mV_v, mV_u)
  # mean_WD <- atan2(mV_north, mV_east) * 180/pi
  
  ## Need to correct if mean is negative so add 360 to negative numbers
  # if(is.na(mean_WD)) {
  #   wind_mean <- mean_WD
  # }
  # 
  # 
  # if(!is.na(mean_WD)) {
  # 
  #   if(mean_WD < 0) {
  #     wind_mean <- 360 + mean_WD
  #   }
  # 
  #   if(mean_WD >= 0) {
  #     wind_mean <- mean_WD
  #   }
  # 
  # }
  
  wind_mean <- mean_WD
  
  ## Finally compile the data into a df for output
  findat <- winddat %>% 
    group_by(date) %>% 
    summarise(wind.speed = mean(wind.speed, na.rm = T),
              wind.dir = wind_mean,
              pressure = mean(pressure, na.rm = T))
  
  
  return(findat)
  
}





#------------------------------------------------#
####           Modelling functions            ####
#------------------------------------------------#

## Create a model to test the best distributions for a global model
## Tests Poisson, Negative Binomial, and a Zero-inflated Negative Binomial
distribution_test <- function(group) {
  
  ## Filter and fine tune data for model input
  birddat <- twd %>% 
    filter(grouping2 == paste(group)) %>% 
    select(-species) %>% 
    group_by(date, year, doy, tsm, total.obs.mins, obs.hours, wind.speed, 
             wind.dir, pressure, observer) %>% 
    summarise(count = sum(count)) %>% 
    na.omit(.)
  
  ## Create distribution model comparison on a global model
  ## Testing Poisson, Negative Binomial, and a Zero-inflated Negative Binomial
  poissont <- glmmTMB(count ~ scale(year) + scale(tsm) + scale(doy^2) +
                      scale(pressure) + scale(wind.speed) + cos(wind.dir) + 
                      sin(wind.dir) + (1 | observer) + (1 | obs.hours), 
                      data = birddat, family = poisson)
  nbinomt <- glmmTMB(count ~ scale(year) + scale(tsm) + scale(doy^2) +
                     scale(pressure) + scale(wind.speed) + cos(wind.dir) + 
                     sin(wind.dir) + (1 | observer) + (1 | obs.hours),
                     data = birddat, family = nbinom2)
  zi.nbinomt <- glmmTMB(count ~ scale(year) + scale(tsm) + scale(doy^2) +
                        scale(pressure) + scale(wind.speed) + cos(wind.dir) + 
                        sin(wind.dir) + (1 | observer) + (1 | obs.hours),
                        data = birddat, family = nbinom2, ziformula = ~1)
  
  ## Define list of models
  models <- list(poissont, nbinomt, zi.nbinomt)
  
  ## Specify model names
  mod.names <- c('poisson', 'nbinom', 'nbinom+zi')
  
  ## Calculate AIC of each model
  aictab(cand.set = models, modnames = mod.names)
  
}



## Create a model to test the top distribution model for overdispersion, 
## zero-inflation, and temporal autocorrelation

model_tests <- function(group, distrib) {
  
  ## Filter and fine tune data for model input
  birddat <- twd %>% 
    filter(grouping2 == paste(group)) %>% 
    select(-species) %>% 
    group_by(date, year, doy, tsm, total.obs.mins, obs.hours, wind.speed, 
             wind.dir, pressure, observer) %>% 
    summarise(count = sum(count)) %>% 
    na.omit(.)
  
  ## Specify best fit distribution from distribution_test
  if (paste(distrib) == "poisson") {
    top.mod <- glmmTMB(count ~ scale(year) + scale(tsm) + scale(doy^2) +
                       scale(pressure) + scale(wind.speed) + cos(wind.dir) + 
                       sin(wind.dir) + (1 | observer) + (1 | obs.hours), 
                       data = birddat, family = poisson)
  }
  
  if (paste(distrib) == "nbinom") {
    top.mod <- glmmTMB(count ~ scale(year) + scale(tsm) + scale(doy^2) +
                       scale(pressure) + scale(wind.speed) + cos(wind.dir) + 
                       sin(wind.dir) + (1 | observer) + (1 | obs.hours),
                       data = birddat, family = nbinom2)
  }
  
  if (paste(distrib) == "nbinom+zi") {
    top.mod <- glmmTMB(count ~ scale(year) + scale(tsm) + scale(doy^2) +
                       scale(pressure) + scale(wind.speed) + cos(wind.dir) + 
                       sin(wind.dir) + (1 | observer) + (1 | obs.hours),
                       data = birddat, family = nbinom2, ziformula = ~1)
  }
  
  
  ## Test top model for overdispersion and zero-inflation
  sim.mod = simulateResiduals(top.mod)
  plot(sim.mod)
  testDispersion(sim.mod)
  testZeroInflation(sim.mod)
  
  ## Test for temporal autocorrelation
  ## Recalculate residuals because we have many obs/time interval
  sim.mod2 = recalculateResiduals(sim.mod, group = unique(birddat$year))
  testTemporalAutocorrelation(sim.mod2, time = unique(birddat$year), plot = TRUE)
  
}



## Run the full set of candidate models with the best distribution and return 
## the AICc table and top model. Top model is saved as an object to the GE.
run_count_models <- function(group, distrib) {
  
  ## Filter and fine tune data for model input
  birddat <- twd %>% 
    filter(grouping2 == paste(group)) %>% 
    select(-species) %>% 
    group_by(date, year, doy, tsm, total.obs.mins, obs.hours, wind.speed, 
             wind.dir, pressure, observer) %>% 
    summarise(count = sum(count)) %>% 
    na.omit(.)
  
  ## Now we can build the models for AICc model comparison
  ## Run models based on distribution
  if (paste(distrib) == "poisson") {
    null <- glmmTMB(count ~ (1 | observer) + (1 | obs.hours), data = birddat, 
                    family = poisson)
    year <- glmmTMB(count ~ scale(year) + (1 | observer) + (1 | obs.hours), 
                    data = birddat, family = poisson)
    yeardoy <- glmmTMB(count ~ scale(year) + scale(doy) + (1 | observer) + 
                      (1 | obs.hours), data = birddat, family = poisson)
    yeartime <- glmmTMB(count ~ scale(year) + scale(tsm) + (1 | observer) + 
                       (1 | obs.hours), data = birddat, family = poisson)
    yearwindsp <- glmmTMB(count ~ scale(year) + scale(wind.speed) + 
                         (1 | observer) + (1 | obs.hours), data = birddat, 
                         family = poisson)
    yearwinddr <- glmmTMB(count ~ scale(year) + cos(wind.dir) + sin(wind.dir) + 
                         (1 | observer) + (1 | obs.hours), data = birddat, 
                         family = poisson)
    yearpressure <- glmmTMB(count ~ scale(year) + scale(pressure) + (1 | observer) + 
                           (1 | obs.hours), data = birddat, family = poisson)
    yeardwind <- glmmTMB(count ~ scale(year) + scale(doy) + scale(wind.speed) + 
                           (1 | observer) + (1 | obs.hours), data = birddat, 
                         family = poisson)
    nowinddr <- glmmTMB(count ~ scale(year) + scale(doy) + scale(tsm) + 
                        scale(wind.speed) + scale(pressure) + (1 | observer) + 
                       (1 | obs.hours), data = birddat, family = poisson)
    nowindsp <- glmmTMB(count ~ scale(year) + scale(doy) + scale(tsm) + 
                        cos(wind.dir) + sin(wind.dir) + scale(pressure) + 
                       (1 | observer) + (1 | obs.hours), data = birddat, 
                        family = poisson)
    global <- glmmTMB(count ~ scale(year) + scale(doy) + scale(tsm) + 
                      scale(wind.speed) + cos(wind.dir) + sin(wind.dir) + 
                      scale(pressure) + (1 | observer) + (1 | obs.hours), 
                      data = birddat, family = poisson)
  }
  
  if (paste(distrib) == "nbinom") {
    null <- glmmTMB(count ~ (1 | observer) + (1 | obs.hours), data = birddat, 
                    family = nbinom2)
    year <- glmmTMB(count ~ scale(year) + (1 | observer) + (1 | obs.hours), 
                    data = birddat, family = nbinom2)
    yeardoy <- glmmTMB(count ~ scale(year) + scale(doy) + (1 | observer) + 
                         (1 | obs.hours), data = birddat, family = nbinom2)
    yeartime <- glmmTMB(count ~ scale(year) + scale(tsm) + (1 | observer) + 
                          (1 | obs.hours), data = birddat, family = nbinom2)
    yearwindsp <- glmmTMB(count ~ scale(year) + scale(wind.speed) + 
                            (1 | observer) + (1 | obs.hours), data = birddat, 
                          family = nbinom2)
    yearwinddr <- glmmTMB(count ~ scale(year) + cos(wind.dir) + sin(wind.dir) + 
                            (1 | observer) + (1 | obs.hours), data = birddat, 
                          family = nbinom2)
    yearpressure <- glmmTMB(count ~ scale(year) + scale(pressure) + (1 | observer) + 
                              (1 | obs.hours), data = birddat, family = nbinom2)
    yeardwind <- glmmTMB(count ~ scale(year) + scale(doy) + scale(wind.speed) + 
                           (1 | observer) + (1 | obs.hours), data = birddat, 
                         family = nbinom2)
    nowinddr <- glmmTMB(count ~ scale(year) + scale(doy) + scale(tsm) + 
                          scale(wind.speed) + scale(pressure) + (1 | observer) + 
                          (1 | obs.hours), data = birddat, family = nbinom2)
    nowindsp <- glmmTMB(count ~ scale(year) + scale(doy) + scale(tsm) + 
                          cos(wind.dir) + sin(wind.dir) + scale(pressure) + 
                          (1 | observer) + (1 | obs.hours), data = birddat, 
                        family = nbinom2)
    global <- glmmTMB(count ~ scale(year) + scale(doy) + scale(tsm) + 
                        scale(wind.speed) + cos(wind.dir) + sin(wind.dir) + 
                        scale(pressure) + (1 | observer) + (1 | obs.hours), 
                      data = birddat, family = nbinom2)
  }
  
  if (paste(distrib) == "nbinom+zi") {
    null <- glmmTMB(count ~ (1 | observer) + (1 | obs.hours), data = birddat, 
                    family = nbinom2, ziformula = ~1)
    year <- glmmTMB(count ~ scale(year) + (1 | observer) + (1 | obs.hours), 
                    data = birddat, family = nbinom2, ziformula = ~1)
    yeardoy <- glmmTMB(count ~ scale(year) + scale(doy) + (1 | observer) + 
                         (1 | obs.hours), data = birddat, family = nbinom2, 
                       ziformula = ~1)
    yeartime <- glmmTMB(count ~ scale(year) + scale(tsm) + (1 | observer) + 
                          (1 | obs.hours), data = birddat, family = nbinom2, 
                        ziformula = ~1)
    yearwindsp <- glmmTMB(count ~ scale(year) + scale(wind.speed) + 
                            (1 | observer) + (1 | obs.hours), data = birddat, 
                          family = nbinom2, ziformula = ~1)
    yearwinddr <- glmmTMB(count ~ scale(year) + cos(wind.dir) + sin(wind.dir) + 
                            (1 | observer) + (1 | obs.hours), data = birddat, 
                          family = nbinom2, ziformula = ~1)
    yearpressure <- glmmTMB(count ~ scale(year) + scale(pressure) + (1 | observer) + 
                              (1 | obs.hours), data = birddat, family = nbinom2, 
                            ziformula = ~1)
    yeardwind <- glmmTMB(count ~ scale(year) + scale(doy) + scale(wind.speed) + 
                           (1 | observer) + (1 | obs.hours), data = birddat, 
                         family = nbinom2, ziformula = ~1)
    nowinddr <- glmmTMB(count ~ scale(year) + scale(doy) + scale(tsm) + 
                          scale(wind.speed) + scale(pressure) + (1 | observer) + 
                          (1 | obs.hours), data = birddat, family = nbinom2, 
                        ziformula = ~1)
    nowindsp <- glmmTMB(count ~ scale(year) + scale(doy) + scale(tsm) + 
                          cos(wind.dir) + sin(wind.dir) + scale(pressure) + 
                          (1 | observer) + (1 | obs.hours), data = birddat, 
                        family = nbinom2, ziformula = ~1)
    global <- glmmTMB(count ~ scale(year) + scale(doy) + scale(tsm) + 
                        scale(wind.speed) + cos(wind.dir) + sin(wind.dir) + 
                        scale(pressure) + (1 | observer) + (1 | obs.hours), 
                      data = birddat, family = nbinom2, ziformula = ~1)
  }
  
  ## Define list of models
  models <- list(null, year, yeardoy, yeartime, yearwindsp, yearwinddr,
                 yearpressure, yeardwind, nowinddr, nowindsp, global)
  
  ## Specify model names
  mod.names <- c('null', 'year', 'yeardoy', 'yeartime', 'yearwindsp', 
                 'yearwinddr', 'yearpressure', 'yeardwind', 'nowinddr', 
                 'nowindsp', 'global')
  
  ## Calculate AIC of each model
  print(aictab(cand.set = models, modnames = mod.names))
  
  ## Get top model and rename to species and model
  topmodel <- get(aictab(cand.set = models, modnames = mod.names)[1,1])
  assign(paste0(tolower(str_remove(paste(group), " ")), "_", 
                paste(str_remove(aictab(cand.set = models,
                                        modnames = mod.names)[1,1], " "))),
         topmodel, envir = .GlobalEnv)
  
}















# null <- glmmTMB(count ~ (1 | observer), data = birddat, family = poisson)
# year <- glmmTMB(count ~ scale(year) + (1 | observer), data = birddat, 
#                 family = poisson)
# yt <- glmmTMB(count ~ scale(year) + scale(tsm) + (1 | observer),
#               data = birddat, family = poisson)
# yh <- glmmTMB(count ~ scale(year) + scale(obs.hours) + (1 | observer),
#               data = birddat, family = poisson)
# yw <- glmmTMB(count ~ scale(year) + wind.dir + (1 | observer), 
#               data = birddat, family = poisson)
# yv <- glmmTMB(count ~ scale(year) + visibility + (1 | observer), 
#               data = birddat, family = poisson)
# most <- glmmTMB(count ~ scale(year) + scale(tsm) + scale(obs.hours) + 
#                   visibility + (1 | observer), data = birddat, 
#                 family = poisson)
# most2 <- glmmTMB(count ~ scale(year) + scale(tsm) + scale(obs.hours) + 
#                    wind.dir + (1 | observer), data = birddat, 
#                  family = poisson)
# global <- glmmTMB(count ~ scale(year) + scale(tsm) + scale(obs.hours) +
#                     wind.dir + visibility + (1 | observer), data = birddat,
#                   family = poisson)