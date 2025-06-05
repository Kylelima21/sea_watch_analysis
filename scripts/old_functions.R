## Run the full set of candidate models with the best distribution and return 
## the AICc table and top model. Top model is saved as an object to the GE.
run_count_models <- function(data, group, distrib) {
  
  ## Filter and fine tune data for model input
  birddat <- data %>% 
    filter(grouping2 == paste(group)) %>% 
    select(-species) %>% 
    group_by(date, year, doy, tsm, total.obs.mins, obs.hours, wind.speed, 
             wind.dir, pressure, observer) %>% 
    summarise(count = sum(count)) %>% 
    na.omit(.)
  
  ## Now we can build the models for AICc model comparison
  ## Run models based on distribution
  if (paste(distrib) == "poisson") {
    null <- glmmTMB(count ~ (1 | observer), data = birddat, 
                    family = poisson)
    year <- glmmTMB(count ~ year + (1 | observer), 
                    data = birddat, family = poisson)
    yeardoy <- glmmTMB(count ~ year + scale(doy) + scale(doy^2) + 
                         (1 | observer), data = birddat, 
                       family = poisson)
    yeartime <- glmmTMB(count ~ year + scale(tsm) + (1 | observer),
                        data = birddat, family = poisson)
    yearwindsp <- glmmTMB(count ~ year + scale(wind.speed) + 
                            (1 | observer), data = birddat, 
                          family = poisson)
    yearwinddr <- glmmTMB(count ~ year + cos(wind.dir) + sin(wind.dir) + 
                            (1 | observer), data = birddat, 
                          family = poisson)
    yearpressure <- glmmTMB(count ~ year + scale(pressure) + (1 | observer),
                            data = birddat, family = poisson)
    yeardwind <- glmmTMB(count ~ year + scale(doy) + scale(doy^2) + 
                           scale(wind.speed) + (1 | observer), 
                         data = birddat, family = poisson)
    nowinddr <- glmmTMB(count ~ year + scale(doy) + scale(doy^2) + 
                          scale(tsm) + scale(wind.speed) + scale(pressure) + 
                          (1 | observer), data = birddat, 
                        family = poisson)
    nowindsp <- glmmTMB(count ~ year + scale(doy) + scale(doy^2) +
                          scale(tsm) + cos(wind.dir) + sin(wind.dir) + 
                          scale(pressure) + (1 | observer), 
                        data = birddat, family = poisson)
    global <- glmmTMB(count ~ year + scale(doy) + scale(doy^2) +
                        scale(tsm) + scale(wind.speed) + cos(wind.dir) + 
                        sin(wind.dir) + scale(pressure) + (1 | observer),
                      data = birddat, family = poisson)
  }
  
  if (paste(distrib) == "nbinom") {
    null <- glmmTMB(count ~ (1 | observer), data = birddat, 
                    family = nbinom2)
    year <- glmmTMB(count ~ year + (1 | observer), 
                    data = birddat, family = nbinom2)
    yeardoy <- glmmTMB(count ~ year + scale(doy) + scale(doy^2) + 
                         (1 | observer), data = birddat, 
                       family = nbinom2)
    yeartime <- glmmTMB(count ~ year + scale(tsm) + (1 | observer),
                        data = birddat, family = nbinom2)
    yearwindsp <- glmmTMB(count ~ year + scale(wind.speed) + 
                            (1 | observer), data = birddat, 
                          family = nbinom2)
    yearwinddr <- glmmTMB(count ~ year + cos(wind.dir) + sin(wind.dir) + 
                            (1 | observer), data = birddat, 
                          family = nbinom2)
    yearpressure <- glmmTMB(count ~ year + scale(pressure) + (1 | observer), 
                            data = birddat, family = nbinom2)
    yeardwind <- glmmTMB(count ~ year + scale(doy) + scale(doy^2) +
                           scale(wind.speed) + (1 | observer), 
                         data = birddat, family = nbinom2)
    nowinddr <- glmmTMB(count ~ year + scale(doy) + scale(doy^2) + 
                          scale(tsm) + scale(wind.speed) + scale(pressure) + 
                          (1 | observer), data = birddat, 
                        family = nbinom2)
    nowindsp <- glmmTMB(count ~ year + scale(doy) + scale(doy^2) + 
                          scale(tsm) + cos(wind.dir) + sin(wind.dir) + 
                          scale(pressure) + (1 | observer), 
                        data = birddat, family = nbinom2)
    global <- glmmTMB(count ~ year + scale(doy) + scale(doy^2) + 
                        scale(tsm) + scale(wind.speed) + cos(wind.dir) + 
                        sin(wind.dir) + scale(pressure) + (1 | observer),
                      data = birddat, family = nbinom2)
  }
  
  if (paste(distrib) == "nbinom+zi") {
    null <- glmmTMB(count ~ (1 | observer), data = birddat, 
                    family = nbinom2, ziformula = ~1)
    year <- glmmTMB(count ~ year + (1 | observer), 
                    data = birddat, family = nbinom2, ziformula = ~1)
    yeardoy <- glmmTMB(count ~ year + scale(doy) + scale(doy^2) + 
                         (1 | observer), data = birddat, 
                       family = nbinom2, ziformula = ~1)
    yeartime <- glmmTMB(count ~ year + scale(tsm) + (1 | observer),
                        data = birddat, family = nbinom2, 
                        ziformula = ~1)
    yearwindsp <- glmmTMB(count ~ year + scale(wind.speed) + 
                            (1 | observer), data = birddat, 
                          family = nbinom2, ziformula = ~1)
    yearwinddr <- glmmTMB(count ~ year + cos(wind.dir) + sin(wind.dir) + 
                            (1 | observer), data = birddat, 
                          family = nbinom2, ziformula = ~1)
    yearpressure <- glmmTMB(count ~ year + scale(pressure) + (1 | observer),
                            data = birddat, family = nbinom2, 
                            ziformula = ~1)
    yeardwind <- glmmTMB(count ~ year + scale(doy) + scale(doy^2) + 
                           scale(wind.speed) + (1 | observer), 
                         data = birddat, family = nbinom2, ziformula = ~1)
    nowinddr <- glmmTMB(count ~ year + scale(doy) + scale(doy^2) + 
                          scale(tsm) + scale(wind.speed) + scale(pressure) + 
                          (1 | observer), data = birddat, 
                        family = nbinom2, ziformula = ~1)
    nowindsp <- glmmTMB(count ~ year + scale(doy) + scale(doy^2) + 
                          scale(tsm) + cos(wind.dir) + sin(wind.dir) + 
                          scale(pressure) + (1 | observer), 
                        data = birddat, family = nbinom2, ziformula = ~1)
    global <- glmmTMB(count ~ year + scale(doy) + scale(doy^2) + 
                        scale(tsm) + scale(wind.speed) + cos(wind.dir) + 
                        sin(wind.dir) + scale(pressure) + (1 | observer),
                      data = birddat, family = nbinom2, 
                      ziformula = ~1)
  }
  
  ## Define list of models
  models <- list(null, year, yeardoy, yeartime, yearwindsp, yearwinddr,
                 yearpressure, yeardwind, nowinddr, nowindsp, global, globalsp)
  
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
  
  ## Test top model for overdispersion and zero-inflation
  sim.mod = simulateResiduals(topmodel)
  plot(sim.mod)
  testDispersion(sim.mod)
  testZeroInflation(sim.mod)
  
  ## Test for temporal autocorrelation
  ## Recalculate residuals because we have many obs/time interval
  sim.mod2 = recalculateResiduals(sim.mod, group = unique(birddat$year))
  testTemporalAutocorrelation(sim.mod2, time = unique(birddat$year), plot = TRUE)
  
}