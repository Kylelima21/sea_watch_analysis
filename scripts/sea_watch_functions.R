## Functions for use in the seawatch_ms.R script


#------------------------------------------------#
####            Plotting function             ####
#------------------------------------------------#

label_md <- function(x) {
  
  format(as.Date(x, origin = "2020-01-01"), "%b-%d")
  
}





#------------------------------------------------#
####           Wind calc. function            ####
#------------------------------------------------#

## Function to calculate average wind speed, wind direction, and pressure
## over the observation period 
avg_windp <- function(dateinput) {
  
  ## Filter weather data to the specific date and times
  winddat <- schweather %>% 
    filter(date %in% dateinput) %>% 
    mutate(time = format(datetime,  format = "%H:%M:%S")) %>% 
    filter(time >= "03:00:00" & time < "19:15:00") %>% 
    select(-time)
    #filter(between(datetime, starttime, endtime))
  
  ## Convert degrees to radians
  ## Calculate the east-west and north-south components
  ## Get the average of the east and north components
  ## Recombine into one angle using arctan2
  ## Change back to degrees (if you want)
  dir.avg <- winddat %>% 
    mutate(dir_rad = wind.direction * pi / 180,
           dir_east = wind.speed * sin(dir_rad),
           dir_north = wind.speed * cos(dir_rad)) %>% 
    group_by(date) %>% 
    summarise(dir_east_mean = mean(dir_east, na.rm = T),
              dir_north_mean = mean(dir_north, na.rm = T)) %>% 
    mutate(dir_mean_rad = atan2(dir_east_mean, dir_north_mean),
           dir_mean_deg = (360 + dir_mean_rad * 180/pi) %% 360) %>% 
    select(date, dir_mean_deg)
  
  wind.spd <- winddat %>% 
    group_by(date) %>% 
    summarise(wind.speed = mean(wind.speed, na.rm = T),
              pressure = mean(pressure, na.rm = T))
  
  avgw <- left_join(dir.avg, wind.spd, by = "date") %>% 
    rename(wind.dir = dir_mean_deg)
  
  ## Return final value in degrees
  return(avgw)
  
}







#------------------------------------------------#
####           Modelling functions            ####
#------------------------------------------------#

## Create a model to test the best distributions for a global model
## Tests Poisson, Negative Binomial, and a Zero-inflated Negative Binomial
distribution_test <- function(data, group, autocorrelated = F) {
  
  options(dplyr.summarise.inform = FALSE)
  
  ## Filter and fine tune data for model input
  if (group != "Seabirds") {
    
    birddat <- data %>% 
      filter(species == paste(group)) %>% 
      select(-species) %>% 
      group_by(date, year, doy, tsm, total.obs.mins, obs.hours, wind.speed, 
               wind.dir.cos, wind.dir.sin, pressure, observer) %>% 
      summarise(count = sum(count)) %>% 
      na.omit(.)
  }
  

  ## Create distribution model comparison on a global model
  ## Testing Poisson, Negative Binomial, and a Zero-inflated Negative Binomial
  if (group != "Seabirds" & autocorrelated == FALSE) {
    
    poissont <- glmmTMB(count ~ scale(year) + scale(tsm) + scale(doy) + I(scale(doy)^2) + 
                          scale(pressure) + scale(wind.speed) + 
                          scale(wind.dir.cos) + scale(wind.dir.sin) + scale(obs.hours) + 
                          (1 | observer), data = birddat, family = poisson)
    nbinomt <- glmmTMB(count ~ scale(year) + scale(tsm) + scale(doy) + I(scale(doy)^2) +  
                         scale(pressure) + scale(wind.speed) + 
                         scale(wind.dir.cos) + scale(wind.dir.sin) + scale(obs.hours) + 
                         (1 | observer), data = birddat, family = nbinom2)
    zi.nbinomt <- glmmTMB(count ~ scale(year) + scale(tsm) + scale(doy) + I(scale(doy)^2) + 
                            scale(pressure) + scale(wind.speed) + 
                            scale(wind.dir.cos) + scale(wind.dir.sin) + scale(obs.hours) +
                            (1 | observer), data = birddat, family = nbinom2, 
                          ziformula = ~1)
  }
  
  
  
  if(autocorrelated == TRUE) {
    
    ## Filter and fine tune data for model input
    birddat.a <- birddat %>%
      mutate(thegroup = factor(1),
             fact.year = factor(year, levels = c("2016", "2017", "2018", "2019", "2020",
                                            "2021", "2022", "2023", "2024")))

    poissont <- glmmTMB(count ~ scale(year) + scale(tsm) + scale(doy) + 
                          I(scale(doy)^2) +  + scale(pressure) + scale(wind.speed) +
                          scale(wind.dir.cos) + scale(wind.dir.sin) + scale(obs.hours) +
                          (1 | observer) + ar1(fact.year + 0 | thegroup), data = birddat.a, family = poisson)
    nbinomt <- glmmTMB(count ~ scale(year) + scale(tsm) + scale(doy) + 
                         I(scale(doy)^2) + scale(pressure) + scale(wind.speed) +
                         scale(wind.dir.cos) + scale(wind.dir.sin) + scale(obs.hours) +
                         (1 | observer) + ar1(fact.year + 0 | thegroup), data = birddat.a, family = nbinom2)
    zi.nbinomt <- glmmTMB(count ~ scale(year) + scale(tsm) + scale(doy) + 
                            I(scale(doy)^2) + scale(pressure) + scale(wind.speed) +
                            scale(wind.dir.cos) + scale(wind.dir.sin) + scale(obs.hours) +
                            (1 | observer) + ar1(fact.year + 0 | thegroup), data = birddat.a, family = nbinom2,
                          ziformula = ~1)
  }

  

  if(group == "Seabirds") {
    
    birddat.w <- data

    poissont <- glmmTMB(count ~ scale(year) + scale(tsm) + scale(doy) + I(scale(doy)^2) + 
                          scale(pressure) + scale(wind.speed) +
                          scale(wind.dir.cos) + scale(wind.dir.sin) + scale(obs.hours) +
                          (1 | observer) + (year | species), data = birddat.w, family = poisson)
    nbinomt <- glmmTMB(count ~ scale(year) + scale(tsm) + scale(doy) + I(scale(doy)^2) + 
                         scale(pressure) + scale(wind.speed) +
                         scale(wind.dir.cos) + scale(wind.dir.sin) + scale(obs.hours) +
                         (1 | observer) + (year | species), data = birddat.w, family = nbinom2)
    zi.nbinomt <- glmmTMB(count ~ scale(year) + scale(tsm) + scale(doy) + I(scale(doy)^2) + 
                            scale(pressure) + scale(wind.speed) +
                            scale(wind.dir.cos) + scale(wind.dir.sin) + scale(obs.hours) +
                            (1 | observer) + (year | species), data = birddat.w, family = nbinom2,
                          ziformula = ~1)
    
  }
  
  
  ## Define list of models
  models <- list(poissont, nbinomt, zi.nbinomt)

  ## Specify model names
  mod.names <- c('poisson', 'nbinom', 'nbinom+zi')

  ## Calculate AIC of each model
  aictab(cand.set = models, modnames = mod.names)
  
}





## Create a model to test the top distribution model for overdispersion, 
## zero-inflation, and temporal autocorrelation

model_tests <- function(data, group, distrib, autocorrelated = F) {
  
  ## Filter and fine tune data for model input
  if (group != "Seabirds") {
    
    birddat <- data %>% 
      filter(species == paste(group)) %>% 
      select(-species) %>% 
      group_by(date, year, doy, tsm, total.obs.mins, obs.hours, wind.speed, 
               wind.dir.cos, wind.dir.sin, pressure, observer) %>% 
      summarise(count = sum(count)) %>% 
      na.omit(.)
  }

  
  if (group != "Seabirds" & autocorrelated == FALSE) {
    
    ## Specify best fit distribution from distribution_test
    if (paste(distrib) == "poisson") {
      top.mod <- glmmTMB(count ~ scale(year) + scale(tsm) + scale(doy) + I(scale(doy)^2) + 
                           scale(pressure) + scale(wind.speed) + 
                           scale(wind.dir.cos) + scale(wind.dir.sin) + scale(obs.hours) +
                           (1 | observer), data = birddat, family = poisson)
    }
    
    if (paste(distrib) == "nbinom") {
      top.mod <- glmmTMB(count ~ scale(year) + scale(tsm) + scale(doy) + I(scale(doy)^2) +  
                           scale(pressure) + scale(wind.speed) + 
                           scale(wind.dir.cos) + scale(wind.dir.sin) + scale(obs.hours) + 
                           (1 | observer), data = birddat, family = nbinom2)
    }
    
    if (paste(distrib) == "nbinom+zi") {
      top.mod <- glmmTMB(count ~ scale(year) + scale(tsm) + scale(doy) + I(scale(doy)^2) +  
                           scale(pressure) + scale(wind.speed) + 
                           scale(wind.dir.cos) + scale(wind.dir.sin) + scale(obs.hours) + 
                           (1 | observer), data = birddat, family = nbinom2, 
                         ziformula = ~1)
    }
  }
  
  
  
  
  if(autocorrelated == TRUE) {
    
    ## Filter and fine tune data for model input
    birddat.a <- birddat %>% 
      mutate(thegroup = factor(1),
             fact.year = factor(year, levels = c("2016", "2017", "2018", "2019", "2020",
                                            "2021", "2022", "2023", "2024")))
    
    if (paste(distrib) == "poisson") {
      top.mod <- glmmTMB(count ~ scale(year) + scale(tsm) + scale(doy) + 
                           I(scale(doy)^2) + scale(pressure) + scale(wind.speed) + 
                           scale(wind.dir.cos) + scale(wind.dir.sin) + scale(obs.hours) +
                           (1 | observer) + ar1(fact.year + 0 | thegroup),
                         data = birddat.a, family = poisson)
    }
    
    if (paste(distrib) == "nbinom") {
      top.mod <- glmmTMB(count ~ scale(year) + scale(tsm) + scale(doy) + 
                           I(scale(doy)^2) + scale(pressure) + scale(wind.speed) + 
                           scale(wind.dir.cos) + scale(wind.dir.sin) + scale(obs.hours) + 
                           (1 | observer) + ar1(fact.year + 0 | thegroup), 
                         data = birddat.a, family = nbinom2)
    }
    
    if (paste(distrib) == "nbinom+zi") {
      top.mod <- glmmTMB(count ~ scale(year) + scale(tsm) + scale(doy) + 
                           I(scale(doy)^2) + scale(pressure) + scale(wind.speed) + 
                           scale(wind.dir.cos) + scale(wind.dir.sin) + scale(obs.hours) + 
                           (1 | observer) + ar1(fact.year + 0 | thegroup), 
                         data = birddat.a, family = nbinom2, ziformula = ~1)
    }
  }
  
  
  
  if(group == "Seabirds") {
    
    birddat <- data
    
    if (paste(distrib) == "poisson") {
      top.mod <- glmmTMB(count ~ scale(year) + scale(tsm) + scale(doy) + I(scale(doy)^2) + 
                           scale(pressure) + scale(wind.speed) + 
                           scale(wind.dir.cos) + scale(wind.dir.sin) + 
                           scale(obs.hours) + (1 | observer) + (year | species), 
                         data = birddat, family = poisson)
    }
    
    if (paste(distrib) == "nbinom") {
      top.mod <- glmmTMB(count ~ scale(year) + scale(tsm) + scale(doy) + I(scale(doy)^2) + 
                           scale(pressure) + scale(wind.speed) + 
                           scale(wind.dir.cos) + scale(wind.dir.sin) + 
                           scale(obs.hours) + (1 | observer) + (year | species), 
                         data = birddat, family = nbinom2)
    }
    
    if (paste(distrib) == "nbinom+zi") {
      top.mod <- glmmTMB(count ~ scale(year) + scale(tsm) + scale(doy) + I(scale(doy)^2) + 
                           scale(pressure) + scale(wind.speed) +
                           scale(wind.dir.cos) + scale(wind.dir.sin) + 
                           scale(obs.hours) + (1 | observer) + (year | species), 
                         data = birddat, family = nbinom2, ziformula = ~1)
    }
  }
  
    
  ## Test top model for overdispersion and zero-inflation
  sim.mod = simulateResiduals(top.mod, plot = T)
  # plot(sim.mod)
  testDispersion(sim.mod)
  testZeroInflation(sim.mod)
  
  ## Test for temporal autocorrelation
  ## Recalculate residuals because we have many obs/time interval
  sim.mod2 = recalculateResiduals(sim.mod, group = unique(birddat$year))
  testTemporalAutocorrelation(sim.mod2, time = unique(birddat$year), plot = TRUE)
  
}








## Run the full set of candidate models with the best distribution and return 
## the AICc table and top model. Top model is saved as an object to the GE.
dredge_count_models <- function(data, group, distrib, autocorrelated = FALSE) {
  
  if (group != "Seabirds" & autocorrelated == FALSE) {
    
    ## Filter and fine tune data for model input
    birddat <- data %>% 
      filter(species == paste(group)) %>% 
      select(-species) %>% 
      group_by(date, year, doy, tsm, total.obs.mins, obs.hours, wind.speed, 
               wind.dir.cos, wind.dir.sin, pressure, observer) %>% 
      summarise(count = sum(count)) %>% 
      na.omit(.)
    
    ## Now we can build the models for AICc model comparison
    ## Run models based on distribution
    if (paste(distrib) == "poisson") {
      glo.mod <- glmmTMB(count ~ scale(year) + scale(tsm) + scale(doy) + I(scale(doy)^2) + 
                           scale(pressure) + scale(wind.speed) + 
                           scale(wind.dir.cos) + scale(wind.dir.sin) + scale(obs.hours) + 
                           (1 | observer), data = birddat, family = poisson,
                         na.action = "na.fail")
    }
    
    if (paste(distrib) == "nbinom") {
      glo.mod <- glmmTMB(count ~ scale(year) + scale(tsm) + scale(doy) + I(scale(doy)^2) + 
                           scale(pressure) + scale(wind.speed) + 
                           scale(wind.dir.cos) + scale(wind.dir.sin) + scale(obs.hours) + 
                           (1 | observer), data = birddat, family = nbinom2,
                         na.action = "na.fail")
    }
    
    if (paste(distrib) == "nbinom+zi") {
      glo.mod <- glmmTMB(count ~ scale(year) + scale(tsm) + scale(doy) + I(scale(doy)^2) +
                           scale(pressure) + scale(wind.speed) + 
                           scale(wind.dir.cos) + scale(wind.dir.sin) + scale(obs.hours) + 
                           (1 | observer), data = birddat, family = nbinom2, 
                         ziformula = ~1, na.action = "na.fail")
    }
  }
  
  
  
  
  if(autocorrelated == TRUE) {
    
    ## Filter and fine tune data for model input
    birddat.a <- data %>% 
      filter(species == paste(group)) %>% 
      select(-species) %>% 
      group_by(date, year, doy, tsm, total.obs.mins, obs.hours, wind.speed, 
               wind.dir.cos, wind.dir.sin, pressure, observer) %>% 
      summarise(count = sum(count)) %>% 
      na.omit(.) %>% 
      mutate(thegroup = factor(1),
             fact.year = factor(year, levels = c("2016", "2017", "2018", "2019", "2020",
                                            "2021", "2022", "2023", "2024")))
    
    if (paste(distrib) == "poisson") {
      glo.mod <- glmmTMB(count ~ scale(year) + scale(tsm) + scale(doy) + I(scale(doy)^2) + 
                           scale(pressure) + scale(wind.speed) + 
                           scale(wind.dir.cos) + scale(wind.dir.sin) + scale(obs.hours) +
                           (1 | observer)  + ar1(fact.year + 0 | thegroup),
                         data = birddat.a, family = poisson, na.action = "na.fail")
    }
    
    if (paste(distrib) == "nbinom") {
      glo.mod <- glmmTMB(count ~ scale(year) + scale(tsm) + scale(doy) + I(scale(doy)^2) +
                           scale(pressure) + scale(wind.speed) + 
                           scale(wind.dir.cos) + scale(wind.dir.sin) + scale(obs.hours) + 
                           (1 | observer) + ar1(fact.year + 0 | thegroup), data = birddat.a, family = nbinom2, 
                         na.action = "na.fail")
    }
    
    if (paste(distrib) == "nbinom+zi") {
      glo.mod <- glmmTMB(count ~ scale(year) + scale(tsm) + scale(doy) + I(scale(doy)^2) +
                           scale(pressure) + scale(wind.speed) + 
                           scale(wind.dir.cos) + scale(wind.dir.sin) + scale(obs.hours) + 
                           (1 | observer) + ar1(fact.year + 0 | thegroup), 
                         data = birddat.a, family = nbinom2, ziformula = ~1, 
                         na.action = "na.fail")
    }
  }
  
  
  if (group == "Seabirds") {
    
    birddat.w <- data
    
    if (paste(distrib) == "poisson") {
      glo.mod <- glmmTMB(count ~ scale(year) + scale(tsm) + scale(doy) + I(scale(doy)^2) + 
                           scale(pressure) + scale(wind.speed) + 
                           scale(wind.dir.cos) + scale(wind.dir.sin) + 
                           scale(obs.hours) + (1 | observer) + (year | species), 
                         data = birddat.w, family = poisson, na.action = "na.fail")
    }
    
    if (paste(distrib) == "nbinom") {
      glo.mod <- glmmTMB(count ~ scale(year) + scale(tsm) + scale(doy) + I(scale(doy)^2) +
                           scale(pressure) + scale(wind.speed) + 
                           scale(wind.dir.cos) + scale(wind.dir.sin) + 
                           scale(obs.hours) + (1 | observer) + (year | species), 
                         data = birddat.w, family = nbinom2, na.action = "na.fail")
    }
    
    if (paste(distrib) == "nbinom+zi") {
      glo.mod <- glmmTMB(count ~ scale(year) + scale(tsm) + scale(doy) + I(scale(doy)^2) + 
                           scale(pressure) + scale(wind.speed) + 
                           scale(wind.dir.cos) + scale(wind.dir.sin) + 
                           scale(obs.hours) + (1 | observer) + (year | species), 
                         data = birddat.w, family = nbinom2, ziformula = ~1, 
                         na.action = "na.fail")
    }
  }
  
  
  ## Dredge
  modlist <- dredge(glo.mod, rank = "AICc", subset = dc(`cond(scale(doy))`, `cond(I(scale(doy)^2))`))
  
  ## Export AIC table
  as_tibble(modlist) %>% 
    filter(delta < 4) %>% 
    write.csv(., paste0("outputs/forpub/aic_tables/", tolower(paste0(group)), "_aictab.csv"))
  
  ## Fix non-convergence in DCCO so that we can calculate weights
  if (group == "DCCO") {
  valid_models <- subset(modlist, !is.na(AICc)) %>% 
    as_tibble() %>% 
    filter(delta < 4) %>% 
    write.csv(., paste0("outputs/forpub/aic_tables/", tolower(paste0(group)), "_aictab.csv"))
  }
  
  ## Get top model
  topmod <- get.models(modlist, 1)[[1]]
  
  ## Rename to species and model and assign to GE
  assign(paste0(tolower(str_remove(paste(group), " ")), "_topmod"), topmod,
         envir = .GlobalEnv)

}





## Calculate predictions for plotting
model_predictions <- function(model, groupname, parameter) {
  
  preddat <- as_tibble(ggpredict(model, terms = paste0(parameter, " [all]"))) %>%
    rename(param = x) %>%
    mutate(group = groupname)
  
  return(preddat)
  
}






## Test final model for issues
test_final_mod <- function(model, outlieryes = F) {
  
  ## Test top model for overdispersion and zero-inflation
  sim.mod = simulateResiduals(model, plot = T)
  # plot(sim.mod)
  testDispersion(sim.mod)
  testZeroInflation(sim.mod)
  
  ## Test for temporal autocorrelation
  ## Recalculate residuals because we have many obs/time interval
  sim.mod2 = recalculateResiduals(sim.mod, group = unique(twd$year))
  testTemporalAutocorrelation(sim.mod2, time = unique(twd$year), plot = TRUE)
  
  if (outlieryes == T) {
    testOutliers(sim.mod, type = "bootstrap")
  }
  
}







## Calculate marginal effects at the mean
calc_AME <- function(model, group) {
  
  output <- avg_slopes(model) %>% 
    as_tibble() %>% 
    mutate(species = paste(group)) %>% 
    select(species, term, estimate, conf.low, conf.high)
  
  return(output)
  
}




