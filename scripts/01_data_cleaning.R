#------------------------------------------------#
####               Setting up                 ####
#------------------------------------------------#

# Set working directory
setwd("Desktop/Schoodic_Analyst/SeaWatch_data")

# Packages
library(tidyverse)
library(lubridate)
source("scripts/SeaWatch.R")


#------------------------------------------------#
####              Do the Work                 ####
#------------------------------------------------#

# Need to read in and format data for function
fulldata <- tibble(read.csv("data/2021 SeaWatch Daily Migration - Daily Seabird Count.csv", skip = 1)) %>% 
  select(-"Running.Season.Totals") %>% 
  filter(X != "") %>% 
  filter(row_number() < which(X == "Total")) %>% 
  rename(X11.21.21 = X11.21)


# Start function here
envar <- fulldata %>% 
  filter(row_number() <= which(X == "Visibility")) %>% 
  mutate_all(as.character) %>% 
  rename(var = X) %>% 
  pivot_longer(2:length(.), #will need to automate columns here
               names_to = "date",
               values_to = "stat") %>% 
  mutate(var = tolower(var),
         var = str_replace_all(var, " ", "."))

input <- envar %>% 
  distinct(var) %>% 
  as.list() %>% 
  unlist()

envar_output <- map_dfc(input, envar_clean) %>% 
  select(-c(`date...3`, `date...5`, `date...7`, `date...9`)) %>% 
  rename(date = `date...1`) %>% 
  mutate_all(na_if, "") %>% 
  mutate(minutes.of.observation = ifelse(is.na(minutes.of.observation), "0", minutes.of.observation),
         minutes.of.observation = ifelse(minutes.of.observation == "No Coverage", "0", minutes.of.observation)) %>%
  rename(total.obs.mins = minutes.of.observation,
         wind.dir.speed = `general.wind.dir/velocity`,
         time.of.obs = `time(s).of.observation`)

bird_data <- fulldata %>% 
  filter(row_number() > which(X == "Visibility")) %>% 
  rename(species = X) %>% 
  mutate_all(as.character) %>% 
  pivot_longer(2:length(.),
               names_to = "date",
               values_to = "day.total") %>% 
  mutate(date = str_remove(date, "X"),
         date = str_replace_all(date, "\\.", "-"),
         date = mdy(date),
         day.total = as.numeric(day.total),
         day.total = ifelse(is.na(day.total), 0, day.total)) %>% 
  arrange(date, species)

cleaned_data <- right_join(bird_data, envar_output, by = "date")

extra_cols <- data.frame(hour.1.total = rep("", times = length(cleaned_data$species)),
                         hour.2.total = rep("", times = length(cleaned_data$species)),
                         hour.3.total = rep("", times = length(cleaned_data$species)),
                         hour.4.total = rep("", times = length(cleaned_data$species)),
                         hour.5.total = rep("", times = length(cleaned_data$species)),
                         start.time.am = rep("", times = length(cleaned_data$species)),
                         start.time.pm = rep("", times = length(cleaned_data$species)),
                         wind.dir = rep("", times = length(cleaned_data$species)),
                         wind.speed = rep("", times = length(cleaned_data$species)))

all_cols_cleaned <- cleaned_data %>% 
  cbind(extra_cols) %>% 
  as_tibble() %>% 
  mutate(total.obs.mins = as.numeric(total.obs.mins)) %>% 
  select(species, date, hour.1.total, hour.2.total, hour.3.total, hour.4.total, hour.5.total,
         day.total, total.obs.mins, time.of.obs, start.time.am, start.time.pm, principle.observer, 
         wind.dir.speed, wind.dir, wind.speed, visibility) %>% 
  write.csv("outputs/example_cleaned_data.csv")










