#------------------------------------------------#
####           Packages and Source            ####
#------------------------------------------------#

## Packages
library(tidyverse)
library(lubridate)


## Source custom functions
source("scripts/sea_watch_functions.R")




#------------------------------------------------#
####                  2014                    ####
#------------------------------------------------#

### Environmental Variables
## Read in and format data for env. variable function
fulldata2014 <- tibble(read.csv("data/SeaWatch_2014_data.csv", skip = 1)) %>% 
  select(-TOTALS) %>% 
  filter(X != "")


## Filter to the env. variables from the full data and clean
envar2014 <- fulldata2014 %>% 
  filter(row_number() <= which(X == "Visitors")) %>% 
  mutate_all(as.character) %>% 
  rename(var = X) %>% 
  filter(var != "Visitors" & var != "Temp.") %>% 
  pivot_longer(2:length(.),
               names_to = "date",
               values_to = "stat") %>%                  # Pivot longer to get dates in a column
  mutate(var = tolower(var),
         var = str_replace_all(var, " ", ".")) %>% 
  pivot_wider(names_from = var, values_from = stat) %>% # Pivot wider to get env. vars as columns
  rename(principle.observer = observer,
         wind.dir.speed = wind,
         total.obs.mins = minutes.of.observation,
         time.of.obs = time) %>% 
  mutate(visibility = NA_character_,
         date = str_remove(date, "X"),                  # Fixing formatting issues and messy values
         date = str_replace_all(date, "\\.", "-"),
         date = mdy(date),
         total.obs.mins = ifelse(is.na(total.obs.mins), "0", total.obs.mins),
         total.obs.mins = ifelse(total.obs.mins == "No Coverage", "0", total.obs.mins),
         total.obs.mins = ifelse(total.obs.mins  == "", "0", total.obs.mins),
         total.obs.mins = as.integer(total.obs.mins),
         time.of.obs = ifelse(time.of.obs == "", NA_character_, time.of.obs),
         principle.observer = ifelse(principle.observer == "", NA_character_, principle.observer)) %>% 
  select(date, total.obs.mins, time.of.obs, principle.observer, wind.dir.speed, visibility)


## Checking for errors in env. variables data
## Observation minutes
envar2014 %>% 
  distinct(total.obs.mins) %>% 
  print(n = nrow(.))

## Date
envar2014 %>% 
  select(date) %>% 
  group_by(date) %>% 
  count(.) %>% 
  arrange(n) %>% 
  print(n = nrow(.))



#------------------------------------------------#

### Bird count data
## Now deal with the birds count data
bird_data2014 <- fulldata2014 %>% 
  filter(row_number() > which(X == "Visitors")) %>%
  rename(species = X) %>% 
  mutate_all(as.character) %>% 
  pivot_longer(2:length(.),
               names_to = "date",
               values_to = "day.total") %>% 
  mutate(date = str_remove(date, "X"),
         date = str_replace_all(date, "\\.", "-"),
         date = mdy(date),
         day.total = as.double(day.total),
         day.total = ifelse(is.na(day.total), 0, day.total),
         species = ifelse(species == "COGE", "COGO", species),
         species = ifelse(species == "Caspian Tern", "CATE", species),
         species = ifelse(species == "Merlin", "MERL", species),
         species = ifelse(species == "GBH", "GBHE", species),
         species = ifelse(species == "Peeps", "Calidris sp.", species),
         species = ifelse(species == "Whimbrel", "WHIM", species),
         species = ifelse(species == "UNBI", "Bird sp.", species),
         species = ifelse(species == "BLDU", "ABDU", species),
         species = ifelse(species == "Swamp Sp.", "SWSP", species),
         species = ifelse(species == "N. Pintail", "NOPI", species),
         species = ifelse(species == "Bonaparte’s Gull", "BOGU", species),
         species = ifelse(species == "Yellow-br. Chat", "YBCH", species),
         species = ifelse(species == "Co. Grackle", "COGR", species),
         species = ifelse(species == "B. Oriole", "BAOR", species),
         species = ifelse(species == "Snow Bunting", "SNBU", species),
         species = ifelse(species == "Harlequin", "HADU", species),
         species = ifelse(species == "L. Scaup", "LESC", species),
         species = ifelse(species == "Tree Sparrow", "ATSP", species),
         species = ifelse(species == "Am. Crow", "AMCR", species)) %>% 
  arrange(date, species) %>% 
  filter(species != "Monarch")


## Checking for errors in the bird data
## Date
bird_data2014 %>% 
  select(date) %>% 
  group_by(date) %>% 
  count(.) %>% 
  arrange(n) %>% 
  print(n = nrow(.))

## Species
bird_data2014 %>% 
  select(species) %>% 
  group_by(species) %>% 
  count(.) %>% 
  arrange(n) %>% 
  print(n = nrow(.))

## Day totals
bird_data2014 %>% 
  distinct(day.total) %>% 
  arrange(day.total) %>% 
  print(n = nrow(.))


## Combine the cleaned bird count data with the cleaned env. variables
cleaned_data2014 <- left_join(bird_data2014, envar2014, by = "date")


## Export the cleaned years data
# write.csv(cleaned_data2014, "data/yearly_cleaned_data/sea_watch_2014_data.csv", row.names = F)




#------------------------------------------------#
####                  2015                    ####
#------------------------------------------------#

### Environmental Variables
## Read in and format data for env. variable function
fulldata2015 <- tibble(read.csv("data/SeaWatch_2015_data.csv")) %>% 
  select(-Species.Totals) %>% 
  rename(X = Date) %>% 
  filter(X != "")


## Filter to the env. variables from the full data and clean
envar2015 <- fulldata2015 %>% 
  filter(row_number() <= which(X == "principle.observer")) %>% 
  mutate_all(as.character) %>% 
  rename(var = X) %>% 
  pivot_longer(2:length(.),
               names_to = "date",
               values_to = "stat") %>%                  # Pivot longer to get dates in a column
  mutate(var = tolower(var),
         var = str_replace_all(var, " ", ".")) %>% 
  pivot_wider(names_from = var, values_from = stat) %>% # Pivot wider to get env. vars as columns
  rename(total.obs.mins = minutes.of.observation,
         time.of.obs = `time(s).of.observation`) %>% 
  mutate(wind.dir.speed = NA_character_,
         visibility = NA_character_,
         date = str_remove(date, "X"),                  # Fixing formatting issues and messy values
         date = str_replace_all(date, "\\.", "-"),
         date = mdy(date),
         total.obs.mins = ifelse(is.na(total.obs.mins), "0", total.obs.mins),
         total.obs.mins = ifelse(total.obs.mins == "No Coverage", "0", total.obs.mins),
         total.obs.mins = ifelse(total.obs.mins  == "", "0", total.obs.mins),
         total.obs.mins = as.integer(total.obs.mins),
         time.of.obs = ifelse(time.of.obs == "", NA_character_, time.of.obs),
         principle.observer = ifelse(principle.observer == "", NA_character_, principle.observer))


## Checking for errors in env. variables data
## Observation minutes
envar2015 %>% 
  distinct(total.obs.mins) %>% 
  print(n = nrow(.))

## Date
envar2015 %>% 
  select(date) %>% 
  group_by(date) %>% 
  count(.) %>% 
  arrange(n) %>% 
  print(n = nrow(.))



#------------------------------------------------#

### Bird count data
## Now deal with the birds count data
bird_data2015 <- fulldata2015 %>% 
  filter(row_number() > which(X == "principle.observer")) %>% 
  filter(row_number() < which(X == "Daily Totals")) %>%
  rename(species = X) %>% 
  mutate_all(as.character) %>% 
  pivot_longer(2:length(.),
               names_to = "date",
               values_to = "day.total") %>% 
  mutate(date = str_remove(date, "X"),
         date = str_replace_all(date, "\\.", "-"),
         date = mdy(date),
         day.total = as.double(day.total),
         day.total = ifelse(is.na(day.total), 0, day.total)) %>% 
  arrange(date, species)


## Checking for errors in the bird data
## Date
bird_data2015 %>% 
  select(date) %>% 
  group_by(date) %>% 
  count(.) %>% 
  arrange(n) %>% 
  print(n = nrow(.))

## Species
bird_data2015 %>% 
  select(species) %>% 
  group_by(species) %>% 
  count(.) %>% 
  arrange(n) %>% 
  print(n = nrow(.))

## Day totals
bird_data2015 %>% 
  distinct(day.total) %>% 
  arrange(day.total) %>% 
  print(n = nrow(.))


## Combine the cleaned bird count data with the cleaned env. variables
cleaned_data2015 <- left_join(bird_data2015, envar2015, by = "date")


## Export the cleaned years data
# write.csv(cleaned_data2015, "data/yearly_cleaned_data/sea_watch_2015_data.csv", row.names = F)




#------------------------------------------------#
####                  2016                    ####
#------------------------------------------------#

### Environmental Variables
## Read in and format data for env. variable function
fulldata2016 <- tibble(read.csv("data/SeaWatch_2016_data.csv")) %>% 
  select(-Species.Totals) %>% 
  rename(X = Date) %>% 
  filter(X != "")


## Filter to the env. variables from the full data and clean
envar2016 <- fulldata2016 %>% 
  filter(row_number() <= which(X == "General Wind Dir/Velocity")) %>% 
  mutate_all(as.character) %>% 
  rename(var = X) %>% 
  pivot_longer(2:length(.),
               names_to = "date",
               values_to = "stat") %>%                  # Pivot longer to get dates in a column
  mutate(var = tolower(var),
         var = str_replace_all(var, " ", ".")) %>% 
  pivot_wider(names_from = var, values_from = stat) %>% # Pivot wider to get env. vars as columns
  rename(total.obs.mins = minutes.of.observation,
         wind.dir.speed = `general.wind.dir/velocity`,
         time.of.obs = `time(s).of.observation`) %>% 
  mutate(visibility = NA_character_,
         date = str_remove(date, "X"),                  # Fixing formatting issues and messy values
         date = str_replace_all(date, "\\.", "-"),
         date = mdy(date),
         total.obs.mins = ifelse(is.na(total.obs.mins), "0", total.obs.mins),
         total.obs.mins = ifelse(total.obs.mins == "No Coverage", "0", total.obs.mins),
         total.obs.mins = ifelse(total.obs.mins  == "", "0", total.obs.mins),
         total.obs.mins = as.integer(total.obs.mins),
         time.of.obs = ifelse(time.of.obs == "", NA_character_, time.of.obs),
         to2 = ifelse(to2 == "", NA_character_, to2),
         time.of.obs = paste0(time.of.obs, "; ", to2),
         time.of.obs = ifelse(time.of.obs == "NA; NA", NA_character_, time.of.obs),
         time.of.obs = str_remove(time.of.obs, "NA; "),
         time.of.obs = str_remove(time.of.obs, "; NA"),
         time.of.obs = ifelse(time.of.obs == '"', NA_character_, time.of.obs),
         principle.observer = ifelse(principle.observer == "", NA_character_, principle.observer),
         wind.dir.speed = ifelse(wind.dir.speed == "", NA_character_, wind.dir.speed)) %>% 
  select(-to2)


## Checking for errors in env. variables data
## Observation minutes
envar2016 %>% 
  distinct(total.obs.mins) %>% 
  print(n = nrow(.))

## Date
envar2016 %>% 
  select(date) %>% 
  group_by(date) %>% 
  count(.) %>% 
  arrange(n) %>% 
  print(n = nrow(.))



#------------------------------------------------#

### Bird count data
## Now deal with the birds count data
bird_data2016 <- fulldata2016 %>% 
  filter(row_number() > which(X == "General Wind Dir/Velocity")) %>% 
  filter(row_number() < which(X == "Season Total")) %>%
  rename(species = X) %>% 
  mutate_all(as.character) %>% 
  pivot_longer(2:length(.),
               names_to = "date",
               values_to = "day.total") %>% 
  mutate(date = str_remove(date, "X"),
         date = str_replace_all(date, "\\.", "-"),
         date = mdy(date),
         day.total = as.double(day.total),
         day.total = ifelse(is.na(day.total), 0, day.total),
         species = ifelse(species == "Tree Sparrow", "ATSP", species),) %>% 
  arrange(date, species)


## Checking for errors in the bird data
## Date
bird_data2016 %>% 
  select(date) %>% 
  group_by(date) %>% 
  count(.) %>% 
  arrange(n) %>% 
  print(n = nrow(.))

## Species
bird_data2016 %>% 
  select(species) %>% 
  group_by(species) %>% 
  count(.) %>% 
  arrange(n) %>% 
  print(n = nrow(.))

## Day totals
bird_data2016 %>% 
  distinct(day.total) %>% 
  arrange(day.total) %>% 
  print(n = nrow(.))


## Combine the cleaned bird count data with the cleaned env. variables
cleaned_data2016 <- left_join(bird_data2016, envar2016, by = "date")


## Export the cleaned years data
# write.csv(cleaned_data2016, "data/yearly_cleaned_data/sea_watch_2016_data.csv", row.names = F)




#------------------------------------------------#
####                  2017                    ####
#------------------------------------------------#

### Environmental Variables
## Read in and format data for env. variable function
fulldata2017 <- tibble(read.csv("data/SeaWatch_2017_data.csv")) %>% 
  select(-Species.Totals) %>% 
  rename(X = Date) %>% 
  filter(X != "")


## Filter to the env. variables from the full data and clean
envar2017 <- fulldata2017 %>% 
  filter(row_number() <= which(X == "General Wind Dir/Velocity")) %>% 
  mutate_all(as.character) %>% 
  rename(var = X) %>% 
  pivot_longer(2:length(.),
               names_to = "date",
               values_to = "stat") %>%                  # Pivot longer to get dates in a column
  mutate(var = tolower(var),
         var = str_replace_all(var, " ", ".")) %>% 
  pivot_wider(names_from = var, values_from = stat) %>% # Pivot wider to get env. vars as columns
  rename(total.obs.mins = minutes.of.observation,
         wind.dir.speed = `general.wind.dir/velocity`,
         time.of.obs = `time(s).of.observation`) %>% 
  mutate(visibility = NA_character_,
         date = str_remove(date, "X"),                  # Fixing formatting issues and messy values
         date = str_replace_all(date, "\\.", "-"),
         date = mdy(date),
         total.obs.mins = ifelse(is.na(total.obs.mins), "0", total.obs.mins),
         total.obs.mins = ifelse(total.obs.mins == "No Coverage", "0", total.obs.mins),
         total.obs.mins = ifelse(total.obs.mins  == "", "0", total.obs.mins),
         total.obs.mins = as.integer(total.obs.mins),
         time.of.obs = ifelse(time.of.obs == "", NA_character_, time.of.obs),
         principle.observer = ifelse(principle.observer == "", NA_character_, principle.observer),
         wind.dir.speed = ifelse(wind.dir.speed == "", NA_character_, wind.dir.speed))


## Checking for errors in env. variables data
## Observation minutes
envar2017 %>% 
  distinct(total.obs.mins) %>% 
  print(n = nrow(.))

## Date
envar2017 %>% 
  select(date) %>% 
  group_by(date) %>% 
  count(.) %>% 
  arrange(n) %>% 
  print(n = nrow(.))



#------------------------------------------------#

### Bird count data
## Now deal with the birds count data
bird_data2017 <- fulldata2017 %>% 
  filter(row_number() > which(X == "General Wind Dir/Velocity")) %>% 
  filter(row_number() < which(X == "Season Totals")) %>%
  rename(species = X) %>% 
  mutate_all(as.character) %>% 
  pivot_longer(2:length(.),
               names_to = "date",
               values_to = "day.total") %>% 
  mutate(date = str_remove(date, "X"),
         date = str_replace_all(date, "\\.", "-"),
         date = mdy(date),
         day.total = as.double(day.total),
         day.total = ifelse(is.na(day.total), 0, day.total)) %>% 
  arrange(date, species)


## Checking for errors in the bird data
## Date
bird_data2017 %>% 
  select(date) %>% 
  group_by(date) %>% 
  count(.) %>% 
  arrange(n) %>% 
  print(n = nrow(.))

## Species
bird_data2017 %>% 
  select(species) %>% 
  group_by(species) %>% 
  count(.) %>% 
  arrange(n) %>% 
  print(n = nrow(.))

## Day totals
bird_data2017 %>% 
  distinct(day.total) %>% 
  arrange(day.total) %>% 
  print(n = nrow(.))


## Combine the cleaned bird count data with the cleaned env. variables
cleaned_data2017 <- left_join(bird_data2017, envar2017, by = "date")


## Export the cleaned years data
# write.csv(cleaned_data2017, "data/yearly_cleaned_data/sea_watch_2017_data.csv", row.names = F)




#------------------------------------------------#
####                  2018                    ####
#------------------------------------------------#

### Environmental Variables
## Read in and format data for env. variable function
fulldata2018 <- tibble(read.csv("data/SeaWatch_2018_data.csv")) %>% 
  select(-X) %>% 
  rename(X = Date) %>% 
  filter(X != "")


## Filter to the env. variables from the full data and clean
envar2018 <- fulldata2018 %>% 
  filter(row_number() <= which(X == "Visibility")) %>% 
  mutate_all(as.character) %>% 
  rename(var = X) %>% 
  pivot_longer(2:length(.),
               names_to = "date",
               values_to = "stat") %>%                  # Pivot longer to get dates in a column
  mutate(var = tolower(var),
         var = str_replace_all(var, " ", ".")) %>% 
  pivot_wider(names_from = var, values_from = stat) %>% # Pivot wider to get env. vars as columns
  rename(total.obs.mins = minutes.of.observation,
         wind.dir.speed = `general.wind.dir/velocity`,
         time.of.obs = `time(s).of.observation`) %>% 
  mutate(date = str_remove(date, "X"),                  # Fixing formatting issues and messy values
         date = str_replace_all(date, "\\.", "-"),
         date = mdy(date),
         total.obs.mins = ifelse(is.na(total.obs.mins), "0", total.obs.mins),
         total.obs.mins = ifelse(total.obs.mins == "No Coverage", "0", total.obs.mins),
         total.obs.mins = ifelse(total.obs.mins  == "", "0", total.obs.mins),
         total.obs.mins = as.integer(total.obs.mins),
         time.of.obs = ifelse(time.of.obs == "", NA_character_, time.of.obs),
         principle.observer = ifelse(principle.observer == "", NA_character_, principle.observer),
         wind.dir.speed = ifelse(wind.dir.speed == "", NA_character_, wind.dir.speed),
         visibility = ifelse(visibility == "", NA_character_, visibility))


## Checking for errors in env. variables data
## Observation minutes
envar2018 %>% 
  distinct(total.obs.mins) %>% 
  print(n = nrow(.))

## Date
envar2018 %>% 
  select(date) %>% 
  group_by(date) %>% 
  count(.) %>% 
  arrange(n) %>% 
  print(n = nrow(.))



#------------------------------------------------#

### Bird count data
## Now deal with the birds count data
bird_data2018 <- fulldata2018 %>% 
  filter(row_number() > which(X == "Visibility")) %>% 
  filter(row_number() < which(X == "Total")) %>%
  rename(species = X) %>% 
  mutate_all(as.character) %>% 
  pivot_longer(2:length(.),
               names_to = "date",
               values_to = "day.total") %>% 
  mutate(date = str_remove(date, "X"),
         date = str_replace_all(date, "\\.", "-"),
         date = mdy(date),
         day.total = as.double(day.total),
         day.total = ifelse(is.na(day.total), 0, day.total),
         species = ifelse(species == "CO sp.", "Cormorant sp.", species),
         species = ifelse(species == "LO sp.", "Loon sp.", species),
         species = ifelse(species == "Great Shearwater", "GRSH", species),
         species = ifelse(species == "UNBI", "Bird sp.", species),
         species = ifelse(species == "SC sp.", "Scoter sp.", species)) %>% 
  arrange(date, species)


## Checking for errors in the bird data
## Date
bird_data2018 %>% 
  select(date) %>% 
  group_by(date) %>% 
  count(.) %>% 
  arrange(n) %>% 
  print(n = nrow(.))

## Species
bird_data2018 %>% 
  select(species) %>% 
  group_by(species) %>% 
  count(.) %>% 
  arrange(n) %>% 
  print(n = nrow(.))

## Day totals
bird_data2018 %>% 
  distinct(day.total) %>% 
  arrange(day.total) %>% 
  print(n = nrow(.))


## Combine the cleaned bird count data with the cleaned env. variables
cleaned_data2018 <- left_join(bird_data2018, envar2018, by = "date")


## Export the cleaned years data
# write.csv(cleaned_data2018, "data/yearly_cleaned_data/sea_watch_2018_data.csv", row.names = F)




#------------------------------------------------#
####                  2019                    ####
#------------------------------------------------#

### Environmental Variables
## Read in and format data for env. variable function
fulldata2019 <- tibble(read.csv("data/SeaWatch_2019_data.csv")) %>% 
  select(-X) %>% 
  rename(X = Date) %>% 
  filter(X != "")


## Filter to the env. variables from the full data and clean
envar2019 <- fulldata2019 %>% 
  filter(row_number() <= which(X == "Visibility")) %>% 
  mutate_all(as.character) %>% 
  rename(var = X) %>% 
  pivot_longer(2:length(.),
               names_to = "date",
               values_to = "stat") %>%                  # Pivot longer to get dates in a column
  mutate(var = tolower(var),
         var = str_replace_all(var, " ", ".")) %>% 
  pivot_wider(names_from = var, values_from = stat) %>% # Pivot wider to get env. vars as columns
  rename(total.obs.mins = minutes.of.observation,
         wind.dir.speed = `general.wind.dir/velocity`,
         time.of.obs = `time(s).of.observation`) %>% 
  mutate(date = str_remove(date, "X"),                  # Fixing formatting issues and messy values
         date = str_replace_all(date, "\\.", "-"),
         date = mdy(date),
         total.obs.mins = ifelse(is.na(total.obs.mins), "0", total.obs.mins),
         total.obs.mins = ifelse(total.obs.mins == "No Coverage", "0", total.obs.mins),
         total.obs.mins = ifelse(total.obs.mins  == "", "0", total.obs.mins),
         total.obs.mins = as.integer(total.obs.mins),
         time.of.obs = ifelse(time.of.obs == "", NA_character_, time.of.obs),
         time.of.obs = ifelse(time.of.obs  == "count not completed because of hurriane Dorian", NA_character_, time.of.obs),
         principle.observer = ifelse(principle.observer == "", NA_character_, principle.observer),
         wind.dir.speed = ifelse(wind.dir.speed == "", NA_character_, wind.dir.speed),
         visibility = ifelse(visibility == "", NA_character_, visibility))


## Checking for errors in env. variables data
## Observation minutes
envar2019 %>% 
  distinct(total.obs.mins) %>% 
  print(n = nrow(.))

## Date
envar2019 %>% 
  select(date) %>% 
  group_by(date) %>% 
  count(.) %>% 
  arrange(n) %>% 
  print(n = nrow(.))



#------------------------------------------------#

### Bird count data
## Now deal with the birds count data
bird_data2019 <- fulldata2019 %>% 
  filter(row_number() > which(X == "Visibility")) %>% 
  filter(row_number() < which(X == "Total")) %>%
  rename(species = X) %>% 
  mutate_all(as.character) %>% 
  pivot_longer(2:length(.),
               names_to = "date",
               values_to = "day.total") %>% 
  mutate(date = str_remove(date, "X"),
         date = str_replace_all(date, "\\.", "-"),
         date = mdy(date),
         day.total = as.double(day.total),
         day.total = ifelse(is.na(day.total), 0, day.total),
         species = ifelse(species == "CO sp.", "Cormorant sp.", species),
         species = ifelse(species == "LO sp.", "Loon sp.", species),
         species = ifelse(species == "Great Shearwater", "GRSH", species),
         species = ifelse(species == "UNBI", "Bird sp.", species),
         species = ifelse(species == "SC sp.", "Scoter sp.", species)) %>% 
  arrange(date, species)


## Checking for errors in the bird data
## Date
bird_data2019 %>% 
  select(date) %>% 
  group_by(date) %>% 
  count(.) %>% 
  arrange(n) %>% 
  print(n = nrow(.))

## Species
bird_data2019 %>% 
  select(species) %>% 
  group_by(species) %>% 
  count(.) %>% 
  arrange(n) %>% 
  print(n = nrow(.))

## Day totals
bird_data2019 %>% 
  distinct(day.total) %>% 
  arrange(day.total) %>% 
  print(n = nrow(.))


## Combine the cleaned bird count data with the cleaned env. variables
cleaned_data2019 <- left_join(bird_data2019, envar2019, by = "date")


## Export the cleaned years data
# write.csv(cleaned_data2019, "data/yearly_cleaned_data/sea_watch_2019_data.csv", row.names = F)




#------------------------------------------------#
####                  2020                    ####
#------------------------------------------------#

### Environmental Variables
## Read in and format data for env. variable function
fulldata2020 <- tibble(read.csv("data/SeaWatch_2020_data.csv")) %>% 
  select(-c(X, `X09.11.20.1`)) %>% 
  rename(X = Date) %>% 
  filter(X != "")


## Filter to the env. variables from the full data and clean
envar2020 <- fulldata2020 %>% 
  filter(row_number() <= which(X == "Visibility")) %>% 
  mutate_all(as.character) %>% 
  rename(var = X) %>% 
  pivot_longer(2:length(.),
               names_to = "date",
               values_to = "stat") %>%                  # Pivot longer to get dates in a column
  mutate(var = tolower(var),
         var = str_replace_all(var, " ", ".")) %>% 
  pivot_wider(names_from = var, values_from = stat) %>% # Pivot wider to get env. vars as columns
  rename(total.obs.mins = minutes.of.observation,
         wind.dir.speed = `general.wind.dir/velocity`,
         time.of.obs = `time(s).of.observation`) %>% 
  mutate(date = str_remove(date, "X"),                  # Fixing formating issues and messy values
         date = str_replace_all(date, "\\.", "-"),
         date = mdy(date),
         total.obs.mins = ifelse(is.na(total.obs.mins), "0", total.obs.mins),
         total.obs.mins = ifelse(total.obs.mins == "No Coverage", "0", total.obs.mins),
         total.obs.mins = ifelse(total.obs.mins  == "", "0", total.obs.mins),
         total.obs.mins = as.integer(total.obs.mins),
         time.of.obs = ifelse(time.of.obs == "", NA_character_, time.of.obs),
         wind.dir.speed = ifelse(wind.dir.speed == "", NA_character_, wind.dir.speed),
         visibility = ifelse(visibility == "", NA_character_, visibility))


## Checking for errors in env. variables data
## Observation minutes
envar2020 %>% 
  distinct(total.obs.mins) %>% 
  print(n = nrow(.))

## Date
envar2020 %>% 
  select(date) %>% 
  group_by(date) %>% 
  count(.) %>% 
  arrange(n) %>% 
  print(n = nrow(.))



#------------------------------------------------#

### Bird count data
## Now deal with the birds count data
bird_data2020 <- fulldata2020 %>% 
  filter(row_number() > which(X == "Visibility")) %>% 
  filter(row_number() < which(X == "Total")) %>%
  rename(species = X) %>% 
  mutate_all(as.character) %>% 
  pivot_longer(2:length(.),
               names_to = "date",
               values_to = "day.total") %>% 
  mutate(date = str_remove(date, "X"),
         date = str_replace_all(date, "\\.", "-"),
         date = mdy(date),
         day.total = as.double(day.total),
         day.total = ifelse(is.na(day.total), 0, day.total),
         species = ifelse(species == "CO sp.", "Cormorant sp.", species),
         species = ifelse(species == "LO sp.", "Loon sp.", species),
         species = ifelse(species == "Great Shearwater", "GRSH", species),
         species = ifelse(species == "UNBI", "Bird sp.", species),
         species = ifelse(species == "SC sp.", "Scoter sp.", species)) %>% 
  arrange(date, species)


## Checking for errors in the bird data
## Date
bird_data2020 %>% 
  select(date) %>% 
  group_by(date) %>% 
  count(.) %>% 
  arrange(n) %>% 
  print(n = nrow(.))

## Species
bird_data2020 %>% 
  select(species) %>% 
  group_by(species) %>% 
  count(.) %>% 
  arrange(n) %>% 
  print(n = nrow(.))

## Day totals
bird_data2020 %>% 
  distinct(day.total) %>% 
  arrange(day.total) %>% 
  print(n = nrow(.))


## Combine the cleaned bird count data with the cleaned env. variables
cleaned_data2020 <- left_join(bird_data2020, envar2020, by = "date")


## Export the cleaned years data
# write.csv(cleaned_data2020, "data/yearly_cleaned_data/sea_watch_2020_data.csv", row.names = F)




#------------------------------------------------#
####                  2021                    ####
#------------------------------------------------#

### Environmental Variables
## Read in and format data for env. variable function
fulldata2021 <- tibble(read.csv("data/SeaWatch_2021_data.csv", skip = 1)) %>%
  select(-`Running.Season.Totals`) %>% 
  rename(X11.21.21 = X11.21) %>% 
  filter(X != "")


## Filter to the env. variables from the full data and clean
envar2021 <- fulldata2021 %>% 
  filter(row_number() <= which(X == "Visibility")) %>% 
  mutate_all(as.character) %>% 
  rename(var = X) %>% 
  pivot_longer(2:length(.),
               names_to = "date",
               values_to = "stat") %>%                  # Pivot longer to get dates in a column
  mutate(var = tolower(var),
         var = str_replace_all(var, " ", ".")) %>% 
  pivot_wider(names_from = var, values_from = stat) %>% # Pivot wider to get env. vars as columns
  rename(total.obs.mins = minutes.of.observation,
         wind.dir.speed = `general.wind.dir/velocity`,
         time.of.obs = `time(s).of.observation`) %>% 
  mutate(date = str_remove(date, "X"),                  # Fixing formating issues and messy values
         date = str_replace_all(date, "\\.", "-"),
         date =  ifelse(date == "10-16-12", "10-16-21", date),
         date = mdy(date),
         total.obs.mins = ifelse(is.na(total.obs.mins), "0", total.obs.mins),
         total.obs.mins = ifelse(total.obs.mins == "No Coverage", "0", total.obs.mins),
         total.obs.mins = ifelse(total.obs.mins  == "", "0", total.obs.mins),
         total.obs.mins = as.integer(total.obs.mins),
         time.of.obs = ifelse(time.of.obs == "", NA_character_, time.of.obs),
         wind.dir.speed = ifelse(wind.dir.speed == "", NA_character_, wind.dir.speed),
         visibility = ifelse(visibility == "", NA_character_, visibility))


## Checking for errors in env. variables data
## Observation minutes
envar2021 %>% 
  distinct(total.obs.mins) %>% 
  print(n = nrow(.))

## Date
envar2021 %>% 
  select(date) %>% 
  group_by(date) %>% 
  count(.) %>% 
  arrange(n) %>% 
  print(n = nrow(.))



#------------------------------------------------#

### Bird count data
## Now deal with the birds count data
bird_data2021 <- fulldata2021 %>% 
  filter(row_number() > which(X == "Visibility")) %>% 
  filter(row_number() < which(X == "Total")) %>%
  rename(species = X) %>% 
  mutate_all(as.character) %>% 
  pivot_longer(2:length(.),
               names_to = "date",
               values_to = "day.total") %>% 
  mutate(date = str_remove(date, "X"),
         date = str_replace_all(date, "\\.", "-"),
         date =  ifelse(date == "10-16-12", "10-16-21", date),
         date = mdy(date),
         day.total = as.integer(day.total),
         day.total = ifelse(is.na(day.total), 0, day.total),
         species = ifelse(species == "CO sp.", "Cormorant sp.", species),
         species = ifelse(species == "LO sp.", "Loon sp.", species),
         species = ifelse(species == "Great Shearwater", "GRSH", species),
         species = ifelse(species == "UNBI", "Bird sp.", species),
         species = ifelse(species == "SC sp.", "Scoter sp.", species)) %>% 
  arrange(date, species)


## Checking for errors in the bird data
## Date
bird_data2021 %>% 
  select(date) %>% 
  group_by(date) %>% 
  count(.) %>% 
  arrange(n) %>% 
  print(n = nrow(.))

## Species
bird_data2021 %>% 
  select(species) %>% 
  group_by(species) %>% 
  count(.) %>% 
  arrange(n) %>% 
  print(n = nrow(.))

## Day totals
bird_data2021 %>% 
  distinct(day.total) %>% 
  arrange(day.total) %>% 
  print(n = nrow(.))


## Combine the cleaned bird count data with the cleaned env. variables
cleaned_data2021 <- left_join(bird_data2021, envar2021, by = "date")


## Export the cleaned years data
# write.csv(cleaned_data2021, "data/yearly_cleaned_data/sea_watch_2021_data.csv", row.names = F)




#------------------------------------------------#
####                  2022                    ####
#------------------------------------------------#

### Environmental Variables
## Read in and format data for env. variable function
fulldata2022 <- tibble(read.csv("data/SeaWatch_2022_data.csv", skip = 1)) %>%
  select(-`Running.Season.Totals`) %>% 
  # rename(X11.21.21 = X11.21) %>% 
  filter(X != "")


## Filter to the env. variables from the full data and clean
envar2022 <- fulldata2022 %>% 
  filter(row_number() <= which(X == "Visibility")) %>% 
  mutate_all(as.character) %>% 
  rename(var = X) %>% 
  pivot_longer(2:length(.),
               names_to = "date",
               values_to = "stat") %>%                  # Pivot longer to get dates in a column
  mutate(var = tolower(var),
         var = str_replace_all(var, " ", ".")) %>% 
  pivot_wider(names_from = var, values_from = stat) %>% # Pivot wider to get env. vars as columns
  rename(total.obs.mins = minutes.of.observation,
         wind.dir.speed = `general.wind.dir/velocity`,
         time.of.obs = `time(s).of.observation`) %>% 
  mutate(date = str_remove(date, "X"),                  # Fixing formatting issues and messy values
         date = str_replace_all(date, "\\.", "-"),
         # date =  ifelse(date == "10-16-12", "10-16-21", date),
         date = mdy(date),
         total.obs.mins = ifelse(is.na(total.obs.mins), "0", total.obs.mins),
         total.obs.mins = ifelse(total.obs.mins == "No Coverage", "0", total.obs.mins),
         total.obs.mins = ifelse(total.obs.mins == "No Count", "0", total.obs.mins),
         total.obs.mins = ifelse(total.obs.mins  == "", "0", total.obs.mins),
         total.obs.mins = as.integer(total.obs.mins),
         time.of.obs = ifelse(time.of.obs == "", NA_character_, time.of.obs),
         principle.observer = ifelse(principle.observer == "", NA_character_, principle.observer),
         wind.dir.speed = ifelse(wind.dir.speed == "", NA_character_, wind.dir.speed),
         visibility = ifelse(visibility == "", NA_character_, visibility))


## Checking for errors in env. variables data
## Observation minutes
envar2022 %>% 
  distinct(total.obs.mins) %>% 
  print(n = nrow(.))

## Date
envar2022 %>% 
  select(date) %>% 
  group_by(date) %>% 
  count(.) %>% 
  arrange(n) %>% 
  print(n = nrow(.))



#------------------------------------------------#

### Bird count data
## Now deal with the birds count data
bird_data2022 <- fulldata2022 %>% 
  filter(row_number() > which(X == "Visibility")) %>% 
  filter(row_number() < which(X == "Total")) %>%
  rename(species = X) %>% 
  mutate_all(as.character) %>% 
  pivot_longer(2:length(.),
               names_to = "date",
               values_to = "day.total") %>% 
  mutate(date = str_remove(date, "X"),
         date = str_replace_all(date, "\\.", "-"),
         date = mdy(date),
         day.total = as.integer(day.total),
         day.total = ifelse(is.na(day.total), 0, day.total),
         species = ifelse(species == "CO sp.", "Cormorant sp.", species),
         species = ifelse(species == "LO sp.", "Loon sp.", species),
         species = ifelse(species == "Great Shearwater", "GRSH", species),
         species = ifelse(species == "UNBI", "Bird sp.", species),
         species = ifelse(species == "SC sp.", "Scoter sp.", species)) %>% 
  arrange(date, species)


## Checking for errors in the bird data
## Date
bird_data2022 %>% 
  select(date) %>% 
  group_by(date) %>% 
  count(.) %>% 
  arrange(n) %>% 
  print(n = nrow(.))

## Species
bird_data2022 %>% 
  select(species) %>% 
  group_by(species) %>% 
  count(.) %>% 
  arrange(n) %>% 
  print(n = nrow(.))

## Day totals
bird_data2022 %>% 
  distinct(day.total) %>%
  arrange(day.total) %>% 
  print(n = nrow(.))


## Combine the cleaned bird count data with the cleaned env. variables
cleaned_data2022 <- left_join(bird_data2022, envar2022, by = "date")


## Export the cleaned years data
# write.csv(cleaned_data2022, "data/yearly_cleaned_data/sea_watch_2022_data.csv", row.names = F)




#------------------------------------------------#
####                  2023                    ####
#------------------------------------------------#

### Environmental Variables
## Read in and format data for env. variable function
fulldata2023 <- tibble(read.csv("data/SeaWatch_2023_data.csv")) %>%
  select(-X) %>% 
  filter(Date != "")


## Filter to the env. variables from the full data and clean
envar2023 <- fulldata2023 %>% 
  filter(row_number() <= which(Date == "Visibility (miles)")) %>% 
  mutate_all(as.character) %>% 
  rename(var = Date) %>% 
  pivot_longer(2:length(.),
               names_to = "date",
               values_to = "stat") %>%                  # Pivot longer to get dates in a column
  mutate(var = tolower(var),
         var = str_replace_all(var, " ", ".")) %>% 
  pivot_wider(names_from = var, values_from = stat) %>% # Pivot wider to get env. vars as columns
  rename(total.obs.mins = minutes.of.observation,
         wind.dir.speed = `general.wind.dir/velocity`,
         time.of.obs = `time(s).of.observation`,
         visibility = `visibility.(miles)`) %>% 
  mutate(date = str_remove(date, "X"),                  # Fixing formatting issues and messy values
         date = str_replace_all(date, "\\.", "-"),
         date =  ifelse(date == "2026-11-22", "2023-11-22", date),
         date = mdy(date),
         total.obs.mins = ifelse(is.na(total.obs.mins), "0", total.obs.mins),
         total.obs.mins = ifelse(total.obs.mins == "No Coverage", "0", total.obs.mins),
         total.obs.mins = ifelse(total.obs.mins == "No Count", "0", total.obs.mins),
         total.obs.mins = ifelse(total.obs.mins  == "", "0", total.obs.mins),
         total.obs.mins = as.integer(total.obs.mins),
         total.obs.mins = ifelse(is.na(total.obs.mins), 0, total.obs.mins),
         time.of.obs = ifelse(time.of.obs == "", NA_character_, time.of.obs),
         time.of.obs = ifelse(time.of.obs == "No Count", NA_character_, time.of.obs),
         principle.observer = ifelse(principle.observer == "", NA_character_, principle.observer),
         wind.dir.speed = ifelse(wind.dir.speed == "", NA_character_, wind.dir.speed),
         visibility = ifelse(visibility == "", NA_character_, visibility),
         visibility = ifelse(visibility == "No Count", NA_character_, visibility),
         visibility = ifelse(visibility == "No Counts", NA_character_, visibility))


## Checking for errors in env. variables data
## Observation minutes
envar2022 %>% 
  distinct(total.obs.mins) %>% 
  print(n = nrow(.))

## Date
envar2022 %>% 
  select(date) %>% 
  group_by(date) %>% 
  count(.) %>% 
  arrange(n) %>% 
  print(n = nrow(.))



#------------------------------------------------#

### Bird count data
## Now deal with the birds count data
bird_data2023 <- fulldata2023 %>% 
  filter(row_number() > which(Date == "Visibility (miles)")) %>% 
  filter(row_number() < which(Date == "Total")) %>%
  rename(species = Date) %>% 
  mutate_all(as.character) %>% 
  pivot_longer(2:length(.),
               names_to = "date",
               values_to = "day.total") %>% 
  mutate(date = str_remove(date, "X"),
         date = str_replace_all(date, "\\.", "-"),
         date = mdy(date),
         day.total = as.integer(day.total),
         day.total = ifelse(is.na(day.total), 0, day.total),
         species = ifelse(species == "CO sp.", "Cormorant sp.", species),
         species = ifelse(species == "LO sp.", "Loon sp.", species),
         species = ifelse(species == "Great Shearwater", "GRSH", species),
         species = ifelse(species == "UNBI", "Bird sp.", species),
         species = ifelse(species == "SC sp.", "Scoter sp.", species),
         species = ifelse(species == "Duck/Alcid/Loon sp.", "Bird sp.", species),
         species = ifelse(species == "Dunlin", "DUNL", species),
         species = ifelse(species == "Osprey", "OSPR", species),
         species = ifelse(species == "WW Gull", "ICGU/GLGU", species)) %>% 
  arrange(date, species)


## Checking for errors in the bird data
## Date
bird_data2023 %>% 
  select(date) %>% 
  group_by(date) %>% 
  count(.) %>% 
  arrange(n) %>% 
  print(n = nrow(.))

## Species
bird_data2023 %>% 
  select(species) %>% 
  group_by(species) %>% 
  count(.) %>% 
  arrange(n) %>% 
  print(n = nrow(.))

## Day totals
bird_data2023 %>% 
  distinct(day.total) %>%
  arrange(day.total) %>% 
  print(n = nrow(.))


## Combine the cleaned bird count data with the cleaned env. variables
cleaned_data2023 <- left_join(bird_data2023, envar2023, by = "date") %>% 
  mutate(year = as.character(year(date)),
         obs.hours = total.obs.mins/60,
         count.p.hour = day.total/obs.hours,
         principle.observer = ifelse(principle.observer == "SB/BG", "BG/SB", principle.observer),
         principle.observer = ifelse(principle.observer == "ND, BG", "ND/BG", principle.observer),
         principle.observer = ifelse(principle.observer == "ND, SB", "ND/SB", principle.observer),
         principle.observer = ifelse(principle.observer == "SB, BG, ZK", "SB/BG/ZK", principle.observer),
         principle.observer = ifelse(principle.observer == "SB/ND", "ND/SB", principle.observer)) %>% 
  select(species, date, year, time.of.obs, total.obs.mins, obs.hours, 
         day.total, count.p.hour, wind.dir.speed, visibility, principle.observer)


## Export the cleaned years data
# write.csv(cleaned_data2023, "data/yearly_cleaned_data/sea_watch_2023_data.csv", row.names = F)




#------------------------------------------------#
####            All years data set            ####
#------------------------------------------------#

## Creating the full data set with all years of data
complete_data <- bind_rows(cleaned_data2014, cleaned_data2015, cleaned_data2016, cleaned_data2017, 
          cleaned_data2018, cleaned_data2019, cleaned_data2020, cleaned_data2021, 
          cleaned_data2022) %>% 
  mutate(principle.observer = ifelse(principle.observer == "Keith", "KO", principle.observer),
         principle.observer = ifelse(principle.observer == "KO/SB?Nick", "KO/SB", principle.observer),
         principle.observer = ifelse(principle.observer == "Seth", "SB", principle.observer),
         principle.observer = ifelse(principle.observer == " Seth", "SB", principle.observer),
         principle.observer = ifelse(principle.observer == "Andrew Wolfgang", "AW", principle.observer),
         principle.observer = ifelse(principle.observer == "Seth Benz", "SB", principle.observer),
         principle.observer = ifelse(principle.observer == "STB", "SB", principle.observer),
         principle.observer = ifelse(principle.observer == "E. Johnson", "EJ", principle.observer),
         principle.observer = ifelse(principle.observer == "S. Benz", "SB", principle.observer),
         principle.observer = ifelse(principle.observer == "E Johnson", "EJ", principle.observer),
         principle.observer = ifelse(principle.observer == "E Johnson/S Benz", "EJ/SB", principle.observer),
         principle.observer = ifelse(principle.observer == "S Benz", "SB", principle.observer),
         principle.observer = ifelse(principle.observer == "E Johnson/K Yakola", "EJ/KY", principle.observer),
         principle.observer = ifelse(principle.observer == "Isabel Brofsky, Keenan Yakola", "IB/KY", principle.observer),
         principle.observer = ifelse(principle.observer == "E Johnson, Isabel Brofsky, Keenan Yakola", "EJ/IB/KY", principle.observer),
         principle.observer = ifelse(principle.observer == "Hallie Daly", "HD", principle.observer),
         principle.observer = ifelse(principle.observer == "No Observer Available", NA_character_, principle.observer),
         principle.observer = ifelse(principle.observer == "Earl Johnson", "EJ", principle.observer),
         principle.observer = ifelse(principle.observer == "Hallie Daly, Seth Benz", "HD/SB", principle.observer),
         principle.observer = ifelse(principle.observer == "Hallie Daly, Serth Benz", "HD/SB", principle.observer),
         principle.observer = ifelse(principle.observer == "Hallie Daly ", "HD", principle.observer),
         principle.observer = ifelse(principle.observer == "Nathan Dubrow", "ND", principle.observer),
         principle.observer = ifelse(principle.observer == "Benz/Klyver", "SB/ZK", principle.observer),
         principle.observer = ifelse(principle.observer == "ZS,SB", "ZS/SB", principle.observer),
         principle.observer = ifelse(principle.observer == "Ak", "AK", principle.observer),
         principle.observer = ifelse(principle.observer == "", NA_character_, principle.observer),
         principle.observer = ifelse(principle.observer == "ACK", "AK", principle.observer),
         principle.observer = ifelse(principle.observer == "A. Világ", "AV", principle.observer),
         principle.observer = ifelse(principle.observer == "A Világ", "AV", principle.observer),
         principle.observer = ifelse(principle.observer == "Bob D.", "BD", principle.observer),
         principle.observer = ifelse(principle.observer == "RZK ", "RZK", principle.observer),
         principle.observer = ifelse(principle.observer == "SS", "ZS", principle.observer),
         principle.observer = ifelse(principle.observer == "RZK/STB", "RZK/SB", principle.observer)) %>% 
  mutate(
         # start.time = str_extract(time.of.obs, "^(.*?)-"),
         # start.time = str_remove(start.time, "-"),
         # start.time = str_remove(start.time, "Fog."),
         # start.time = str_remove(start.time, "AM, "),
         # start.time = str_remove(start.time, "=930,325"),
         # start.time = str_remove(start.time, "FOG;330"),
         # start.time = str_replace(start.time, ";", ":"),
         # start.time = str_trim(start.time),
         # start.time = ifelse(start.time == "", NA_character_, start.time),
         year = as.character(year(date)),
         obs.hours = total.obs.mins/60,
         count.p.hour = day.total/obs.hours) %>% 
  select(species, date, year, time.of.obs, total.obs.mins, obs.hours, 
         day.total, count.p.hour, wind.dir.speed, visibility, principle.observer)


# write.csv(complete_data, "outputs/sea_watch_cleaned_data.csv", row.names = F)






test <- complete_data %>% 
  filter(species == "BLSC" & total.obs.mins != 0) %>% 
  mutate(year = as.character(year(date)),
         month = month(date),
         day = mday(date),
         plot.date = as.Date(paste0(2022, "-", month, "-", day)),
         obs.hours = total.obs.mins/60,
         cph = day.total/obs.hours) %>% 
  select(-c(month, day))


test %>% 
  ggplot(aes(x = plot.date, y = cph)) +
  geom_smooth()





#------------------------------------------------#
####          Old version for 2021            ####
#------------------------------------------------#

# # Need to read in and format data for function
# fulldata <- tibble(read.csv("data/2021 SeaWatch Daily Migration - Daily Seabird Count.csv", skip = 1)) %>% 
#   select(-"Running.Season.Totals") %>% 
#   filter(X != "") %>% 
#   filter(row_number() < which(X == "Total")) %>% 
#   rename(X11.21.21 = X11.21)
# 
# 
# # Start function here
# envar <- fulldata %>% 
#   filter(row_number() <= which(X == "Visibility")) %>% 
#   mutate_all(as.character) %>% 
#   rename(var = X) %>% 
#   pivot_longer(2:length(.), # will need to automate columns here
#                names_to = "date",
#                values_to = "stat") %>% 
#   mutate(var = tolower(var),
#          var = str_replace_all(var, " ", "."))
# 
# input <- envar %>% 
#   distinct(var) %>% 
#   as.list() %>% 
#   unlist()
# 
# envar_output <- map_dfc(input, envar_clean) %>% 
#   select(-c(`date...3`, `date...5`, `date...7`, `date...9`)) %>% 
#   rename(date = `date...1`) %>% 
#   #mutate_all(na_if, "") %>% 
#   mutate(minutes.of.observation = ifelse(is.na(minutes.of.observation), "0", minutes.of.observation),
#          minutes.of.observation = ifelse(minutes.of.observation == "No Coverage", "0", minutes.of.observation)) %>%
#   rename(total.obs.mins = minutes.of.observation,
#          wind.dir.speed = `general.wind.dir/velocity`,
#          time.of.obs = `time(s).of.observation`)
# 
# bird_data <- fulldata %>% 
#   filter(row_number() > which(X == "Visibility")) %>% 
#   rename(species = X) %>% 
#   mutate_all(as.character) %>% 
#   pivot_longer(2:length(.),
#                names_to = "date",
#                values_to = "day.total") %>% 
#   mutate(date = str_remove(date, "X"),
#          date = str_replace_all(date, "\\.", "-"),
#          date = mdy(date),
#          day.total = as.numeric(day.total),
#          day.total = ifelse(is.na(day.total), 0, day.total),
#          species = ifelse(species == "CO sp.", "Cormorant sp.", species),
#          species = ifelse(species == "LO sp.", "Loon sp.", species),
#          species = ifelse(species == "Great Shearwater", "GRSH", species),
#          species = ifelse(species == "UNBI", "Bird sp.", species),
#          species = ifelse(species == "SC sp.", "Scoter sp.", species)) %>% 
#   arrange(date, species)
# 
# cleaned_data <- right_join(bird_data, envar_output, by = "date")
# 
# extra_cols <- data.frame(hour.1.total = rep("", times = length(cleaned_data$species)),
#                          hour.2.total = rep("", times = length(cleaned_data$species)),
#                          hour.3.total = rep("", times = length(cleaned_data$species)),
#                          hour.4.total = rep("", times = length(cleaned_data$species)),
#                          hour.5.total = rep("", times = length(cleaned_data$species)),
#                          start.time.am = rep("", times = length(cleaned_data$species)),
#                          start.time.pm = rep("", times = length(cleaned_data$species)),
#                          wind.dir = rep("", times = length(cleaned_data$species)),
#                          wind.speed = rep("", times = length(cleaned_data$species)))
# 
# all_cols_cleaned <- cleaned_data %>% 
#   cbind(extra_cols) %>% 
#   as_tibble() %>% 
#   mutate(total.obs.mins = as.numeric(total.obs.mins)) %>% 
#   select(species, date, hour.1.total, hour.2.total, hour.3.total, hour.4.total, hour.5.total,
#          day.total, total.obs.mins, time.of.obs, start.time.am, start.time.pm, principle.observer, 
#          wind.dir.speed, wind.dir, wind.speed, visibility)# %>% 
# #write.csv("outputs/example_cleaned_data.csv")