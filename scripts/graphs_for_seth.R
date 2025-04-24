#------------------------------------------------#
####           Packages and Source            ####
#------------------------------------------------#

## Packages
library(tidyverse)
library(lubridate)
library(scales)




#------------------------------------------------#
####           Read and clean data            ####
#------------------------------------------------#

## Read, fix some formatting/groups, and calculate stats
sp.groups <- read.csv("outputs/species_groupings.csv")

flightcomp <- tibble(read.csv("outputs/species_totals.csv")) %>% 
  left_join(., sp.groups, by = "species") %>% 
  select(-X) %>% 
  mutate(grouping = str_replace(grouping, "Alcids", "Other waterbirds"),
         grouping = str_replace(grouping, "Grebes", "Other waterbirds"),
         grouping = str_replace(grouping, "Raptors", "Other waterbirds"),
         grouping = str_replace(grouping, "Waterbird sp.", "Other waterbirds"),
         grouping = str_replace(grouping, "Herons", "Other waterbirds"),
         grouping = str_replace(grouping, "Jaegers", "Other waterbirds"),
         grouping = str_replace(grouping, "Shearwaters", "Other waterbirds"),
         grouping = str_replace(grouping, "Storm-Petrels", "Other waterbirds"),
         grouping = str_replace(grouping, "Shorebirds", "Other waterbirds"),
         grouping = str_replace(grouping, "Other ducks and geese", "Other ducks/geese"),
         grouping = str_replace(grouping, "Larids", "Gulls")) %>% 
  group_by(grouping) %>% 
  summarise(total = sum(total.indiv)) %>% 
  arrange(-total) %>% 
  filter(row_number() < 9) %>% 
  mutate(percent = 100*(total/sum(total)),
         grouping = factor(grouping, levels = c("Cormorants", "Scoters", "Common Eider",
                                                "Northern Gannet", "Other ducks/geese",
                                                "Loons", "Gulls", "Other waterbirds")))


## Write csv
# write.csv(flightcomp, "outputs/sea_watch_flight_composition.csv", row.names = F)


## Plot
flightcomp %>% 
  ggplot(aes(x = grouping, y = total, fill = total)) +
  geom_bar(stat = "identity", color = "black", linewidth = 0.4) +
  scale_y_continuous(labels = comma, expand = c(0,0), limits = c(0, 225000)) +
  labs(y = "Total migrants", x = NULL, title = "Seasonal Waterbird Flight Composition",
       subtitle = "2016 - 2024") +
  theme_classic() +
  theme(plot.title.position = "plot",
        plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(color = "gray30", size = 15, margin = margin(0, 0, 0.6, 0, "cm")),
        axis.text = element_text(color = "black", size = 13),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 13),
        axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, "cm")),
        legend.position = "none")


## Save plot
# ggsave("outputs/flight_composition_figure_2025.png", dpi = 700, height = 5.5, width = 8)




