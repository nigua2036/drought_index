#!/usr/bin/env Rscript

library(tidyverse)
library(glue)
library(lubridate)
library(showtext)

font_add_google("Roboto slab", family = "roboto-slab")
font_add_google("Montserrat", family = "montserrat")

showtext_auto() #make sure the font family is visible

prcp_data <- read_tsv("data/ghcnd_tidy.tsv.gz")

station_data <- read_tsv("data/ghcnd_regions_years.tsv")

buffered_end <- today() - 5
buffered_start <- buffered_end - 30


# anti_join(prcp_data, station_data, by = "id")
# anti_join( station_data, prcp_data, by = "id")

lat_long_prcp <- inner_join(prcp_data, station_data, by = "id") %>%
    filter((year != first_year & year != last_year) | year == buffered_end) %>%
    group_by( latitude, longitude, year)%>%
    summarise(mean_prcp = mean(prcp),
    .groups = "drop")


# end <- format(buffered_end, "%B %d %Y")
# start <- format(buffered_end - 30, "%B %d")

# different month same year
# end <- format(buffered_end, "%B %d %Y")
# start <- format(buffered_start, "%B %d")

# same month, same year
# end <- format(buffered_end, "%d %Y")
# start <- format(buffered_start, "%B %d")

# different month, different year
# end <- format(buffered_end, "%B %d %Y")
# start <- format(buffered_start, "%B %d")



end <- case_when(month(buffered_start) != month(buffered_end) ~ format(buffered_end, "%B %-d %Y"),
                 month(buffered_start) == month(buffered_end) ~ format(buffered_end, "%-d %Y"),
                 TRUE ~ NA_character_)

start <- case_when(year(buffered_start) != year(buffered_end) ~ format(buffered_start, "%B %-d %Y"),
                   year(buffered_start) == year(buffered_end) ~ format(buffered_start, "%B %-d"),
                   TRUE ~ NA_character_)

date_range <- glue("{start} to {end}")

# this_year <- lat_long_prcp %>%
#     filter(year == 2026) %>%
#     select(-year)

# inner_join(lat_long_prcp, this_year, 
#             by = c("latitude", "longitude")) %>%
#     rename(all_years = mean_prcp.x, 
#            this_year = mean_prcp.y ) %>%
  
world_map <- map_data("world") %>%
    filter(region != "Antarctica") 
    # %>%
    # mutate(lat = round(lat),
    #        long = round(long))

  lat_long_prcp%>%
    group_by(latitude, longitude) %>%
    mutate(z_score = (mean_prcp - mean(mean_prcp)) / sd(mean_prcp),
           n = n()) %>%
    ungroup() %>%
    filter(n >= 50 & year == year(buffered_end)) %>% #at least 50 years of data
    select(-n, -mean_prcp, -year) %>% 
    mutate(z_score = if_else(z_score > 2, 2, z_score),
           z_score = if_else(z_score < -2, -2, z_score)) %>%
    ggplot(aes(x = longitude, y = latitude, fill = z_score)) +
      geom_map(data = world_map, aes(map_id = region),
               map = world_map, fill = NA, color = "#f5f5f5", size = 0.05,
               inherit.aes = FALSE) +
      expand_limits(x = world_map$long, y = world_map$lat) +
      geom_tile() +
      coord_fixed() +
      scale_fill_gradient2(name = NULL,
                           low = "#d8b365", mid = "#f5f5f5", high = "#5ab4ac", 
                           midpoint = 0, 
                           breaks = c(-2, -1, 0, 1, 2),
                           labels = c("<-2", "-1", "0", "1", ">2")) +
    theme(plot.background = element_rect(fill = "black", color = "black"),
        panel.background = element_rect(fill = "black"),
        panel.grid = element_blank(),
        plot.title = element_text(color  = "#f5f5f5", size = 18,
                                  family = "roboto-slab"),
        plot.subtitle = element_text(color = "#f5f5f5",
                                     family = "montserrat", size = 10),
        plot.caption = element_text(color = "#f5f5f5",
                                    family = "montserrat"),
        legend.background = element_blank(),
        legend.text = element_text(colour = "#f5f5f5",
                                   family = "montserrat"),
        legend.position  = c(0.1, 0.0), 
        legend.direction = "horizontal",
        legend.key.height = unit(0.25, "cm"),
        axis.text = element_blank()
        ) +
    labs(title = glue("Amount of precipitation for {date_range}"),
    subtitle = "Standardised Z-scores for at least the past 50 years",
    caption = "Precipitation data collected from GHCN daily data at NOAA")

ggsave("visuals/world_drought.png", width = 8, height = 4)

