#!/usr/bin/env Rscript

library(tidyverse)
library(glue)
library(lubridate)


prcp_data <- read_tsv("data/ghcnd_tidy.tsv.gz")

station_data <- read_tsv("data/ghcnd_regions_years.tsv")

# anti_join(prcp_data, station_data, by = "id")
# anti_join( station_data, prcp_data, by = "id")

lat_long_prcp <- inner_join(prcp_data, station_data, by = "id") %>%
    filter((year != first_year & year != last_year) | year == 2026) %>%
    group_by( latitude, longitude, year)%>%
    summarise(mean_prcp = mean(prcp))%>%
    summarise(n = n())



