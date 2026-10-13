#!/usr/bin/env Rscript

library(tidyverse)
library(glue)
library(lubridate)

# https://www.ncei.noaa.gov/pub/data/ghcn/daily/readme.txt
# ------------------------------
# Variable   Columns   Type
# ------------------------------
# ID            1-11   Character
# YEAR         12-15   Integer
# MONTH        16-17   Integer
# ELEMENT      18-21   Character
# VALUE1       22-26   Integer
# MFLAG1       27-27   Character
# QFLAG1       28-28   Character
# SFLAG1       29-29   Character
# VALUE2       30-34   Integer
# MFLAG2       35-35   Character
# QFLAG2       36-36   Character
# SFLAG2       37-37   Character
#   .           .          .
#   .           .          .
#   .           .          .
# VALUE31    262-266   Integer
# MFLAG31    267-267   Character
# QFLAG31    268-268   Character
# SFLAG31    269-269   Character
# ------------------------------

tday_julian <- yday(today())
window <- 30

quadruple <- function(x) {
    c(glue("VALUE{x}"),glue("MFLAG{x}"), glue("QFLAG{x}"), glue("SFLAG{x}") )

}

widths <- c(11, 4, 2, 4, rep(c(5, 1, 1, 1), 31))
headers <- c("ID", "YEAR", "MONTH", "ELEMENT", unlist(map(1:31, quadruple)))


# test <- read_fwf(
#   "data/ghcnd_cat.gz",
#   fwf_widths(widths, headers),
#   na = c("NA", "-9999"),
#   col_types = cols(.default = col_character()),
#   col_select = c(ID, YEAR, MONTH, ELEMENT, starts_with("VALUE")),
#   n_max = 1000,
#   lazy = FALSE,
#   num_threads = 1
# )


# archive("write_dir.tar.gz")%>%
#     pull(path)%>%
#     map_dfr(., ~read_tsv(archive_read("write_dir.tar.gz", .x)))

# dly_files <- archive("data/ghcnd_all.tar.gz")%>%
#     filter(str_detect(path, "dly"))%>%
#     slice_sample(n = 12)%>%
#     pull(path)


# Sys.time()
# dly_files %>%
# map_dfr(., ~

# read_fwf("data/ghcnd_cat.gz",

process_prcpfiles <- function(x){
    
    print(x)

    read_fwf(x,
        fwf_widths(widths, headers),
        na = c("NA", "-9999"),
        col_types = cols(.default = col_character()),
        col_select = c(ID, YEAR, MONTH, starts_with("VALUE"))) %>%
    rename_all(tolower) %>%
    pivot_longer(cols = starts_with("value"), names_to = "day",
            values_to = "prcp") %>%
    drop_na() %>%
    filter(prcp != 0) %>%
    mutate(day = str_replace(day, "value", ""),
        date = ymd(glue("{year}-{month}-{day}")),
        prcp = as.numeric(prcp) / 100) %>% #prcp in cm
    select(id, date, prcp) %>%
    mutate(julian_day = yday(date),
        diff = tday_julian - julian_day,
        is_in_window = case_when(diff < window & diff > 0 ~ TRUE,
                                diff > window ~ FALSE,
                                tday_julian < window &
                                diff + 365 < window ~ TRUE,
                                diff < 0 ~ FALSE),
        year = year(date),
        year = if_else(diff < 0 & is_in_window, year + 1, year)) %>%
    filter(is_in_window) %>%
    group_by(id, year) %>%
    summarise(prcp = sum(prcp), .groups = "drop")
    }
   write_tsv("data/composite_dly.tsv")

# if all the compresied files were decompresied
# dly_files <- list.files("data/ghcnd_all", full.names = TRUE)


prcp_files <- list.files("data/temp", full.names = TRUE)

map_dfr(prcp_files, process_prcpfiles) %>%
  group_by(id, year) %>%
  summarise(prcp = sum(prcp), .groups = "drop") %>%
  write_tsv("data/ghcnd_tidy.tsv.gz")


# # read data based on fixed width 
# read_fwf(dly_files,
#         fwf_widths(widths, headers), 
#         na = c("NA", "-9999"),
#         col_types = cols(.default = col_character()), 
#         col_select =c(ID,YEAR, MONTH, ELEMENT, starts_with("VALUE"))) %>%
#     rename_all(tolower)%>%
#     filter(element == "PRCP")%>%
#     select(-element)%>%
#     pivot_longer(cols = starts_with("value"), names_to = "day", 
#                     values_to = "prcp")%>%
#     drop_na()%>%
#     mutate(day = str_replace(day, "value", ""), 
#     date = ymd(glue("{year}-{month}-{day}")),
#     prcp = as.numeric(prcp)/100)%>% #prcp in cm 
#     select(id, date, prcp)%>%
#     write_tsv("data/composite_dly.tsv")
