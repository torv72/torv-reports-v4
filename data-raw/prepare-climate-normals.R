library(here)
library(vroom)
library(sf)
library(tidyverse)
library(rvest)
library(janitor)

# data retrieved from https://www.ncei.noaa.gov/data/normals-monthly/1991-2020/archive/
mly_normal_all_all <- vroom(file = "data-raw/mly-normal-allall.csv")

month_abb_table <- tibble(month = 1:12, month_abb = month.abb)

mly_tavg_normal <- mly_normal_all_all %>%
  select(GHCN_ID, month, `MLY-TAVG-NORMAL`) %>%
  clean_names() %>%
  rename(station_id = ghcn_id) %>%
  mutate(month = as.numeric(month)) %>%
  left_join(month_abb_table, by = "month") %>%
  select(-month) %>%
  pivot_wider(names_from = month_abb, values_from = mly_tavg_normal)


# data retrieved from ftp.ncdc.noaa.gov

# data on stations, including latitude and longitude
station_data <- vroom_fwf(here("data-raw", "allstations.txt"),
                          col_positions = fwf_widths(widths = c(11, 9, 10, 7, 3, 31, 4, 4, 6),
                                                     col_names = c("station_id",
                                                                   "lat", "lon",
                                                                   "height",
                                                                   "state",
                                                                   "name",
                                                                   "abbrev_1", "abbrev_2",
                                                                   "possible_zip")),
                          col_types = c("cdddcccci")) %>%
  mutate(across(where(is_character), str_trim))

# station zip codes
station_zip <- vroom(here("data-raw", "zipcodes-normals-stations.txt"),
                     delim = " ",
                     col_names = c("station_id", "zip_code", "location"))

# combine data and turn into spatial object



monthly_normals <- mly_tavg_normal %>%
  left_join(station_data, by = "station_id") %>%
  left_join(station_zip, by = "station_id") %>%
  select(c("station_id", month.abb, "lat", "lon", "state", "location", "name", "zip_code")) %>%
  filter(!is.na(lat), !is.na(lon), !is.na(zip_code)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

write_rds(monthly_normals, "data/monthly_normals_2010.rds")
