# Testing deficits values based on calculations provided by Eric
testing <- T

filtered_database <- full_database %>%
  filter(site == "Maroon Creek Club",
         date_sample_submitted == lubridate::ymd("2021-05-21 UTC"))

# Replicate station data in the Excel sheet:
nearest_station_data <- tibble(station_id = "temp",
                               Jan = 19.8,
                               Feb = 22.2,
                               Mar = 30.9,
                               Apr = 39.8,
                               May = 48.4,
                               Jun = 57.5,
                               Jul = 63.5,
                               Aug = 61.3,
                               Sep = 53.3,
                               Oct = 42.2,
                               Nov = 29.3,
                               Dec = 20.0)

optimum_growth_temperature <- 68
optimum_growth_variance <- 10

source(here("aux-scripts/deficit-tables.R"))

load(here("aux-scripts/tests/deficit_data.rds"))
testthat::expect_equal(deficit_table_all_ref, deficit_table_all)
testthat::expect_equal(climate_all_types_ref, climate_all_types_ref)

rm(testing)
