library(tidyverse) # for ease of loading ggplot functions and pipe
library(here) # set file paths relative to .Rproj file
library(lubridate)
library(glue)
library(patchwork) # works if load all up to this one


rowAny <- function(x) rowSums(x) > 0
full_database <<- readxl::read_excel(here("data", params$database_name),
                                     range = readxl::cell_cols("A:Q"),
                                     col_types = c("numeric", # A
                                                   "text", # B
                                                   "text", # C
                                                   "text", # D
                                                   "date", # E
                                                   "text", # F
                                                   "text", # G
                                                   "text", # H
                                                   "text", # I
                                                   "text", # J
                                                   "text", # K
                                                   "text", # L
                                                   "text", # M
                                                   "text", # N
                                                   "text", # O
                                                   "text", # P
                                                   "numeric" # Q
                                     )) %>%
  filter(rowAny(across(everything(), ~ !is.na(.x)))) %>%   # remove empty rows
  janitor::clean_names() %>%  # make names r friendly
  filter(date_sample_submitted > param_start_date)

nitrogen_sums <- full_database %>%
  filter(measurement_name %in% c("Ammonium (ppm)", "Nitrate (ppm)")) %>%
  group_by(source_filename, date_sample_submitted, sample_type, sample_description_number_1,
           sample_description_number_2, sample_description_number_3, sample_description_number_4) %>%
  add_count() %>% # this allows you to look through the data to see where there was only one measurement, if desired
  mutate(measurement_result = sum(measurement_result),
         measurement_name = "Total Nitrogen (ppm)") %>%
  select(!c(row_number, n)) %>%
  unique()

full_database <- full_database %>%
  bind_rows(nitrogen_sums) %>%
  select(-source_filename) %>%

  # Anonymize stuff
  mutate(client_name = apply(str_extract_all(client_name, "[A-Z]" ,simplify = TRUE), 1, paste, collapse="")) %>%
  mutate(site = apply(str_extract_all(site, "[A-Z]" ,simplify = TRUE), 1, paste, collapse="")) %>%
  write_csv("data-raw/torv-anonymized-data.csv")


