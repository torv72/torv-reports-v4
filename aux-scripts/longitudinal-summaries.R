
# This code chunk generates longitudinal data in the longitudinal-data folder

if (!fs::dir_exists("longitudinal-summaries")) {
  fs::dir_create("longitudinal-summaries")
}



# Load Data ---------------------------------------------------------------

# function to automate the excel file creation
excel_file_creator <- function(site_list, database) {

  for (i in seq_along(site_list)) {

    soil_dataframe <- database %>%
      filter(
        sample_type == "Soil",
        site == site_list[i]) %>%
      unite("sample_location", sample_description_number_1:sample_description_number_4, sep = " ", na.rm = T) %>%
      select(
        date_sample_submitted, sample_location, measurement_name, measurement_result
      ) %>%
      mutate(date_sample_submitted = lubridate::as_date(date_sample_submitted)) %>%
      arrange(date_sample_submitted, measurement_name) %>%
      set_names(c("Date", "Sample Location", "Measurement Name", "Measurement Result"))


    water_dataframe <- database %>%
      filter(
        sample_type == "Water",
        site == site_list[i]) %>%
      unite("sample_location", sample_description_number_1:sample_description_number_4, sep = " ", na.rm = T) %>%
      select(
        date_sample_submitted, sample_location, measurement_name, measurement_result
      ) %>%
      mutate(date_sample_submitted = lubridate::as_date(date_sample_submitted)) %>%
      arrange(date_sample_submitted, measurement_name) %>%
      set_names(c("Date", "Sample Location", "Measurement Name", "Measurement Result"))

    physical_dataframe <- database %>%
      filter(
        sample_type == "Physical",
        site == site_list[i]) %>%
      unite("sample_location", sample_description_number_1:sample_description_number_4, sep = " ", na.rm = T) %>%
      select(
        date_sample_submitted, sample_location, measurement_name, measurement_result
      ) %>%
      mutate(date_sample_submitted = lubridate::as_date(date_sample_submitted)) %>%
      arrange(date_sample_submitted, measurement_name) %>%
      set_names(c("Date", "Sample Location", "Measurement Name", "Measurement Result"))


    environmental_dataframe <- database %>%
      filter(
        sample_type == "Environmental",
        site == site_list[i]) %>%
      unite("sample_location", sample_description_number_1:sample_description_number_4, sep = " ", na.rm = T) %>%
      select(
        date_sample_submitted, sample_location, measurement_name, measurement_result
      ) %>%
      mutate(date_sample_submitted = lubridate::as_date(date_sample_submitted)) %>%
      arrange(date_sample_submitted, measurement_name) %>%
      set_names(c("Date", "Sample Location", "Measurement Name", "Measurement Result"))


    spe_dataframe <- database %>%
      filter(
        sample_type == "SPE",
        site == site_list[i]) %>%
      unite("sample_location", sample_description_number_1:sample_description_number_4, sep = " ", na.rm = T) %>%
      select(
        date_sample_submitted, sample_location, measurement_name, measurement_result
      ) %>%
      mutate(date_sample_submitted = lubridate::as_date(date_sample_submitted)) %>%
      arrange(date_sample_submitted, measurement_name) %>%
      set_names(c("Date", "Sample Location", "Measurement Name", "Measurement Result"))


    compost_dataframe <- database %>%
      filter(
        sample_type == "Compost",
        site == site_list[i]) %>%
      unite("sample_location", sample_description_number_1:sample_description_number_4, sep = " ", na.rm = T) %>%
      select(
        date_sample_submitted, sample_location, measurement_name, measurement_result
      ) %>%
      mutate(date_sample_submitted = lubridate::as_date(date_sample_submitted)) %>%
      arrange(date_sample_submitted, measurement_name) %>%
      set_names(c("Date", "Sample Location", "Measurement Name", "Measurement Result"))


    fertilizer_dataframe <- database %>%
      filter(
        sample_type == "Fertilizer",
        site == site_list[i]) %>%
      unite("sample_location", sample_description_number_1:sample_description_number_4, sep = " ", na.rm = T) %>%
      select(
        date_sample_submitted, sample_location, measurement_name, measurement_result
      ) %>%
      mutate(date_sample_submitted = lubridate::as_date(date_sample_submitted)) %>%
      arrange(date_sample_submitted, measurement_name) %>%
      set_names(c("Date", "Sample Location", "Measurement Name", "Measurement Result"))


    dfs <- list(soil_dataframe, water_dataframe, physical_dataframe, environmental_dataframe, spe_dataframe, compost_dataframe, fertilizer_dataframe)
    df_names <- c("Soil", "Water", "Physical", "Environmental", "SPE", "Compost", "Fertilizer")
    name_vals <- c()
    list_init <- list()
    for (j in seq_along(df_names)){

      if (nrow(dfs[[j]]) != 0) {

        name_vals <- c(name_vals, df_names[j])
        to_add <- list(dfs[[j]])
        list_init <- append(list_init, to_add)

      }

    }

    names(list_init) <- name_vals

    cleaned_name <- str_squish((str_replace_all(site_list[i], regex("\\W+"), " ")))
    file_name <- paste0(str_replace_all(cleaned_name, " ", " "), " Longitudinal Data.xlsx")
    full_path <- here("longitudinal-summaries", file_name)

    if (length(list_init) == 0) {

      message(paste0("***** No data for Soil, Water, or Organic Matter found for ", all_sites[i], " *****"))

    } else {

      openxlsx::write.xlsx(list_init, full_path,
                           overwrite = TRUE)

    }

  }

}


generate_longitudinal_summary <- function(.site_name,
                                          .start_date) {

  start_date <- ymd(.start_date)

  rowAny <- function(x) rowSums(x) > 0
  full_database <- readxl::read_excel(here("data", "MASTER_DATABASE.xlsx"),
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
    filter(date_sample_submitted > start_date)

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
    bind_rows(nitrogen_sums)


  excel_file_creator(.site_name, full_database)

}


