
# Create and load the function
update_database <- function() {

  library(dplyr)
  library(tibble)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(readxl)
  library(writexl)
  library(janitor)
  library(here)
  library(glue)
  library(lubridate)
  library(patchwork)

  master_database_file <- here("data", "MASTER_DATABASE.xlsx")
  raw_files_location <- here("data-raw/lab-reports/")
  
  # Read in manually compiled database ----------------------------
  rowAny <- function(x) rowSums(x) > 0
  original_database <- read_excel(master_database_file,
                                  # skipping the data checking columns to the
                                  range = cell_cols("A:Q"),
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
    clean_names() # make names r friendly

  # Read in site codes ----
  client_codes <- read_excel(here("data", "code_book.xlsx"),
                             sheet = "client_list") %>%
    clean_names() %>%
    mutate(client_number = as.character(client_number))

  site_codes <- read_excel(here("data", "code_book.xlsx"),
                           sheet = "site_codes") %>%
    clean_names()

  test_codes <- read_excel(here("data", "code_book.xlsx"),
                           sheet = "catalog") %>%
    clean_names()

  measurement_codes <- read_excel(here("data", "code_book.xlsx"),
                                  sheet = "measurement_names") %>%
    clean_names()

  # Set up functions and empty list to use later ----

  datasets_parsed <- list()

  # get_date_submitted <- function(file_path) {
  #   date_submitted <- as_date(read_excel(file_path, skip = 1, n_max = 1)[[2]][1])
  #
  #   if (is.na(date_submitted)) date_submitted <- NA_Date_
  #
  #   return(date_submitted)
  # }

  # Get file names in lab-reports folder ----------------------------
  raw_data_list <- grep("xls",
                        list.files(raw_files_location),
                        value = TRUE) %>%
    data.frame(file_name = .)

  # Check files that have already been added, to avoid duplication
  if(any(gsub("\"|.xlsx|.xls", "", raw_data_list$file_name) %in% original_database$source_filename)) {

    message("\n\n* The following datasets are already in the MASTER_DATABASE.xlsx file. *",
            paste0("\n - ", intersect(gsub("\"|.xlsx|.xls", "", raw_data_list$file_name), original_database$source_filename)),
            "\nI will skip those to avoid overriding any manual changes you had previously made to them in MASTER_DATABASE.xlsx!\n\n")
    
    raw_data_list <- raw_data_list %>%
      filter(!gsub("\"|.xlsx|.xls", "", raw_data_list$file_name) %in%
               original_database$source_filename)

    if(nrow(raw_data_list) == 0) {
      stop("
All the datasets in `data-raw/lab-reports` are already in MASTER_DATABASE.xlsx!
I will therefore not create a new MASTER_DATABASE_UPDATED_[date].xlsx.")
    }
  }

  # Set up details for different excel files which share a similar format
  skip_rows <- list(SOILd = 7,
                    SOILa = 6,
                    WATERa = 6,
                    SLLBS = 6,
                    PHYSa = 6,
                    PHYSb = 6,
                    PHYSd = 6,
                    FERT = 6,
                    COMPOSTc = 7,
                    SOILGEN = 7)

  # Read in data for sheets with common names ("SOILd", "SOILa", "WATERa", "SLLBS", PHYSb", "PHYSd", "FERT") ----

  for(file_type in names(skip_rows)){

    if(nrow(filter(raw_data_list, str_detect(file_name, file_type))) > 0) {
      datasets_parsed[[file_type]] <- raw_data_list %>%
        filter(str_detect(file_name, file_type)) %>%
        mutate(file_path = here(raw_files_location, file_name)) %>%
        pmap_df(~(read_excel(.y,
                             skip = skip_rows[[file_type]],
                             col_types = c("text"),
                             trim_ws = TRUE) %>%
                    mutate(source_filename = gsub("\"|.xlsx|.xls", "", .x)) %>%
                    mutate(date_sample_submitted = lubridate::mdy(gsub("(.*_)(\\d{5})_([A-Z]+?\\d.+?)_(.*)_(\\d{6})(.*)", "\\5", .x))) %>%
                    mutate(client_number = gsub("(.*_)(\\d{5})(_.*)", "\\2", .x)) %>%
                    mutate(test_short_name = gsub("(.*_)(\\d{5})_([A-Z]+?\\d.+?)(_.*)", "\\3", .x)) %>%
                    mutate(sample_type_raw = gsub("(.*_)(\\d{5})_([A-Z]+?\\d.+?)(_)(.*?)(_.*)", "\\5", .x))
        )) %>%
        # using setdiff() allows for occasions when there is no "Sample Description #2" column
        mutate(across(setdiff(names(.),
                              c("source_filename", "date_sample_submitted", "Sample Location",
                                "Sample Description #1", "Sample Description #2", "Lab Number",
                                "client_number", "test_short_name", "sample_type_raw", "sample_type_raw")),
                      ~(str_replace(.x, "<|>", "") %>%
                          as.numeric()))) %>%  # if below limit of detection, set to max possible value
        relocate(source_filename) %>%
        pivot_longer(cols = setdiff(names(.),
                                    c("source_filename", "date_sample_submitted", "Sample Location",
                                      "Sample Description #1", "Sample Description #2", "Lab Number",
                                      "client_number", "test_short_name", "sample_type_raw")),
                     names_to = "measurement_name",
                     values_to = "measurement_result") %>%
        clean_names() %>%
        filter(!is.na(measurement_result))
    }
  }


  # Read in data for PHYSg sheet, slightly different format than others ----------------------------

  if(nrow(filter(raw_data_list, str_detect(file_name, "PHYSg"))) > 0) {
    datasets_parsed[["PHYSg_data"]] <- raw_data_list %>%
      filter(str_detect(file_name, "PHYSg")) %>%
      mutate(file_path = here(raw_files_location, file_name)) %>%
      pmap_df(~(read_excel(.y,
                           skip = 6,
                           col_types = c("text"),
                           trim_ws = TRUE) %>%
                  mutate(source_filename = gsub("\"|.xlsx|.xls", "", .x)) %>%
                  mutate(lubridate::mdy(gsub("(.*_)(\\d{5})_([A-Z]+?\\d.+?)_(.*)_(\\d{6})(.*)", "\\5", .x))) %>%
                  mutate(client_number = gsub("(.*_)(\\d{5})(_.*)", "\\2", .x)) %>%
                  mutate(test_short_name = gsub("(.*_)(\\d{5})_([A-Z]+?\\d.+?)(_.*)", "\\3", .x)) %>%
                  mutate(sample_type_raw = gsub("(.*_)(\\d{5})_([A-Z]+?\\d.+?)(_)(.*?)(_.*)", "\\5", .x)))) %>%
      mutate(across(setdiff(names(.),
                            c("source_filename", "date_sample_submitted", "SampleLocation",
                              "SampleDescription1", "SampleDescription2", "LabNumber",
                              "client_number", "test_short_name", "sample_type_raw")),
                    ~(str_replace(.x, "<|>", "") %>%
                        as.numeric()))) %>%  # if below limit of detection, set to max possible value
      relocate(source_filename) %>%
      pivot_longer(cols = setdiff(names(.),
                                  c("source_filename", "date_sample_submitted", "SampleLocation",
                                    "SampleDescription1", "SampleDescription2", "LabNumber",
                                    "client_number", "test_short_name", "sample_type_raw")),
                   names_to = "measurement_name",
                   values_to = "measurement_result") %>%
      rename("Sample Description #1" = "SampleDescription1",
             "Sample Description #2" = "SampleDescription2") %>%
      clean_names() %>%
      filter(!is.na(measurement_result))
  }

  # Read in data for PHYSh sheet, slightly different format than others ----------------------------

  if(nrow(filter(raw_data_list, str_detect(file_name, "PHYSh"))) > 0) {
    datasets_parsed[["PHYSh_data"]] <- raw_data_list %>%
      filter(str_detect(file_name, "PHYSh")) %>%
      mutate(file_path = here(raw_files_location, file_name)) %>%
      pmap_df(~(read_excel(.y,
                           skip = 6,
                           col_types = c("text"),
                           trim_ws = TRUE) %>%
                  mutate(source_filename = gsub("\"|.xlsx|.xls", "", .x)) %>%
                  mutate(date_sample_submitted = lubridate::mdy(gsub("(.*_)(\\d{5})_([A-Z]+?\\d.+?)_(.*)_(\\d{6})(.*)", "\\5", .x))) %>%
                  mutate(client_number = gsub("(.*_)(\\d{5})(_.*)", "\\2", .x)) %>%
                  mutate(test_short_name = gsub("(.*_)(\\d{5})_([A-Z]+?\\d.+?)(_.*)", "\\3", .x)) %>%
                  mutate(sample_type_raw = gsub("(.*_)(\\d{5})_([A-Z]+?\\d.+?)(_)(.*?)(_.*)", "\\5", .x)))) %>%
      mutate(across(setdiff(names(.),
                            c("source_filename", "date_sample_submitted", "SampleLocation",
                              "SampleDescription1", "LabNumber",
                              "client_number", "test_short_name", "sample_type_raw")),
                    ~(str_replace(.x, "<|>", "") %>%
                        as.numeric()))) %>%  # if below limit of detection, set to max possible value
      relocate(source_filename) %>%
      pivot_longer(cols = setdiff(names(.),
                                  c("source_filename", "date_sample_submitted", "SampleLocation",
                                    "SampleDescription1", "LabNumber",
                                    "client_number", "test_short_name", "sample_type_raw")),
                   names_to = "measurement_name",
                   values_to = "measurement_result") %>%
      rename("Sample Description #1" = "SampleDescription1") %>%
      clean_names() %>%
      filter(!is.na(measurement_result))
  }


  # Read in data for EnvGeneral sheets format than others ----------------------------

  read_env_table <- function(file_name, file_path) {
    env_table <- read_excel(file_path,
                            skip = 6,
                            col_types = c("text"),
                            trim_ws = TRUE)

    measurement_names <- str_to_title(rep(names(env_table)[str_detect(names(env_table), r"(\.\.\.)", negate = TRUE)], each = 2))
    first_measurement <- which(str_detect(names(env_table), r"(\.\.\.)", negate = TRUE))[1]
    new_names <- c(rep("", first_measurement - 1), measurement_names) %>%
      str_replace("^Ph$", "pH")

    names(env_table) <- map2_chr(new_names, env_table[1,], ~glue("{.x} {.y}")) %>%
      str_remove("Result ") %>%
      str_trim() %>%
      str_replace("# ", "#")

    env_table[-1,] %>%
      mutate(source_filename = gsub("\"|.xlsx|.xls", "", file_name)) %>%
      mutate(date_sample_submitted = lubridate::mdy(gsub("(.*_)(\\d{5})_([A-Z]+?\\d.+?)_(.*)_(\\d{6})(.*)", "\\5", file_name)))
  }

  if(nrow(filter(raw_data_list, str_detect(file_name, "EnvGeneral|EnvBio|EnvMulti"))) > 0) {
    datasets_parsed[["EnvGeneral_data"]] <- raw_data_list %>%
      filter(str_detect(file_name, "EnvGeneral|EnvBio|EnvMulti")) %>%
      mutate(file_path = here(raw_files_location, file_name)) %>%
      mutate(source_filename = gsub("\"|.xlsx|.xls", "", file_name)) %>%
      pmap_df(~read_env_table(.x, .y) %>%
                mutate(client_number = gsub("(.*_)(\\d{5})(_.*)", "\\2", .x)) %>%
                mutate(test_short_name = gsub("(.*_)(\\d{5})_([A-Z]+?\\d.+?)(_.*)", "\\3", .x)) %>%
                mutate(sample_type_raw = gsub("(.*_)(\\d{5})_([A-Z]+?\\d.+?)(_)(.*?)(_.*)", "\\5", .x))) %>%
      mutate(across(setdiff(names(.),
                            c("source_filename", "date_sample_submitted", "Sample Location",
                              "Sample Description #1", "Sample Description #2", "Lab Number",
                              "client_number", "test_short_name", "sample_type_raw")),
                    ~(str_replace(.x, "<|>", "") %>%
                        str_replace("negative", "0") %>%
                        na_if("ND") %>%
                        as.numeric()))) %>%  # if below limit of detection, set to max possible value
      relocate(source_filename) %>%
      pivot_longer(cols = setdiff(names(.),
                                  c("source_filename", "date_sample_submitted", "Sample Location",
                                    "Sample Description #1", "Sample Description #2", "Lab Number",
                                    "client_number", "test_short_name", "sample_type_raw")),
                   names_to = "measurement_name",
                   values_to = "measurement_result") %>%
      clean_names() %>%
      filter(!is.na(measurement_result))
  }


  # Prepare the data to be added into the database ----

  # Dummy columns required for logic in next step

  cols <- c(sample_description_number_1 = "",
            sample_description_number_2 = "",
            sample_description_number_3 = "",
            sample_description_number_4 = "")


  duplicates <- bind_rows(datasets_parsed) %>%
    filter(duplicated(.))

  if(nrow(duplicates != 0)) {
    write.csv(duplicates, row.names = F, here("data/duplicates.csv"))

    message("*** \nI found duplicate rows in the following files and have removed them.
Only the unique rows of data from those files have been kept and added to the database.")

    message(paste0("\n- ", unique(duplicates$source_filename)))

    message("\nI have created a file called `duplicates.csv` in the `data` folder,
containing the data I excluded. Please take a look at these and at the original data
to confirm these were indeed duplicates.
***\n\n")
  }

  data_to_add_to_db <- bind_rows(datasets_parsed) %>%
    # TODO: check if we want to keep this
    filter(!duplicated(.)) %>%
    # Create dummy columns necessary for the following bits of logic if the columns don't already exist
    {
      add_column(., !!!cols[!names(cols) %in% names(.)])
    } %>%
    mutate(analytical_company_name = "TORV, LLC") %>%
    left_join(client_codes) %>%
    left_join(test_codes) %>%
    left_join(measurement_codes) %>%
    mutate(measurement_name = case_when(
      is.na(master_database_naming_convention) ~ measurement_name,
      TRUE ~ master_database_naming_convention),
      sample_description_number_1 = gsub("#", "", sample_description_number_1),
      # legacy: the value used to be OM; leaving both options for backward compatibility
      # if reading in old data or in using old data within the database
      sample_type = case_when(sample_type_raw %in% c("OM", "Physical") ~ "Physical",
                              TRUE ~ sample_type_raw)) %>%
    # Shifting things around to match current database
    mutate(sample_description_number_4 = case_when(sample_type_raw %in% c("OM", "Physical") ~ sample_description_number_2,
                                                   TRUE ~ ""),
           sample_description_number_3 = case_when(sample_type_raw %in% c("OM", "Physical") ~ sample_description_number_1,
                                                   TRUE ~ ""),
           sample_description_number_2 = case_when(sample_type_raw %in% c("OM", "Physical") ~ gsub(" OM", "", sample_location),
                                                   TRUE ~ sample_description_number_1),
           sample_description_number_1 = case_when(sample_type_raw %in% c("OM", "Physical") ~ ifelse(grepl("PHYSb", source_filename), "OM", "-"),
                                                   TRUE ~ sample_location)) %>%
    select(intersect(c(names(original_database), "client_number"), names(.)))

  message("\n\n --- Please help me match up the clients and their sites --- \n\n")

  existing_sites <- original_database %>%
    select(client_name, site) %>%
    unique() %>%
    full_join(client_codes)

  correct_sites <- tibble()

  for(filename in unique(data_to_add_to_db$source_filename)){

    happy_with_input <- "n"

    site_matches <- filter(data_to_add_to_db, source_filename == filename) %>%
      select(source_filename, client_number, client_name) %>%
      unique() %>%
      left_join(existing_sites)


    while(happy_with_input != "y") {

      if(length(site_matches$site) > 0) {

        # print matches, ask for selection or 0 to type in a new one;
        # left join the selection (filter so it matches the site / create new cell value based on input)

        message(paste(filename, "\nI found", verbaliseR::pluralise("site", length(site_matches$site)),
                      "corresponding to this client in the database:"))

        message(paste0("\n     ", (1:length(site_matches$site)), ". ", site_matches$site))

        message("\nPlease type the number corresponding to the site you want to select, or type in a new site name, then hit ENTER.")

        site_entered <- readline("Answer: ")

        if(!is.na(suppressMessages(as.numeric(site_entered)))) {

          while(!between(as.numeric(site_entered), 1, length(site_matches$site))) {

            message("!! The number you entered was not one of the options!! Please try again.")

            message(paste("\n", filename, "\nI found", verbaliseR::pluralise("site", length(site_matches$site)),
                          "corresponding to this client in the database:"))

            message(paste0("\n     ", (1:length(site_matches$site)), ". ", site_matches$site))

            message("\nPlease type the number corresponding to the site you want to select, or type in a new site name, then hit ENTER.")


            site_entered <- readline("Answer: ")

          }
          site_to_use <- site_matches$site[as.numeric(site_entered)]

          print(site_to_use)

        } else {

          site_to_use <- site_entered

        }
      } else {

        message("I did not find any sites corresponding to that client number. Please type in the site name, then hit ENTER.")
        site_entered <- readline("Answer: ")

      }

      message(paste0("For the data corresponding to file ", filename, ", I will use this site: ", site_to_use))

      happy_with_input <- readline("Is that correct? (y/n):")

      if(happy_with_input == "n") message("No problem, let's try that one again!\n")

    }

    correct_sites <- correct_sites %>%
      rbind(tibble(source_filename = filename,
                   site = site_to_use))

  }

  data_to_add_to_db <- left_join(data_to_add_to_db,
                                 correct_sites)

  # TODO: add site - type match-up? Could use site codes or info from within the database (latter is easier to maintain)


  # Bind newly parsed files to existing database and write updated file for manual checking
  updated_database <- bind_rows(original_database,
                                data_to_add_to_db)


  write_xlsx(updated_database, here("data", paste0("MASTER_DATABASE_UPDATED_", Sys.Date(), ".xlsx")))

  message("\n\n***",
          "\nI have finished updating the database!",
          "\nPlease search for \"\\??\\\" to find values I wasn't able to establish programmatically,
update any other values as required, and replace the data/MASTER_DATABASE.xlsx file
with the one I've just created!

Once you've done that, also consider updating data/site_codes.csv to add in codes for
any new sites I should be able to match up next time!
***\n\n")
}

# Run the function
update_database()
