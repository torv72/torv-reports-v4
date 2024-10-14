
generate_report <- function(.site_name,
                            .zip_code,
                            .date_sample_submitted,
                            .start_date,
                            .end_date = NULL,
                            .om_seasons = "all",
                            .om_stats = "average",
                            .warm_or_cool,
                            .acid_extract,
                            .include_results_interpretation,
                            .include_sand_fraction,
                            .draw_beeswarm = "Yes",
                            .typeface = "Georama",
                            .output = "html",
                            .overwrite_report = "No",
                            .test = "No") {

  # Check function inputs
  if (!.om_seasons %in% c("all", "season")) stop('.om_seasons should be either "all" or "season".')
  if (!.om_stats %in% c("average", "median")) stop('.om_stats should be either "average" or "median".')
  if (!.warm_or_cool %in% c("warm", "cool")) stop('.warm_or_cool should be either "warm" or "cool".')
  if (!.acid_extract %in% c("Mehlich", "Olsen")) stop('.acid_extract should be either "Mehlich" or "Olsen".')
  if (!.include_results_interpretation %in% c("Yes", "No")) stop('.include_results_interpretation should be either "Yes" or "No".')
  if (!.include_sand_fraction %in% c("Yes", "No")) stop('.include_sand_fraction should be either "Yes" or "No".')
  if (!.draw_beeswarm %in% c("Yes", "No")) stop('.draw_beeswarm should be either "Yes" or "No".')
  if (!is.character(.typeface)) stop('.typeface should be of type character.')
  output <- stringr::str_to_lower(.output)
  if (any(!(output %in% c("html", "pdf") | output == c("pdf", "html") | .output == c("html", "pdf")))) stop('.output should be either "html", "pdf", or c("html", "pdf").')
  if (!.overwrite_report %in% c("Yes", "No")) stop('.overwrite_report should be either "Yes" or "No".')
  if (!.test %in% c("Yes", "No")) stop('.test should be either "Yes" or "No".')
  
  # Clears the environment, to avoid cross contamination between successive reports!
  rm(list = setdiff(ls(pos = ".GlobalEnv"),
                    # but keep the functions we need to run reports, set up tests and update the database
                    c("generate_report", "generate_longitudinal_summary",
                      "create_reference_pngs", "create_reference_data",
                      "test_report", "generate_report_args")),
     pos = ".GlobalEnv")

  # Import packages
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(forcats)
  library(ggplot2)
  library(purrr)
  library(vroom)
  library(rvest)
  library(janitor)
  library(here) 
  library(sf)
  library(systemfonts)
  library(lubridate)
  library(glue)
  library(patchwork) 
  library(prismatic) 
  library(flextable)
  library(ftExtra)
  library(officer)
  library(ggiraph)
  
  options(tigris_use_cache = TRUE)
  options(dplyr.summarise.inform = FALSE)
  
  ## Define months based on .om_seasons
  if (.om_seasons == "all") season <- 1:12
  if (.om_seasons == "season") {
    month <- month(.date_sample_submitted)
    if (month %in% 3:5) season <- 3:5
    if (month %in% 6:8) season <- 6:8
    if (month %in% 9:11) season <- 9:11
    if (month %in% c(1, 2, 12)) season <- c(1, 2, 12)
  }
  
  # Export to global Env so can be used by other scripts
  typeface <<- .typeface
  output_html <<- "html" %in% output
  output_pdf <<- "pdf" %in% output
  overwrite_report <<- ifelse(.overwrite_report == "Yes", TRUE, FALSE)
  testing_report <<- .test
  
  # Set up Figure directories if they aren't already there
  figure_dirs <- c("headers", "organic_matter", "soil_testing", "trendlines", "water_testing")
  
  for (figure_dir in figure_dirs) {
    if(!dir.exists(here("report", "figures", figure_dir))) {
      dir.create(here("report", "figures", figure_dir), recursive = TRUE)
    }
  }
  
  # Create alternative location for figures associated with test reports for ease of testing
  root_figure_location <<-
    if(testing_report != "No") {
      file.path("tests", "test-reports", testing_report, "figures")
    } else {
      file.path("report", "figures")
    }

  # Remove all the figures from report/figures for the same reason
  # Those are only temporary to get them into the word document
  file.remove(grep(".png", grep("_ref.png",
                                list.files(here(root_figure_location), recursive = TRUE, full.names = TRUE),
                                value = TRUE, invert = TRUE), value = TRUE))


  # The test variable allows us to test the output against know input
  # and compare it to the output we've already manually checked. You
  # don't need to do anything about this, as .test has a default value of "No".

  if(.test == "MC_202106") {
    # Full report, includes all sections other than water; tested against Excel calculator
    # Tests the "how to interpret" section switch
    input_params <- list(site_name = "Maroon Creek Club",
                         zip_code = 81611,
                         date_sample_submitted = "2021-05-21",
                         start_date = "1900-01-01",
                         # date at which the tests were finalised
                         end_date = "2021-12-31",
                         warm_or_cool = "cool",
                         acid_extract = "Mehlich",
                         include_results_interpretation = "Yes")
  } else if(.test == "MC_202108") {
    # Full report
    input_params <- list(site_name = "Maroon Creek Club",
                         zip_code = 81611,
                         date_sample_submitted = "2021-07-28",
                         end_date = "2022-03-24",
                         start_date = "1900-01-01",
                         warm_or_cool = "cool",
                         acid_extract = "Mehlich",
                         include_results_interpretation = "No")
  } else if(.test == "Saratoga_202109") {
    # Full report with only one date in sample data; tests trend plots
    # work with no date range
    input_params <- list(site_name = "Saratoga Lake Golf Club",
                         zip_code = 12866,
                         date_sample_submitted = "2021-09-24",
                         start_date = "2016-01-01",
                         end_date = "2022-03-24",
                         warm_or_cool = "cool",
                         acid_extract ="Olsen",
                         include_results_interpretation = "No")
  } else if(.test == "Wellshire_202111") {
    # Small report with no GREEN data; tests that the Turf Growth model
    # can still be created
    input_params <- list(site_name = "Wellshire Golf Club",
                         zip_code = 80222,
                         date_sample_submitted = "2021-11-04",
                         start_date = "2016-01-01",
                         end_date = "2022-03-24",
                         warm_or_cool = "cool",
                         acid_extract = "Mehlich",
                         include_results_interpretation = "No")
  } else if(.test == "Sonnenalp_202111") {
    # Small number of measurements; tests trends plots work in absence
    # of past data
    input_params <- list(site_name = "Sonnenalp Golf Club",
                         zip_code = 81632,
                         date_sample_submitted = "2021-11-04",
                         start_date = "2016-01-01",
                         end_date = "2022-03-24",
                         warm_or_cool = "cool",
                         acid_extract = "Mehlich",
                         include_results_interpretation = "No")
  } else if(.test == "Bartlett_202112") {
    # Small report with only OM data; tests the Turf Growth Model switch
    input_params <- list(site_name = "Bartlett Hills Golf Course",
                         zip_code = 60103,
                         date_sample_submitted = "2021-12-16",
                         start_date = "2016-01-01",
                         end_date = "2022-03-24",
                         warm_or_cool = "cool",
                         acid_extract = "Mehlich",
                         include_results_interpretation = "No")
  } else {
    input_params <- list(site_name = .site_name,
                         zip_code = .zip_code,
                         date_sample_submitted = .date_sample_submitted,
                         start_date = .start_date,
                         warm_or_cool = .warm_or_cool,
                         acid_extract = .acid_extract,
                         season = season,
                         om_seasons = .om_seasons,
                         om_stats = .om_stats,
                         include_results_interpretation = ifelse(.include_results_interpretation == "Yes", TRUE, FALSE),
                         include_sand_fraction = ifelse(.include_sand_fraction == "Yes", TRUE, FALSE),
                         beeswarm = ifelse(.draw_beeswarm == "Yes", TRUE, FALSE))
  }

  # The default is for the end date to be the same as the sample date (so that we're not
  # pulling in data from "the future")
  if(is.null(input_params$end_date)) {
    input_params$end_date <- input_params$date_sample_submitted
  }

  # list returned to the global Env so it can be accessed by the markdown file
  input_params <<- input_params


  # Ask before overwriting a file!
  if(.test == "No") {

    quiet_reporting <- FALSE
    
    filename <- paste0("TORV-report_",
                       gsub(" ", "-", input_params$site_name),
                       "_", input_params$date_sample_submitted,
                       "_", input_params$om_seasons,
                       "_", input_params$om_stats)
    
    filename_pdf <- paste0(filename, ".pdf")
    filename_html <- paste0(filename, ".html")

    if(output_pdf & file.exists(here("generated-reports", filename_pdf)) & overwrite_report == FALSE) {

      message("\n❗ There is already a report called ", filename_pdf, " in the generated-reports folder!\n\n❓ Do you want to overwrite it?")

      overwrite <- readline("Type y for yes or any other letter to exit and hit ENTER.")

      if(overwrite == "n") stop("\n❌ Ok, I'll exit now. Phew!")
    }
    if(output_html & file.exists(here("generated-reports", filename_html)) & overwrite_report == FALSE) {
      
      message("\n❗ There is already a report called ", filename_html, " in the generated-reports folder!\n\n❓ Do you want to overwrite it?")
      
      overwrite <- readline("Type y for yes or any other letter to exit and hit ENTER.")
      
      if(overwrite == "n") stop("\n❌ Ok, I'll exit now. Phew!")
    }
  } else {
    # Keep only test output in console when running tests
    quiet_reporting <- TRUE
  }
  
  # Generate the report
  if (output_html) {
    library(tufte)
    
    rmarkdown::render(
      input = "report/report.Rmd",
      quiet = quiet_reporting,
      output_file = here("generated-reports", filename_html),
      output_format = "tufte_html",
      output_options = list(css = "theme/style.css", embed_resources = TRUE),
      params = list(html = TRUE)
    )
  } 
  
  if (output_pdf) {
    
    rmarkdown::render(
      input = "report/report.Rmd",
      quiet = quiet_reporting,
      output_file = here("generated-reports", filename_pdf),
      output_format = "pdf_document",
      params = list(html = FALSE)
    )
  }

  # Run tests if this is a test report
  if (.test != "No") {
    testthat::test_file(here("tests/test-scripts/value-tests.R"))
    testthat::test_file(here("tests/test-scripts/plot-tests.R"))
  }
  
  # Clean-up
  graphics.off()
  x <- file.remove(here("report", "setup.md"))
  
}
