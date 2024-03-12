
generate_report <- function(.site_name,
                            .zip_code,
                            .date_sample_submitted,
                            .start_date,
                            .end_date = NULL,
                            .warm_or_cool,
                            .acid_extract,
                            .include_results_interpretation,
                            .test = "No") {

  # Clears the environment, to avoid cross contamination between successive reports!
  rm(list = setdiff(ls(pos = ".GlobalEnv"),
                    # but keep the functions we need to run reports, set up tests and update the database
                    c("generate_report", "generate_longitudinal_summary",
                      "create_reference_pngs", "create_reference_data",
                      "test_report", "generate_report_args")),
     pos = ".GlobalEnv")

  # Export to global Env so can be used by other scripts
  testing_report <<- .test

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
                                list.files(here::here(root_figure_location), recursive = T, full.names = T),
                                value = T, invert = T), value = T))


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
                         acid_extract ="Mehlich",
                         include_results_interpretation = TRUE)
  } else if(.test == "MC_202108") {
    # Full report
    input_params <- list(site_name = "Maroon Creek Club",
                         zip_code = 81611,
                         date_sample_submitted = "2021-07-28",
                         end_date = "2022-03-24",
                         start_date = "1900-01-01",
                         warm_or_cool = "cool",
                         acid_extract ="Mehlich",
                         include_results_interpretation = FALSE)
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
                         include_results_interpretation = FALSE)
  } else if(.test == "Wellshire_202111") {
    # Small report with no GREEN data; tests that the Turf Growth model
    # can still be created
    input_params <- list(site_name = "Wellshire Golf Club",
                         zip_code = 80222,
                         date_sample_submitted = "2021-11-04",
                         start_date = "2016-01-01",
                         end_date = "2022-03-24",
                         warm_or_cool = "cool",
                         acid_extract ="Mehlich",
                         include_results_interpretation = FALSE)
  } else if(.test == "Sonnenalp_202111") {
    # Small number of measurements; tests trends plots work in absence
    # of past data
    input_params <- list(site_name = "Sonnenalp Golf Club",
                         zip_code = 81632,
                         date_sample_submitted = "2021-11-04",
                         start_date = "2016-01-01",
                         end_date = "2022-03-24",
                         warm_or_cool = "cool",
                         acid_extract ="Mehlich",
                         include_results_interpretation = FALSE)
  } else if(.test == "Bartlett_202112") {
    # Small report with only OM data; tests the Turf Growth Model switch
    input_params <- list(site_name = "Bartlett Hills Golf Course",
                         zip_code = 60103,
                         date_sample_submitted = "2021-12-16",
                         start_date = "2016-01-01",
                         end_date = "2022-03-24",
                         warm_or_cool = "cool",
                         acid_extract ="Mehlich",
                         include_results_interpretation = FALSE)
  } else {
    input_params <- list(site_name = .site_name,
                         zip_code = .zip_code,
                         date_sample_submitted = .date_sample_submitted,
                         start_date = .start_date,
                         warm_or_cool = .warm_or_cool,
                         acid_extract = .acid_extract,
                         include_results_interpretation = .include_results_interpretation)
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

    if(file.exists(here::here("generated-reports",
                              paste0("TORV-report_",
                                     gsub(" ", "-", input_params$site_name),
                                     "_", input_params$date_sample_submitted, ".docx")))){

      message("\nThere is already a report called ", paste0("TORV-report_",
                                                            gsub(" ", "-", input_params$site_name),
                                                            "_", input_params$date_sample_submitted, ".docx"),
              "\nin the generated-reports folder!")

      overwrite <- readline("Do you want to overwrite it? Type y for yes or any other letter to exit and hit ENTER.")

      if(overwrite == "y") {
        message("Great, thanks! Always good to make sure!")
      } else {
        stop("Ok, I'll exit now. Phew!")
      }
    }
  } else {
    # Keep only test output in console when running tests
    quiet_reporting <- TRUE
  }

  # Generate the report
  rmarkdown::render(here::here("report/report.Rmd"),
                    quiet = quiet_reporting,
                    output_file = here::here(ifelse(testing_report == "No",
                                                    "generated-reports",
                                                    # saving the output elsewhere if we're generating reports
                                                    # just to test consequences of code changes
                                                    file.path(here::here("tests", "test-reports", testing_report))),
                                             paste0("TORV-report_",
                                                    gsub(" ", "-", input_params$site_name),
                                                    "_", input_params$date_sample_submitted, ".docx")))

  # Run tests if this is a test report
  if (.test != "No") {
    testthat::test_file(here("tests/test-scripts/value-tests.R"))
    testthat::test_file(here("tests/test-scripts/plot-tests.R"))
  }
}
