
generate_report <- function(.site_name,
                            .site_name_abbr = NULL,
                            .date_sample_submitted,
                            .start_date,
                            .end_date = NULL,
                            .measurements_add = NULL, 
                            .measurements_default = c("pH",
                                                      "Organic Matter (%)",
                                                      "Total Nitrogen (ppm)",
                                                      "Potassium (ppm)",
                                                      "Phosphorus (ppm)", 
                                                      "Calcium (ppm)",
                                                      "Magnesium (ppm)",
                                                      "Sodium (ppm)",
                                                      "Sulfur (ppm)",
                                                      "Iron (ppm)",
                                                      "Manganese (ppm)",
                                                      "Micronutrients"),
                            .om_seasons = "all",
                            .om_stats = "average",
                            .warm_or_cool,
                            .acid_extract,
                            .include_results_interpretation = "No",
                            .include_sand_fraction = "No",
                            .draw_beeswarm = "Yes",
                            .typeface = "Georama",
                            .output = "html",
                            .overwrite = "No") {
  
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

  # Check function inputs
  if (!is.character(.site_name)) stop('.site_name should be of type character.')
  if (!is.character(.site_name_abbr)) stop('.site_name_abbr should be of type character.')
  if (!.om_seasons %in% c("all", "season")) stop('.om_seasons should be either "all" or "season".')
  if (!.om_stats %in% c("average", "median")) stop('.om_stats should be either "average" or "median".')
  if (!.warm_or_cool %in% c("warm", "cool")) stop('.warm_or_cool should be either "warm" or "cool".')
  if (!.acid_extract %in% c("Mehlich", "Olsen")) stop('.acid_extract should be either "Mehlich" or "Olsen".')
  if (!.include_results_interpretation %in% c("Yes", "No")) stop('.include_results_interpretation should be either "Yes" or "No".')
  if (!.include_sand_fraction %in% c("Yes", "No", "yes", "no")) stop('.include_sand_fraction should be either "Yes" or "No".')
  if (!.draw_beeswarm %in% c("Yes", "No", "yes", "no")) stop('.draw_beeswarm should be either "Yes" or "No".')
  if (!is.character(.typeface)) stop('.typeface should be of type character.')
  output <- str_to_lower(.output)
  if (any(!(output %in% c("html", "pdf") | output == c("pdf", "html") | .output == c("html", "pdf")))) stop('.output should be either "html", "pdf", or c("html", "pdf").')
  if (!.overwrite %in% c("Yes", "No", "yes", "no")) stop('.overwrite should be either "Yes" or "No".')
  
  # Clears the environment, to avoid cross contamination between successive reports!
  rm(list = setdiff(ls(pos = ".GlobalEnv"),
                    # but keep the functions we need to run reports and update the database
                    c("generate_report", "generate_longitudinal_summary",
                      "create_reference_pngs", "create_reference_data",
                      "generate_report_args")),
     pos = ".GlobalEnv")
  
  # Set site name for folder and files
  if (is.null(.site_name_abbr)) .site_name_abbr <- str_to_lower(str_replace_all(.site_name, " ", "-"))
  
  # Define months based on .om_seasons
  if (.om_seasons == "all") season <- 1:12
  if (.om_seasons == "season") {
    month <- month(.date_sample_submitted)
    ## true season logic
    # if (month %in% 3:5) season <- 3:5
    # if (month %in% 6:8) season <- 6:8
    # if (month %in% 9:11) season <- 9:11
    # if (month %in% c(1, 2, 12)) season <- c(1, 2, 12)
    ## two-period logic
    if (month %in% 2:7) season <- 2:7
    if (month %in% c(1, 8:12)) season <- c(1, 8:12)
  }
  
  # Update measurement names (if needed)
  if(is.null(.measurements_add)) {
    measurement_names <- .measurements_default
  } else {
    measurement_names <- unique(c(.measurements_default, .measurements_add))
  }
  
  # Export to global Env so can be used by other scripts
  typeface <<- .typeface
  output_html <<- "html" %in% output
  output_pdf <<- "pdf" %in% output
  overwrite_report <<- ifelse(.overwrite %in% c("Yes", "yes"), TRUE, FALSE)
  
  # Set up Figure directories if they aren't already there
  figure_dirs <- c("headers", "organic_matter", "soil_testing", "trendlines", "water_testing")
  
  for (figure_dir in figure_dirs) {
    if(!dir.exists(here("report", "figures", figure_dir))) {
      dir.create(here("report", "figures", figure_dir), recursive = TRUE)
    }
  }
  
  root_figure_location <<- file.path("report", "figures")

  # Remove all the figures from report/figures for the same reason
  # Those are only temporary to get them into the word document
  file.remove(grep(".png", grep("_ref.png",
                                list.files(here(root_figure_location), recursive = TRUE, full.names = TRUE),
                                value = TRUE, invert = TRUE), value = TRUE))

  # List of input parameters to be used to generate the report later
  input_params <- list(site_name = .site_name,
                       date_sample_submitted = .date_sample_submitted,
                       start_date = .start_date,
                       measurement_names = measurement_names,
                       warm_or_cool = .warm_or_cool,
                       acid_extract = .acid_extract,
                       season = season,
                       om_seasons = .om_seasons,
                       om_stats = .om_stats,
                       include_results_interpretation = ifelse(.include_results_interpretation %in% c("Yes", "yes"), TRUE, FALSE),
                       include_sand_fraction = ifelse(.include_sand_fraction %in% c("Yes", "yes"), TRUE, FALSE),
                       beeswarm = ifelse(.draw_beeswarm %in% c("Yes", "yes"), TRUE, FALSE))

  # The default is for the end date to be the same as the sample date (so that we're not
  # pulling in data from "the future")
  if(is.null(input_params$end_date)) {
    input_params$end_date <- input_params$date_sample_submitted
  }

  # list returned to the global Env so it can be accessed by the markdown file
  input_params <<- input_params
  
  # Set up report directory and filenames
  report_name_folder <<- paste0(
    input_params$date_sample_submitted, "-", .site_name_abbr
  )
  
  report_name_file <<- paste0(
    "report-",
    report_name_folder, "-",
    input_params$om_seasons, "-",
    input_params$om_stats
  )
  
  report_path_folder <- here("generated-reports", report_name_folder)
  report_name_file_pdf <- paste0(report_name_file, ".pdf")
  report_name_file_html <- paste0(report_name_file, ".html")
  
  if(!dir.exists(report_path_folder)) {
    dir.create(report_path_folder, recursive = TRUE)
  }

  # Ask before overwriting a file!
  if(output_pdf & file.exists(paste0(report_path_folder, "/", report_name_file_pdf)) & overwrite_report == FALSE) {

    message("\n❗ There is already a report called ", report_name_file_pdf, " in the generated-reports folder!\n\n❓ Do you want to overwrite it?")

    overwrite <- readline("Type y for yes or any other letter to exit and hit ENTER.")

    if(overwrite == "n") stop("\n❌ Ok, I'll exit now. Phew!")
  }
  if(output_html & file.exists(paste0(report_path_folder, "/", report_name_file_html)) & overwrite_report == FALSE) {
    
    message("\n❗ There is already a report called ", report_name_file_html, " in the generated-reports folder!\n\n❓ Do you want to overwrite it?")
    
    overwrite <- readline("Type y for yes or any other letter to exit and hit ENTER.")
    
    if(overwrite == "n") stop("\n❌ Ok, I'll exit now. Phew!")
  }
  
  # Generate the report
  if (output_html) {
    library(tufte)
    
    rmarkdown::render(
      input = "report/report.Rmd",
      quiet = FALSE,
      output_file = paste0(report_path_folder, "/", report_name_file_html),
      output_format = "tufte_html",
      output_options = list(css = "theme/style.css", embed_resources = TRUE),
      params = list(html = TRUE)
    )
  } 
  
  if (output_pdf) {
    
    rmarkdown::render(
      input = "report/report.Rmd",
      quiet = FALSE,
      output_file = paste0(report_path_folder, "/", report_name_file_pdf),
      output_format = "pdf_document",
      params = list(html = FALSE)
    )
  }

  # Clean-up
  graphics.off()
  x <- file.remove(here("report", "setup.md"))
  
}
