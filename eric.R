##------------------------------------------------------------------------------

# Step 1: Update Database ------------------------------------------------------
# Run the follopwing code to update your database.
source(here::here("aux-scripts/update-database.R"))
# Note: Make sure you don't have the file open, or you'll get an error.


# Step 2: Generate Report ------------------------------------------------------
# First, source the file to load the generate_report() function...
source(here::here("aux-scripts/generate-report.R"))

# ... and now run the function with the custom inputs:

generate_report(
  .site_name = "Hemingway Golf Course",
  .site_name_abbr = "34498-hemingway",
  .date_sample_submitted = "2025-04-21",
  .start_date = "2017-01-01",
  .om_seasons = "season",
  .om_stats = "average",
  .warm_or_cool = "cool",
  .acid_extract = "Mehlich",
  .include_results_interpretation = "No",
  .include_sand_fraction = "Yes",
  .output = c("pdf")
)

generate_report(
  .site_name = "CommonGround Golf Course",
  .site_name_abbr = "48321-commonground",
  .date_sample_submitted = "2025-04-15",
  .start_date = "2017-01-01",
  .om_seasons = "season",
  .om_stats = "average",
  .warm_or_cool = "cool",
  .acid_extract = "Mehlich",
  .include_results_interpretation = "No",
  .include_sand_fraction = "Yes",
  .output = c("pdf")
)

generate_report(
  .site_name = "Town of Basalt",
  .site_name_abbr = "46061-basalt",
  .date_sample_submitted = "2025-04-17",
  .start_date = "2017-01-01",
  .om_seasons = "season",
  .om_stats = "average",
  .warm_or_cool = "cool",
  .acid_extract = "Mehlich",
  .include_results_interpretation = "Yes",
  .include_sand_fraction = "No",
  .output = c("pdf")
)

generate_report(
  .site_name = "Town of Basalt",
  .site_name_abbr = "46061-basalt",
  .date_sample_submitted = "2025-04-16",
  .start_date = "2017-01-01",
  .om_seasons = "season",
  .om_stats = "average",
  .warm_or_cool = "cool",
  .acid_extract = "Mehlich",
  .include_results_interpretation = "Yes",
  .include_sand_fraction = "No",
  .output = c("pdf")
)

generate_report(
  .site_name = "Greeley Country Club",
  .site_name_abbr = "89424-greeley",
  .date_sample_submitted = "2025-04-15",
  .start_date = "2017-01-01",
  .om_seasons = "season",
  .om_stats = "average",
  .warm_or_cool = "cool",
  .acid_extract = "Mehlich",
  .include_results_interpretation = "Yes",
  .include_sand_fraction = "No",
  .output = c("html","pdf")
)

generate_report(
  .site_name = "Crown Mountain Park",
  .site_name_abbr = "93893-crown",
  .date_sample_submitted = "2025-04-03",
  .start_date = "2017-01-01",
  .om_seasons = "season",
  .om_stats = "average",
  .warm_or_cool = "cool",
  .acid_extract = "Mehlich",
  .include_results_interpretation = "Yes",
  .include_sand_fraction = "No",
  .output = c("pdf")
)

generate_report(
  .site_name = "Town of Basalt",
  .site_name_abbr = "46061-basalt",
  .date_sample_submitted = "2024-07-26",
  .start_date = "2017-01-01",
  .om_seasons = "season",
  .om_stats = "average",
  .warm_or_cool = "cool",
  .acid_extract = "Mehlich",
  .include_results_interpretation = "Yes",
  .include_sand_fraction = "No",
  .output = c("pdf")
)
generate_report(
  .site_name = "The Valley Club Irwin",
  .site_name_abbr = "89973-valley-irwin",
  .date_sample_submitted = "2024-10-08",
  .start_date = "2017-01-01",
  .om_seasons = "season",
  .om_stats = "average",
  .warm_or_cool = "cool",
  .acid_extract = "Mehlich",
  .include_results_interpretation = "No",
  .include_sand_fraction = "No",
  .output = c("html","pdf")
)
generate_report(
  .site_name = "Maroon Creek Club",
  .site_name_abbr = "35873-maroon",
  .date_sample_submitted = "2024-09-18",
  .start_date = "2017-01-01",
  .om_seasons = "season",
  .om_stats = "median",
  .warm_or_cool = "cool",
  .acid_extract = "Mehlich",
  .include_results_interpretation = "No",
  .include_sand_fraction = "Yes",
  .output = c("pdf", "html")
)
generate_report(
  .site_name = "Snowmass Club",
  .site_name_abbr = "36644-snowmass",
  .date_sample_submitted = "2024-09-12",
  .start_date = "2017-01-01",
  .om_seasons = "season",
  .warm_or_cool = "cool",
  .acid_extract = "Mehlich",
  .include_results_interpretation = "No",
  .include_sand_fraction = "Yes",
  .output = c("pdf","html")
)


# Optional Step: Generate Longitudinal Summary ---------------------------------

# First, source the script...
source(here("aux-scripts/longitudinal-summaries.R"))

# ... and now run the function with the respective inputs:
generate_longitudinal_summary(
  .site_name = "Bartlett Hills Golf Course",
  .start_date = "2007-01-01"
)

##------------------------------------------------------------------------------