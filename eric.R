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
  .site_name = "Maroon Creek Club",
  .zip_code = 81615,
  .date_sample_submitted = "2026-11-12",
  .start_date = "2007-01-01",
  .om_seasons = "all",
  .warm_or_cool = "cool",
  .acid_extract = "Mehlich",
  .include_results_interpretation = "No",
  .include_sand_fraction = "No",
  .output = "html"
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