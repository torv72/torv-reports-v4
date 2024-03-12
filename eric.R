
# Update Database ---------------------------------------------------------

# Source this script to update the database

# The code will scan through all the files in data-raw/lab-reports. It will skip ones
# whose filenames are already in the database, so you won't be overwriting any data
# you'd previously checked/corrected manually! It returns a list of all the files it
# has skipped in the console window. Because that list is quite long, you might want
# to create an archive folder and store lab reports you've already added to the database
# elsewhere. The only real advantage to that it that it will make your list of skipped
# files easier to read.

# It makes use of data/site-codes to try to match codenames it finds in the data with
# the names of sites and the client types. If it can't find a match, it will add a \??\
# string so you can go in and update it manually. Keeping as many of the clients/sites
# in data/site-codes.csv as you can will reduce the need for manual changes. We
# recommend updating data/site-codes.csv with new clients/codes before running the
# database update.

# It also scans through the database to match he test names up with the types of tests.
# Where it finds several matches (e.g. pH is used in several types of tests), it adds
# all the matches together with a \??\ between them, so you can easily search and
# correct them.

# Make sure you don't have the file open, or you'll get a "permission denied" error.
source(here::here("aux-scripts/update-database.R"))


# Generate Reports --------------------------------------------------------

# Run this code to generate reports ...
# First, source this file to load the generate_report() function into the environment.
source(here::here("aux-scripts/generate-report.R"))

# Then run the function, giving it the arguments it needs. This approach means you never
# need to edit the YAML in the report.Rmd file. We have provided an example below, so that
# you can see which arguments need to be filled in and how. Instead of editing the example
# here, you can just type `generate_report(`  into the console and fill in the arguments
# there. Remember you can use TAB to get R to suggest and auto complete the argument names.
# Alternatively, copy the example into the console, and edit the arguments as required in the
# console rather than here. That will avoid you needing to edit this file, which will keep
# the git history cleaner. Once you have completed the arguments in the console, hit ENTER.

# Here's an example:
generate_report(.site_name = "Maroon Creek Club",
                .zip_code = 81611,
                .date_sample_submitted = "2023-05-24",
                .start_date = "2018-01-01",
                .warm_or_cool = "cool",
                .acid_extract = "Mehlich",
                .include_results_interpretation = FALSE)


# It will save a file in generated-reports with a filename which includes the site name
# and the date_sample_submitted argument. If a file with that name already exists, it will
# ask you whether you want to overwrite it before continuing. If you do want to overwrite a
# file, please make sure you close the file before running generate_report().

# If you take a look at the function, you'll noticed there's an extra argument: .test.
# This test variable allows us to easily test the output against know input
# and compare it to the output we've already manually checked. You can ignore that
# argument when compiling reports, as it has a default value of "No".

# You will also notice the option to add .end_date. This is currently set to NULL,
# which aligns it with date_sample_submitted. If at any point you want to look at
# how data from a past report compared to data from the "future" for that client,
# you can simply add an .end_date argument, using the same format as the other dates.


# Generate Longitudinal Summaries -----------------------------------------

# You can generate a longitudinal summary for any club using the code below.
# First, source this file to load the generate_longitudinal_summary() function into the environment.

source(here::here("aux-scripts/longitudinal-summaries.R"))

# Then, adjust the .site_name and .start_date arguments and run the code to generate your data.
# The data will live in the longitudinal-summaries folder. Same as with generate_report(), the
# best way to use this function is to run it directly in the console rather than editing it
# in this file. Either type it in, or copy and past the example before editing. Then hit ENTER.

generate_longitudinal_summary(.site_name = "Bartlett Hills Golf Course",
                              .start_date = "2007-01-01")

# Testing for unintended consequences -------------------------------------

# You can test that the code produces expected outputs based on known inputs by running:
testthat::test_file(here::here("tests/test-scripts/run-all-tests.R"))
# It will result in an output in the console which will show only count of passing and
# failing tests (and warnings). If anything fails, it prints what the fail corresponds to
# for further investigations. This series of tests relies on known past data (the Master
# Database as it stood in September 2022). If we make significant changes to the Master
# Database (e.g. change column names, restructure in some way), we'll need to update
# the file that the tests are using as their starting point. But otherwise this is just
# a way of not letting new historical data confound the testing of whether changes to
# the code have the intended consequences and no inintended ones. Happy testing!


# TODO: tidy up water header file names
# TODO: add test escape from additional OM tests
# TODO: check water header file names
# TODO: check values and averages are as expected
# TODO: align tests with new text automation and plots
# TODO: fix rounding in the automated text (see MC_202106 test report)
