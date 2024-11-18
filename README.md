# TORV Reports

This repository provides a semi-automated workflow for generating interactive and 
static agronomic survey reports for golf courses, designed specifically for TORV, 
LLC. It streamlines the process of managing and analyzing soil test data, allowing 
you to create detailed, customized reports for your clients based on their soil 
sample analysis results. 

The R-based workflow offers flexibility, enabling you to generate reports in 
multiple formats (HTML, PDF) with various customization options. You can adjust 
parameters like the sampling season, analysis method, turfgrass type (warm or 
cool season), and details of the data visualizaitons (e.g., beeswarm plots or 
dot strips) to meet each client’s needs.

**Key steps:**

- [Simple updating of your database](#update-database) with new client and site data. 
- [Automated report generation](#generate-reports) using preset or custom 
  parameters to create static or interactive versions.
- [Preparation of longitudinal summaries](#generate-longitudinal-summaries) for your convenience.

---

## Update Database

Source the `update-database.R` script to update the database.

Before running the code, make sure to 
- update `data/site-codes.csv` with new clients/codes 
- keep the csv files closed (or you'll get a "permission denied" error)

```{r}
source(here::here("aux-scripts/update-database.R"))
```

The code will scan through all the files in `data-raw/lab-reports`. It
will skip ones whose file names are already in the database, so you
won't be overwriting any data you'd previously checked/corrected
manually! 

The lab report files must be named as follows in order to be read
correctly by `update_database()`:

-   `[TestType]_[client-number]_[test-key]_[sample-type]_[date:MMDDYY].xlsx`

For example:

-   `PHYSa_86259_S004_Physical_040422.xls`

The script returns a list of all the files it has skipped in the
console window. Because that list is quite long, you might want to
create an archive folder and store lab reports you've already added to
the database elsewhere. The only real advantage to that it that it will
make your list of skipped files easier to read.

The function makes use of data/site-codes to try to match code names it
finds in the data with the names of sites and the client types. If it
can't find a match, it will add a `??` string so you can go in and
update it manually. Keeping as many of the clients/sites in
`data/site-codes.csv` as you can will reduce the need for manual
changes.

The function also scans through the database to match the test names up
with the types of tests. Where it finds several matches (e.g. pH is used
in several types of tests), it adds all the matches together with a `??`
between them, so you can easily search and correct them.

---

## Generate Reports 

First, source the `generate-report.R` script to load the
`generate_report()` function into the environment.

```{r}
source(here::here("aux-scripts/generate-report.R"))
```

Then run the function, giving it the arguments it needs. This approach
means you never need to edit the YAML in the `report.Rmd` file.


### Function Inputs

#### Required Inputs

- **`.site_name`**:  
  *(str)* The name of the golf course or site for which the report is generated.  

- **`.site_name_abbr`**:  
  *(str, optional)* An abbreviated version of the site name that includes the 
  site ID, formatted as `"id-abbreviation"`. Default is `NULL`.  

- **`.zip_code`**:  
  *(str)* The postal code of the site location.  

- **`.date_sample_submitted`**:  
  *(str)* The date the soil samples were submitted for analysis in `YYYY-MM-DD` format.  

- **`.start_date`**:  
  *(str)* The starting date of the sampling or analysis period in `YYYY-MM-DD` format.  

- **`.end_date`**:  
  *(str, optional)* The ending date of the sampling or analysis period in 
  `YYYY-MM-DD` format. Default is `NULL`.  


#### Optional Inputs

- **`.measurements_add`**:  
  *(vector)* Additional soil properties or chemical parameters to include in the 
  trend line charts. These supplement the default measurements (specified by 
  `.measurements_default`). When specifying additional measurements, ensure the 
  names match exactly as they appear in the database. For example, you can add a 
  single measurement by specifying `.measurements_add = "Aluminum (lbs/ac in)"` 
  or a vector of multiple measurements, e.g. 
  `c("Aluminum (lbs/ac in)", "C/N Ratio As-Is (ppm)")`. Default is `NULL`.

- **`.measurements_default`**:  
  *(vector)* A predefined list of soil properties or chemical parameters analyzed by default:  
  - pH  
  - Organic Matter (%)  
  - Total Nitrogen (ppm)  
  - Potassium (ppm)  
  - Phosphorus (ppm)  
  - Calcium (ppm)  
  - Magnesium (ppm)  
  - Sodium (ppm)  
  - Sulfur (ppm)  
  - Iron (ppm)  
  - Manganese (ppm)  
  - Micronutrients  
  Only modify this input if you wish to remove measurements or reorder them, or 
  to insert additional measurements between the default ones.

- **`.om_seasons`**:  
  *(str)* Specifies the season(s) for organic matter analysis. Options are:  
  - `"all"`: Use data from all seasons.  
  - `"season"`: Automatically determines the relevant season based on `date_sample_submitted`.  
    - **Feb-Jul**: Spring season.  
    - **Aug-Jan**: Autumn season.  
  Default is `"all"`.  

- **`.om_stats`**:  
  *(str)* Refers to the statistical summary method used for organic matter results. 
  Options are:  
  - `"average"`  
  - `"median"`  
  Default is `"average"`.  

- **`.warm_or_cool`**:  
  *(str)* Indicates the turfgrass type, which determines pre-specified optimal 
  growth temperature and variance. Options are:  
  - `"warm"`: Warm-season grasses.  
  - `"cool"`: Cool-season grasses.  

- **`.acid_extract`**:  
  *(str)* Specifies the acid extraction method used in the soil analysis, which 
  translates to different MLSN (Minimum Levels for Sustainable Nutrition Soil) values. Options are:  
  - `"Mehlich"`  
  - `"Olsen"`  

- **`.include_results_interpretation`**:  
  *(str)* Includes an interpretation of the soil analysis results in the report 
  if set to `"Yes"`. Default is `"No"`.  

- **`.include_sand_fraction`**:  
  *(str)* Adds information about the sand fraction of the soil if set to `"Yes"`. 
  Default is `"No"`.  

- **`.draw_beeswarm`**:  
  *(str)* Specifies the type of plot used to show the raw data in the trend line charts:  
  - `"Yes"`: Draw beeswarm plots.  
  - `"No"`: Draw dot strips (points along a vertical axis).  
  Default is `"Yes"`.  

- **`typeface`**:  
  *(str)* The typeface (font) used in the generated report. Default is `"Georama"`. 
  Do not change for consistent use of your corporate typefaces. 

- **`output`**:  
  *(str)* Specifies the format of the report. Options are:  
  - `"html"` to generate a web-based report with interactive graphics
  - `"pdf"` to generate a static report
  - `c("html", "pdf")` (or the reverse order) to generate both formats simultaneously.  
  Default is `"html"`.  

- **`overwrite`**:  
  *(str)* If set to `"Yes"`, skips the "sure to overwrite" confirmation and 
  overwrites any existing reports or auto-generated `.txt` files containing 
  executive summaries. Default is `"No"`. Keep in mind that by setting "Yes" you 
  risk to erase manually edited `.txt` files.


### Example Reports

- **Generating an HTML report for Snowmass Club**:  
  This example generates an HTML report using average results across all seasons 
  for Organic Matter (OM). The raw data values in the trendline charts are 
  represented as beeswarms. The sand fraction section and the interpretation 
  of the results are excluded. These settings are the defaults, so they don't 
  need to be specified in the function call. For an example that explicitly 
  addresses those options, see below.

  ```{r}
  generate_report(
    .site_name = "Snowmass Club", 
    .zip_code = 81615,
    .date_sample_submitted = "2024-09-12", 
    .start_date = "2007-01-01",
    .warm_or_cool = "cool", 
    .acid_extract = "Mehlich",
    .output = "html"
  )
  ```

- **Generating both a PDF and an HTML report for Maroon Creek Club:**
  This example generates both a PDF and an HTML report using median values for 
  the respective season (here "Spring", automatically determined from the 
  `.date_sample_submitted value`). It includes the sand fraction results and 
  does not use beeswarms for the raw data values in the trendline charts.

  ```{r}
  generate_report(
    .site_name = "Maroon Creek Club", 
    .zip_code = 81611,
    .date_sample_submitted = "2024-09-18", 
    .start_date = "1997-01-01",
    .om_seasons = "season", 
    .om_stats = "median", 
    .warm_or_cool = "cool",
    .acid_extract = "Mehlich", 
    .include_results_interpretation = "No",
    .include_sand_fraction = "Yes", 
    .draw_beeswarm = "No", 
    .output = c("pdf","html")
  )
  ```

It will save a file in generated-reports with a file name which includes
the site name and the date_sample_submitted argument. If a file with
that name already exists, it will ask you whether you want to overwrite
it before continuing. If you do want to overwrite a file, please make
sure you close the file before running `generate_report()`.

You will notice the option to add `.end_date`. This is currently set to
NULL, which aligns it with `date_sample_submitted`. If at any point you
want to look at how data from a past report compared to data from the
"future" for that client, you can simply add an `.end_date` argument,
using the same format as the other dates.


---

## Generate Longitudinal Summaries

You can generate a longitudinal summary for any club using the code
below.

First, source this file to load the `generate_longitudinal_summary()`
function into the environment.

```{r}
source(here("aux-scripts/longitudinal-summaries.R"))
```

Then, adjust the `.site_name` and `.start_date` arguments and run the
code to generate your data. The data will live in the
longitudinal-summaries folder.

```{r}
generate_longitudinal_summary(
  .site_name = "Bartlett Hills Golf Course",
  .start_date = "2007-01-01"
)
```
