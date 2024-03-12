# Setting up tests for Reports ----

## Manual process:
## - Run report and have it checked manually; once satisfied rename to <report-name>_ref.docx
## - Identify key dataframes that make up the tables and other key output in the report

## Helper functions
# This one can also be used to reset the ref PNGs after checking that we're happy with all the changes
create_reference_pngs <- function(report) {
  file.rename(from = grep(".png",
                          # avoid ref_ref_ref files!
                          grep("_ref.png", list.files(here::here("tests", "test-reports", report),
                                     recursive = T, full.names = T), invert = T, value = T),
                          value = T),
              to = gsub(".png", "_ref.png",
                        grep(".png",
                             list.files(here::here("tests", "test-reports", report),
                                        recursive = T, full.names = T),
                             value = T)))
}

create_reference_data <- function(report) {
  for (i in setdiff(c(ls(envir = as.environment(.GlobalEnv))),
                    # don't save functions or objects we're creating within this script
                    c(lsf.str(envir = as.environment(.GlobalEnv)),
                      "test_report", "generate_report_args"))) {
    assign(paste0(i, "_ref"),
           eval(parse(text = i)),
           envir = as.environment(.GlobalEnv))
  }
  save(list = grep("_ref",
                   ls(envir = as.environment(.GlobalEnv)),
                   value = T),
       file = here::here("tests", "test-reports", report, "reference_data.RData"),
       # saving as ascii makes it easier to inspect files (e.g. git diff)
       ascii = T)
}

generate_report_args <- list("MC_202108" = list(.test = "MC_202108"),
                             "Saratoga_202109" = list(.test = "Saratoga_202109"),
                             "Wellshire_202111" = list(.test = "Wellshire_202111"),
                             "Sonnenalp_202111" = list(.test = "Sonnenalp_202111"),
                             "Bartlett_202112" = list(.test = "Bartlett_202112"))

# Run test report to generate figures
for(test_report in names(generate_report_args)){

  print(test_report)

  # Generate the report to get the figures into the relevant test repo
#  generate_report(.test = eval(test_report))
  do.call(generate_report, as.list(generate_report_args[[test_report]]))

  # Create the reference figures by renaming them
  create_reference_pngs(report = test_report)

  # Create reference data
  create_reference_data(report = test_report)
}


update_reference_data <- function(report) {
  save(list = grep("_ref",
                 ls(envir = as.environment(.GlobalEnv)),
                 value = T),
     file = here::here("tests", "test-reports", report, "reference_data.RData"),
     # saving as ascii makes it easier to inspect files (e.g. git diff)
     ascii = T)
}
