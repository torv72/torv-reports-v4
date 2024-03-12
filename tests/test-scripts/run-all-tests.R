# Run all the tests in one go

source(here::here("aux-scripts/generate-report.R"))

generate_report_args <- list("MC_202106" = list(.test = "MC_202106"),
                             "MC_202108" = list(.test = "MC_202108"),
                             "Saratoga_202109" = list(.test = "Saratoga_202109"),
                             "Wellshire_202111" = list(.test = "Wellshire_202111"),
                             "Sonnenalp_202111" = list(.test = "Sonnenalp_202111"),
                             "Bartlett_202112" = list(.test = "Bartlett_202112"))

for(test_report in names(generate_report_args)) {

  cat("\n\n## Testing ", test_report, "...")
  do.call(generate_report, as.list(generate_report_args[[test_report]]))

}
