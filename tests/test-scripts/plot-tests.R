
# Test that the plots and stylised numbers produced by the report match the manually checked plots
for(reference_png in grep("_ref.png",
                          list.files(here::here("tests", "test-reports", testing_report),
                                     recursive = T, full.names = T),
                          value = T)) {

  short_file_name <- gsub(here("tests", "test-reports", testing_report),
                          "",
                          reference_png)

  # Check the figure exists
  testthat::expect_true(file.exists(gsub("_ref.png", ".png", reference_png)),
                        label = paste("**", gsub("_ref.png", ".png", short_file_name),
                                      "was created when the report was run **"))

  # If the figure exists, check it matches its reference figure
  if(file.exists(gsub("_ref.png", ".png", reference_png))){
    testthat::expect_equal(png::readPNG(gsub("_ref.png", ".png", reference_png)),
                           png::readPNG(reference_png),
                           label = paste(gsub("_ref.png", ".png", short_file_name),
                                         "matches its reference plot"))
  }
}


