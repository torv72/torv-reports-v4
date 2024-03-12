

# Test that the values pulled into the report match the manually checked values
load(here::here("tests", "test-reports", testing_report, "reference_data.RData"),
     envir = .GlobalEnv)

for(tested_object in setdiff(
  # regex to substitute _ref only if at end of string
  # looking for all _ref values and checking their non _ref counterparts exist
  gsub("_ref$", "", grep("_ref", ls(pos = ".GlobalEnv"),
                         value = T)),
  # Excluding functions & looping object
  c(lsf.str(pos = ".GlobalEnv"),
    "tested_object"))) {
  test_that("All the objects required by the report are computed by the R script", {
    expect_equal(any(class(try(eval(parse(text = tested_object)), silent = T)) == "try-error"),
                 FALSE,
                 label = paste0("*", tested_object, "* was created without causing an error"))
  }
  )
}


for(tested_object in setdiff(
  # regex to substitute _ref only if at end of string
  # looking for all _ref values and checking their non _ref counterparts exist
  gsub("_ref$", "", grep("_ref", ls(pos = ".GlobalEnv"),
                         value = T)),
  # Excluding functions, plots (tested as their outputs) & looping object
  c(lsf.str(pos = ".GlobalEnv"),
    "tested_object",
    "growth_potential_plot",
    "legend_plot",
    "greens_ph_trends",
    "combined_trends"))) {
  # non existent objects are flagged by the previous test; they break this test
  # "all" accounts for objects with two classes (e.g. data.table and data.frame)
  if (all(class(try(eval(parse(text = tested_object)), silent = T)) != "try-error")) {
    test_that("The objects required for the report (that we already know exist!) have the expected class ", {
      expect_equal(class(eval(parse(text = tested_object))),
                   class(eval(parse(text = paste0(tested_object, "_ref")))),
                   label = paste0("*", gsub("_ref", "", tested_object),
                                  "* is ",
                                  toupper(class(eval(parse(text = tested_object)))),
                                  "; it should be ",
                                  toupper(class(eval(parse(text = paste0(tested_object, "_ref"))))),
                                  " --"))
    }
    )

    # exclude ones that failed the previous test from this one
    test_that("The objects  required for the report (that exist and are of the right class!) are identical to the reference objects", {
      expect_equal(eval(parse(text = tested_object)),
                   eval(parse(text = paste0(tested_object, "_ref"))),
                   label = paste0("*", gsub("_ref", "", tested_object),
                                  "* does not match its expected value --"))
    }
    )
  }
}
