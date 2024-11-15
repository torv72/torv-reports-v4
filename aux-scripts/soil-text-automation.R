
# Add commentary per soil type for MLSN-tied measurements ----
mlsn_summary <- deficits_graph_data %>%
  group_by(sample_description_number_1, mehlich_3) %>%
  summarise(mean_result = mean(mean_measurement ),
            aiming_for = unique(aiming_for),
            proportion_below = unique(proportion_in_deficit)) %>%
  mutate(flag = case_when(proportion_below == 0 ~ "ok",
                          TRUE ~ "low"),
         verbal_proportion = case_when(proportion_below == 100 ~ "all the",
                                       proportion_below == 0 ~ "none of the",
                                       proportion_below < 49 ~ "a minority of the",
                                       TRUE ~ "the majority of the"),
         measurement_name = recode(mehlich_3, "K<sub>2</sub>O" = "Potassium (ppm)",
                                   "P<sub>2</sub>O<sub>5</sub>" = "Phosphorus (ppm)",
                                   "Ca" = "Calcium (ppm)",
                                   "Mg" = "Magnesium (ppm)",
                                   "S" = "Sulfur (ppm)",
                                   "Fe" = "Iron (ppm)",
                                   "Mn" = "Manganese (ppm)"),
         # I have kept the commentary very generic. If you want to add something about the
         # consequences of being below the MLSN value, use a more detailed logic like the one
         # in the extra_soil_measurement table blow. (e.g. case_when(measurement == "blah" & flag == "ok" ~ ...))
         commentary = case_when(flag == "ok" ~ str_squish(paste0("The mean ", measurement_name, " measurement is ",
                                                                 ifelse(abs(mean_result) < 10,
                                                                        janitor::round_half_up(mean_result, digits = 1),
                                                                        janitor::round_half_up(mean_result, digits = 0)),
                                                                 " ppm and is above the MLSN value of ",
                                                                 ifelse(abs(aiming_for) < 10,
                                                                        janitor::round_half_up(aiming_for, digits = 1),
                                                                        janitor::round_half_up(aiming_for, digits = 0)),
                                                                 " ppm for all the samples.")),
                                TRUE ~ str_squish(paste0("The mean ", measurement_name, " measurement is ",
                                                         ifelse(abs(mean_result) < 10,
                                                                janitor::round_half_up(mean_result, digits = 1),
                                                                janitor::round_half_up(mean_result, digits = 0)),
                                                         " ppm; it is below the MLSN value of ",
                                                         ifelse(abs(aiming_for) < 10,
                                                                janitor::round_half_up(aiming_for, digits = 1),
                                                                janitor::round_half_up(aiming_for, digits = 0)),
                                                         " in ", verbal_proportion, " samples. ")))) %>%
  select(sample_description_number_1, measurement_name, mean_result, flag, commentary)

# Create commentary for non-MLSN values ----
extra_soil_measurements <- filtered_database %>%
  filter(sample_description_number_1 %in% soil_types &
           # pH and Nitrogen are also in Water measurements;
           # in discussion with Eric, we're restricting to Soil
           sample_type == "Soil") %>%
  group_by(sample_description_number_1, measurement_name) %>%
  summarise(mean_result = mean(measurement_result)) %>%
  filter(measurement_name %in% c(c("pH",
                                   "Organic Matter (%)",
                                   "Total Nitrogen (ppm)",
                                   "Sodium (ppm)",
                                   "Micronutrients"))) %>%
  bind_rows(tibble(sample_description_number_1 = soil_types,
                   measurement_name = "Micronutrients",
                   mean_result = as.numeric(NA))) %>%
  # case_when evaluates things in order: if the first statement isn't true, move
  # on to the next one; so we first deal with cases that need to be flagged
  # and if those aren't true, we move onto the default "All is well" text
  mutate(flag = case_when(measurement_name == "pH" & mean_result < 6.5 ~ "low",
                          measurement_name == "pH" & mean_result > 7.9 ~ "high",
                          measurement_name == "Organic Matter (%)" & mean_result > 4.5 ~ "high",
                          measurement_name == "Total Nitrogen (ppm)" & mean_result < 5 ~ "low",
                          measurement_name == "Total Nitrogen (ppm)" & mean_result > 10 ~ "high",
                          measurement_name == "Sodium (ppm)" & mean_result > 110 ~ "high",
                          TRUE ~ "ok"),
         commentary = case_when(measurement_name == "pH" & mean_result < 6.5 ~
                                  "The pH is below the optimal range of 6.5 to 7.9.",
                                measurement_name == "pH" & mean_result > 7.9 ~
                                  "The pH is above the optimal range of 6.5 to 7.9.",
                                # glue() doesn't like being used inside case_when, so useing paste0() instead
                                # str_squish() gets rid of the extra white space caused by the line breaks
                                # in the text which are just there to make the code easier to read.
                                measurement_name == "pH" ~
                                  str_squish(paste0("The average pH is ",
                                                    janitor::round_half_up(mean_result, 1),
                                                    ".")),
                                measurement_name == "Organic Matter (%)" ~
                                  str_squish(paste0("The average Organic Matter is ",
                                                    janitor::round_half_up(mean_result, 1),
                                                    "%.")),
                                measurement_name == "Total Nitrogen (ppm)" & mean_result < 5 ~
                                  str_squish(paste0("The average total available Nitrogen is ",
                                                    janitor::round_half_up(mean_result, 1), " ppm.")),
                                measurement_name == "Total Nitrogen (ppm)" & mean_result > 10 ~
                                  str_squish(paste0("The average total available Nitrogen is ",
                                                    janitor::round_half_up(mean_result, 1), " ppm.")),
                                measurement_name == "Total Nitrogen (ppm)" ~
                                  str_squish(paste0("The average total available Nitrogen is ",
                                                    janitor::round_half_up(mean_result, 1), " ppm.")),
                                measurement_name == "Sodium (ppm)" & mean_result > 110 ~
                                  str_squish(paste0("The average Sodium is ",
                                                    janitor::round_half_up(mean_result, 0), " ppm.")),
                                measurement_name == "Sodium (ppm)" ~
                                  str_squish(paste0("The average Sodium is ",
                                                    janitor::round_half_up(mean_result, 0), " ppm.")),
                                measurement_name == "Micronutrients" ~ str_squish("All Micronutrients are present and there are no recommendations for
                                                                                  additional applications.")))



# Bind all commentary together ----
# |- This will form the basis of the bullet points in the body of the report
measurement_commentaries <- bind_rows(mlsn_summary,
                                      extra_soil_measurements)

# |- Summarise measurements needing attention for Executive Summary
exec_summary_commentary <- filter(measurement_commentaries,
                                  flag != "ok")
