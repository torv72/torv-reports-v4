
# Add commentary for OM measurements ----

om_no_additional_analyses_commentary <- readLines(here("aux-scripts/om-no-additional-analyses.txt"))

# Only add OM commentary if there are OM measurements in this report

sample_dates_per_type <- 
  all_om_data %>% 
  summarize(n = length(unique(date_sample_submitted)), .by = sample_description_number_2) %>%
  summarize(n = mean(n))

if(sample_dates_per_type$n == 1) {

  all_om_data_comms <- 
    all_om_longitudinal_table %>%
    group_by(sample_description_number_2, depth) %>%
    mutate(reference_only = case_when(max(date_sample_submitted, na.rm = TRUE) == as_date(input_params$date_sample_submitted) ~ "",
                                      TRUE ~ "for reference")) %>%
    rowwise() %>%
    mutate(type_date = paste0(case_when(reference_only == "for reference" ~ paste0("\n- Just for reference, since no **",
                                                                                   sample_description_number_2,
                                                                                   "** samples were taken for this report, t"),
                                        TRUE ~ "\n- T"),
                              "he **", sample_description_number_2, "** samples taken on ", verbaliseR::prettify_date(date_sample_submitted, "US"),
                              " were as follows:"),
           difference = paste0("\n  - At a depth of **", depth, "**, the OM content was **", janitor::round_half_up(avg_measurement_result, 2), "%**."))


  # If there is only one OM measurement, read in text about benefits of repeating the measurement
  om_single_measure_commentary <- readLines(here("aux-scripts/om-single-test-summary.txt"))

} else {

  all_om_data_comms <- 
    all_om_longitudinal_table %>%
    group_by(sample_description_number_2, depth) %>%
    mutate(date_rank = paste0("date_", rank(date_sample_submitted))) %>%
    pivot_wider(names_from = date_rank, values_from = c(sample_description_number_2, date_sample_submitted, depth, avg_measurement_result)) %>%
    mutate(reference_only = case_when(max(date_sample_submitted_date_2, na.rm = TRUE) == as_date(input_params$date_sample_submitted) ~ "",
                                      is.na(date_sample_submitted_date_2) ~ "",
                                      TRUE ~ "for reference")) %>%
    rowwise() %>%
    mutate(type_date = paste0(case_when(reference_only == "for reference" ~ paste0("\n- Just for reference, since no **",
                                                                                   sample_description_number_2_date_1,
                                                                                   "** samples were taken for this report, c"),
                                        is.na(date_sample_submitted_date_2) ~ paste0("\n- The **", sample_description_number_2_date_1,
                                                                                     "** samples taken on ", verbaliseR::prettify_date(date_sample_submitted_date_1, "US"),
                                        " were as follows:"),
                                        TRUE ~ "\n- C"),
                              case_when(!is.na(date_sample_submitted_date_2) ~ paste0("omparing the **", sample_description_number_2_date_1,
                                                                                      "** samples taken on ", verbaliseR::prettify_date(date_sample_submitted_date_2, "US"),
                                                                                      " to their most recent previous samples (",
                                                                                      verbaliseR::prettify_date(date_sample_submitted_date_1, "US"), ")."),
                                        TRUE ~ "")),
           difference = paste0("\n  * At a depth of **", depth_date_1, "**, the OM content ",
                               case_when(
                                 is.na(date_sample_submitted_date_2) ~ paste0("was **", janitor::round_half_up(avg_measurement_result_date_1, 2), "%**."),
                                 janitor::round_half_up(avg_measurement_result_date_1, 2) - janitor::round_half_up(avg_measurement_result_date_2, 2)  > 0 ~
                                           paste0("decreased from ",
                                                  janitor::round_half_up(avg_measurement_result_date_1, 2),
                                                  "% to ",
                                                  janitor::round_half_up(avg_measurement_result_date_2, 2), "%."),
                                         janitor::round_half_up(avg_measurement_result_date_1, 2) - janitor::round_half_up(avg_measurement_result_date_2, 2) < 0 ~
                                           paste0("increased from ",
                                                  janitor::round_half_up(avg_measurement_result_date_1, 2),
                                                  "% to ",
                                                  janitor::round_half_up(avg_measurement_result_date_2, 2), "%."),
                                         janitor::round_half_up(avg_measurement_result_date_1, 2) == janitor::round_half_up(avg_measurement_result_date_2, 2)  ~
                                           paste0("was stable at ",
                                                  janitor::round_half_up(unique(avg_measurement_result_date_1,
                                                         avg_measurement_result_date_2), 2), "%."))),
           commentary = case_when(is.na(date_sample_submitted_date_2) ~ paste0(difference, "."),
                                  TRUE ~ paste0(difference, ", a change of **", janitor::round_half_up(avg_measurement_result_date_2 - avg_measurement_result_date_1, 2), "%**.")))

  if(nrow(om_acc_df) > 0) {

    acc_rate_commentary <- paste0("The OM Accumulation Rate was calculated for ",
                                  verbaliseR::listify(unique(om_acc_df$type)), " samples.")

    om_acc_df <- om_acc_df %>%
      rowwise() %>%
      mutate(commentary = paste0("\n  - At a depth of ", soil_depth, " cm, from ", verbaliseR::prettify_date(start_date),
                                 " to ", verbaliseR::prettify_date(end_date),
                                 " the OM content ", case_when(starting_om - ending_om > 0 ~ paste0("decreased from ", starting_om, "% to ", ending_om, "%."),
                                                         starting_om - ending_om < 0 ~ paste0("increased from ", starting_om, "% to ", ending_om, "%."),
                                                         TRUE ~ paste0("was stable at ", unique(starting_om, ending_om), "%.")),
                                 " This represents an OM Accumulation Rate of ", janitor::round_half_up(accum_rate, 2), " g per kg of soil per year."))

  }

  if(nrow(sand_req_df) > 0) {

    sand_req_commentary <- paste0("The Sand Requirement, based on the OM Accumulation Rate was calculated for ",
                                  verbaliseR::listify(unique(sand_req_df$type)), " samples.")

    sand_req_df <- sand_req_df %>%
      rowwise() %>%
      mutate(commentary = paste0("\n  - Adding **", sand_req, " mm** of sand to **",
                                 depth, " cm** depth by ", verbaliseR::prettify_date(end_date, "US"),
                                 " will result in a total organic matter content of **",
                                 desired_om, "%** if the OM accumulation rate remains at ",
                                 janitor::round_half_up(om_acc_rate, 2), " g/kg"))
  }
}

