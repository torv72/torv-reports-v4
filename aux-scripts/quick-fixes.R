# Quick fixes
plot_measurement <- water_measurements_to_plot[1]

make_water_plot <- function(plot_measurement) {

  plot_measurement_value <- filtered_database %>%
    filter(sample_type == "Water",
           sample_description_number_1 == "IRRIGATION",
           measurement_name == plot_measurement) %>%
    select(measurement_name, measurement_result)

  if (plot_measurement == "SAR") {

    conductivity_value <- filtered_database %>%
      filter(sample_type == "Water",
             sample_description_number_1 == "IRRIGATION",
             measurement_name == "Conductivity (mmhos/cm)") %>%
      pull(measurement_result)

    sar_value <- plot_measurement_value %>%
      pull(measurement_result)

    water_plot_data <- fao_water_guidelines %>%
      left_join(plot_measurement_value, by = "measurement_name") %>%
      filter(measurement_name == plot_measurement) %>%

      # Adjust SAR values as necessary; see logic in reference-docs/torv-reports-etc/fao-water-ranges.xls
      # e.g. IF the SAR value is 0-3 and the EC is greater than .7, then it is LOW.
      # Setting high and low values based on logic in document and defaulting to mid.
      # Mid-range points are sufficient for this scale.


      mutate(measurement_result_adjusted = case_when(
        # The "high" and "low" cutoff are the other way round compared to the other measurements
        # because of the nature of the logic. So 0.15 is high and 0.95 is low.
        between(sar_value, 0, 3) & conductivity_value > 0.7 ~ .95,
        between(sar_value, 0, 3) & conductivity_value < 0.2 ~ .15,
        between(sar_value, 3.001, 6) & conductivity_value > 1.2 ~ .95,
        between(sar_value, 3.001, 6) & conductivity_value < 0.3 ~ .15,
        between(sar_value, 6.001, 12) & conductivity_value > 1.9 ~ .95,
        between(sar_value, 6.001, 12) & conductivity_value < 0.5 ~ .15,
        between(sar_value, 12.001, 20) & conductivity_value > 2.9 ~ .95,
        between(sar_value, 12.001, 20) & conductivity_value < 1.3 ~ .15,
        # Half way between the two cutoffs
        TRUE ~ 0.5
      ))


  } else {

    water_plot_data <- fao_water_guidelines %>%
      left_join(plot_measurement_value, by = "measurement_name") %>%
      filter(measurement_name == plot_measurement) %>%
      mutate(measurement_result_adjusted = measurement_result)
  }

  cutoff_distance <- water_plot_data$med_high_cutoff - water_plot_data$low_med_cutoff
  label_size <- 8/.pt
  line_size <- .5

  plot_pre_annotate <- water_plot_data %>%
    ggplot(aes(x = measurement_result_adjusted, y = 0,
               label = ifelse(abs(measurement_result) > 100,
                              janitor::round_half_up(measurement_result, 0),
                              signif(measurement_result, 3)))) +
    geom_hline(yintercept = 0, color = "#BFBFBF",
               size = line_size) +
    geom_segment(aes(x = low_med_cutoff, xend = low_med_cutoff,
                     y = -0.5, yend = 0.5),
                 color = "#BFBFBF",
                 size = line_size) +
    geom_segment(aes(x = med_high_cutoff, xend = med_high_cutoff,
                     y = -0.5, yend = 0.5),
                 color = "#BFBFBF",
                 size = line_size) +
    geom_point(size = 10, color = "#119AD6") +
    geom_text(color = "white", fontface = "bold", family = typeface,
              size = 8/.pt) +
    labs(y = str_wrap(plot_measurement, 20)) +
    theme_void() +
    theme(text = element_text(family = typeface),
          axis.title.y = element_text(size = 8))

  if (plot_measurement == "Residual Sodium Carbonate (RSC)") {
    lower_bound <- min(water_plot_data$measurement_result, -5)

    xrange <- c(lower_bound, water_plot_data$low_med_cutoff + (water_plot_data$low_med_cutoff - lower_bound))

    plot_post_annotate <- plot_pre_annotate +
      annotate("text",
               x = (lower_bound + water_plot_data$low_med_cutoff)/2, # halfway between left edge and cutoff
               y = -0.5,
               label = "Low",
               color = "#626262",
               family = typeface,
               size = label_size) +
      annotate("text",
               x = (water_plot_data$low_med_cutoff - lower_bound)/2 + water_plot_data$low_med_cutoff, # halfway between right edge and cutoff
               y = -0.5,
               label = "Medium/High",
               color = "#626262",
               family = typeface,
               size = label_size)
  } else if (plot_measurement == "Chloride (ppm)") {
    lower_bound <- 0

    xrange <- c(lower_bound, water_plot_data$low_med_cutoff + (water_plot_data$low_med_cutoff - lower_bound)) # make cutoff in center of plot

    plot_post_annotate <- plot_pre_annotate +
      annotate("text",
               x = (lower_bound + water_plot_data$low_med_cutoff)/2, # halfway between left edge and cutoff
               y = -0.5,
               label = "Low",
               color = "#626262",
               family = typeface,
               size = label_size) +
      annotate("text",
               x = (water_plot_data$low_med_cutoff - lower_bound)/2 + water_plot_data$low_med_cutoff, # halfway between right edge and cutoff
               y = -0.5,
               label = "Medium/High",
               color = "#626262",
               family = typeface,
               size = label_size)


  } else {
    xrange <- c(water_plot_data$low_med_cutoff - cutoff_distance * 2/3,
                water_plot_data$med_high_cutoff + cutoff_distance * 2/3)

    plot_post_annotate <- plot_pre_annotate +
      annotate("text",
               x = water_plot_data$low_med_cutoff - (cutoff_distance)/2,
               y = -0.5,
               label = "Low",
               color = "#626262",
               family = typeface,
               size = label_size) +
      annotate("text",
               x = water_plot_data$low_med_cutoff + (cutoff_distance)/2,
               y = -0.5,
               label = "Medium",
               color = "#626262",
               family = typeface,
               size = label_size) +
      annotate("text",
               x = water_plot_data$med_high_cutoff + (cutoff_distance)/2,
               y = -0.5,
               label = "High",
               color = "#626262",
               family = typeface,
               size = label_size)
  }

  plot_post_annotate +
    coord_cartesian(xlim = xrange,
                    ylim = c(-1, 1), clip = "off")
}


for(plot_measurement in water_measurements_to_plot) {
  print(plot_measurement)

  make_water_plot(plot_measurement)
}

nitrogen_sums_test <- full_database %>%
  filter(measurement_name %in% c("Ammonium (ppm)", "Nitrate (ppm)")) %>%
  group_by(source_filename, date_sample_submitted, sample_type, sample_description_number_1,
           sample_description_number_2, sample_description_number_3, sample_description_number_4) %>%
  add_count() %>% # this allows you to look through the data to see where there was only one measurement, if desired
  mutate(measurement_result = sum(measurement_result),
         measurement_name = "Total Nitrogen (ppm)") %>%
  select(!c(row_number, n)) %>%
  unique()

full_database_test <- full_database %>%
  bind_rows(nitrogen_sums_test)


full_database_test %>%
  filter(site == input_params$site_name,
         measurement_name == "Total Nitrogen (ppm)") %>%
  mutate(year = year(date_sample_submitted),
         year_date = ymd(glue("{year}-01-01")),
         month = month(date_sample_submitted),
         month_date = ymd(glue("{year}-{month}-01"))) %>%
  {
    if(year_range[2] - year_range[1] < 2) {
      group_by(., month_date) %>%
        rename(., plot_date = month_date)
    } else {
      group_by(., year_date) %>%
        rename(., plot_date = year_date)
    }
  } %>%
  summarize(avg_measurement_result = mean(measurement_result, na.rm = TRUE), .groups = "drop") %>% View()


full_database_test <- full_database_test %>%
  filter(site == input_params$site_name,
         sample_type == "Soil",
         sample_description_number_1 == "TEE",
         measurement_name == "Calcium") %>%
  mutate(year = year(date_sample_submitted),
         year_date = ymd(glue("{year}-01-01")),
         month = month(date_sample_submitted),
         month_date = ymd(glue("{year}-{month}-01"))) %>%
  {
    if(year_range[2] - year_range[1] < 2) {
      rename(., plot_date = month_date)
    } else {
      rename(., plot_date = year_date)
    }
  }


plot_range <- if(year_range[2] - year_range[1] < 2) {
  range(as_date(ymd(full_database_test$plot_date)))
} else {
  as_date(date_decimal(full_database_test))
}
