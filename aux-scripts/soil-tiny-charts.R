create_tiny_charts <- function(soil_type) { 
  measurements_to_plot <- input_params$measurement_names
  measurements_to_plot <- measurements_to_plot[!measurements_to_plot %in% c("Micronutrients", "Organic Matter (%)")]
  
  soil_data <-
    full_database |>
    filter(site == input_params$site_name) |>
    filter(
      sample_type == "Soil",
      sample_description_number_1 == soil_type,
      measurement_name %in% measurements_to_plot
    ) |> 
    mutate(year = year(date_sample_submitted))
  
  dict_measurement_labs <- tibble::tribble(
    ~measurement_name,          ~measurement_lab,              ~unit,
    "pH",                       "pH",                          "",
    "Total Nitrogen (ppm)",     "DIN",                         "ppm",
    "Potassium (ppm)",          "K<sub>2</sub>O",              "ppm",
    "Phosphorus (ppm)",         "P<sub>2</sub>O<sub>5</sub>",  "ppm",
    "Calcium (ppm)",            "Ca",                          "ppm",
    "Magnesium (ppm)",          "Mg",                          "ppm",
    "Sodium (ppm)",             "Na",                          "ppm",
    "Sulfur (ppm)",             "S",                           "ppm",
    "Iron (ppm)",               "Fe",                          "ppm",      
    "Manganese (ppm)",          "Mn",                          "ppm",
    "Chloride (ppm)",           "Cl",                          "ppm",
    "Boron (ppm)",              "B",                           "ppm",
    "Aluminum (ppm)",           "Al",                          "ppm",
    "Soluble Salts (mmhos/cm)", "TSS",                         "(mmhos/cm)",
    "Copper (ppm)",             "Cu",                          "ppm",
    "Zinc (ppm)",               "Zi",                          "ppm"
  )
  
  if(html) font_size_header <- 5.7 else font_size_header <- 4.9
  
  if(length(unique(soil_data$year)) > 1) {

    latest_years <-
      soil_data |>
      count(year) |>
      arrange(year) |>
      slice_tail(n = 2) |>
      pull(year)

    soil_results <-
      soil_data |>
      filter(year %in% latest_years) |>
      group_by(
        measurement_name,
        year
      ) |>
      summarize(avg_measurement = mean(measurement_result, na.rm = TRUE)) |>
      mutate(
        has_increased = first(avg_measurement) > nth(avg_measurement, 2),
        perc_change = (nth(avg_measurement, 2) - first(avg_measurement)) / first(avg_measurement) * 100,
        perc_change = case_when(
          perc_change > 0.01 & measurement_name == "pH" ~ paste0("+", round(perc_change, 1), "%"),
          perc_change < -0.01 & measurement_name == "pH" ~ paste0(round(perc_change, 1), "% "),
          perc_change > 0.1 & measurement_name != "pH" ~ paste0("+", round(perc_change), "%"),
          perc_change < -0.1 & measurement_name != "pH" ~ paste0(round(perc_change), "%"),
          TRUE ~ "" ## no change
        ),
        lwr = if_else(perc_change == "", avg_measurement - avg_measurement * .2, min(avg_measurement)),
        upr = if_else(perc_change == "", avg_measurement + avg_measurement * .15, max(avg_measurement)),
        has_increased = factor(if_else(perc_change == "", 2, as.numeric(has_increased))),
        previous = first(avg_measurement),
        latest = nth(avg_measurement, 2)
      ) |>
      ungroup() |>
      left_join(dict_measurement_labs, by = join_by(measurement_name)) |>
      mutate(
        measurement_name = factor(measurement_name, levels = measurements_to_plot),
        latest_lab = if_else(measurement_name %in% c("pH", "Total Nitrogen (ppm)"), sprintf("%1.1f", latest), sprintf("%4.0f", latest)),
        previous_lab = if_else(measurement_name %in% c("pH", "Total Nitrogen (ppm)"), sprintf("%1.1f", previous), sprintf("%4.0f", previous))
      )

    fig_h <- (length(measurements_to_plot) - 1) %/% 5 * 1.3 + 1

    tiny_charts <-
      ggplot(soil_results, aes(x = year, y = avg_measurement, color = has_increased)) +
      geom_rect(
        aes(xmin = -Inf, xmax = Inf, ymin = lwr - (upr - lwr) * .2, ymax = upr + (upr - lwr) * .2),
        fill = theme_gray_boxes, color = theme_gray_boxes
      ) +
      geom_line(linewidth = .8) +
      geom_segment(
        aes(x = latest_years[1], xend = latest_years[2], y = previous, yend = latest),
        arrow = arrow(type = "closed", length = unit(.075, "npc"))
      ) +
      ggtext::geom_richtext(
        aes(label = measurement_lab, x = latest_years[1] - .15, y = upr + (upr - lwr) * .8),
        stat = "unique",
        family = typeface_ultrabold, size = font_size_header, hjust = 0, vjust = 1,
        label.color = NA, label.padding = unit(rep(0, 4), "pt")#, color = "grey20"
      ) +
      ggtext::geom_richtext(
        aes(label = latest_lab, x = latest_years[2] + .1, y = upr + (upr - lwr) * .8),
        stat = "unique",
        family = typeface_ultrabold, size = font_size_header, hjust = 1, vjust = 1,
        label.color = NA, label.padding = unit(rep(0, 4), "pt")#, color = "grey20"
      ) +
      geom_label(
        aes(label = previous_lab, x = latest_years[1] - .15, y = previous),
        stat = "unique",
        family = typeface_condensed, fontface = "bold", size = 3.9,
        fill = theme_gray_boxes, label.size = 0, hjust = 0
      ) +
      geom_text(
        aes(label = perc_change, x = latest_years[2] + .05, y = previous),
        stat = "unique",
        family = typeface_condensed, fontface = "bold", size = 3.9, hjust = 1
      ) +
      facet_wrap(
        ~ measurement_name,
        scales = "free_y", ncol = 5
      ) +
      scale_x_continuous(expand = expansion(mult = c(0, 0))) +
      scale_y_continuous(expand = expansion(mult = c(0, 0))) +
      scale_color_manual(values = c("0" = clr_darken(torv_green, .3), "1" = clr_darken(torv_orange, .3), "2" = torv_gray), guide = "none") +
      labs(x = NULL, y = NULL) +
      theme(
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(),
        strip.text.x = element_blank(),
        panel.spacing.x = unit(1, "lines"),
        panel.spacing.y = unit(1.7, "lines"),
        plot.margin = margin(rep(1, 4))
      )

  } else { ## if only one year
    
    soil_results <-
      soil_data |> 
      group_by(measurement_name) |> 
      summarize(avg_measurement = mean(measurement_result, na.rm = TRUE)) |> 
      left_join(dict_measurement_labs) |> 
      mutate(
        measurement_name = factor(measurement_name, levels = measurements_to_plot),
        latest_lab = if_else(measurement_name %in% c("pH", "Total Nitrogen (ppm)"), sprintf("%1.1f", avg_measurement), sprintf("%4.0f", avg_measurement))
      )
    
    fig_h <- (length(measurements_to_plot) - 1) %/% 5 * .45 + .45
    
    tiny_charts <- 
      ggplot(soil_results, aes(y = 1)) +
      geom_rect(
        aes(xmin = -Inf, xmax = Inf, ymin = .9, ymax = .92),
        fill = theme_gray_boxes, color = theme_gray_boxes
      ) +
      ggtext::geom_richtext(
        aes(label = measurement_lab, x = 1),
        stat = "unique",
        family = typeface_ultrabold, size = font_size_header, hjust = 0, vjust = 1, 
        label.color = NA, fill = NA, label.padding = unit(rep(0, 4), "pt"), color = torv_gray
      ) +
      ggtext::geom_richtext(
        aes(label = latest_lab, x = 2),
        stat = "unique",
        family = typeface_ultrabold, size = font_size_header, hjust = 1, vjust = 1, 
        label.color = NA, fill = NA, label.padding = unit(rep(0, 4), "pt"), color = torv_gray
      ) +
      facet_wrap(
        ~ measurement_name,
        scales = "free_y", ncol = 5
      ) +
      scale_x_continuous(expand = expansion(mult = c(0, 0))) +
      scale_y_continuous(expand = expansion(mult = c(-.01, -.005))) +
      labs(x = NULL, y = NULL) +
      theme(
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(),
        strip.text.x = element_blank(),
        panel.spacing.x = unit(2, "lines"),
        panel.spacing.y = unit(1.2, "lines"),
        plot.margin = margin(rep(1, 4))
      )
    
  }
  
  ggsave(here(root_figure_location, "soil_testing", glue::glue("tiny_chart_{soil_type}.png")),
         tiny_charts, width = 6.8, height = fig_h, bg = "white", dpi = 500)
}

purrr::map(soil_types, create_tiny_charts)