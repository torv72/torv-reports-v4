
# Ask Eric for manual input values ----

grass_max_n_per_month_per_1000sqft <- list()

happy_with_input <- "n" # Allows user to check values and enter them again if they made a mistake

message("\nI found analyses for the following types of soil in the database: ",
        paste0(soil_types, collapse = ", "), ".")

if (all(soil_types %in% c("GREEN", "TEE", "ROUGH", "FAIRWAY", "APPROACH"))) { # check if uncommon soil types are included in the data
  
  message("\n❓ Do you want to use the default Grass Maximum N/month lb/1000 sq ft values for the deficit tables?")
  happy_with_input <- readline("Type y for yes or n for no and hit ENTER: ")
  
  while(!happy_with_input %in% c("y", "n")) {
    message("\n\n❌ Sorry, I didn't understand your answer.\n")
    happy_with_input <- readline("Please type y for yes or n for no and hit ENTER: ")
  }
  
  if (happy_with_input == "y") {
    grass_max_n_per_month_per_1000sqft[["GREEN"]] <- .5
    grass_max_n_per_month_per_1000sqft[["TEE"]] <- .5
    grass_max_n_per_month_per_1000sqft[["ROUGH"]] <- .7
    grass_max_n_per_month_per_1000sqft[["FAIRWAY"]] <- .7
    grass_max_n_per_month_per_1000sqft[["APPROACH"]] <- .7
    
    message("\n✅ Great, I'll use these default values: ",
            paste0(gsub("list\\(|\\)", "", list(grass_max_n_per_month_per_1000sqft[soil_types]))), ".\n")
  }
} 

while (happy_with_input == "n") {
  #message("\n\nI need some information from you in order to compile the MLSN deficit tables.")
  
  message("\n❓ Please specify the custom value for each soil type (e.g. 0.85) and hit ENTER.")

  for (soil_type in soil_types){

    grass_max_n_per_month_per_1000sqft[[soil_type]] <- as.numeric(
      readline(paste0("What is the value for soil type ", soil_type, "?   ")))
  }

  message("\nYou have provided the following values: ",
          paste0(gsub("list\\(|\\)", "", list(grass_max_n_per_month_per_1000sqft)), ".\n\n"),
          "❓ Are these correct?")

  
  happy_with_input <- readline("Type y for yes or n for no and hit ENTER: ")
}

while (!happy_with_input %in% c("y", "n")) {
  message("\n\n❌ Sorry, I didn't understand your answer.\n")
  happy_with_input <- readline("Please type y for yes or n for no and hit ENTER: ")
}

if (happy_with_input == "n") message("\n🔁 No problem, let's try that again!")
  
# Use these values and the values in the database to recreate all the rest! ----

# Hard coded values
elemental_ratios <- tribble(~element, ~ratio,
                            "N", 1,
                            "K", .5,
                            "P", .125,
                            "Ca", .1,
                            "Mg", .0625,
                            "S", .075,
                            "Fe", .005,
                            "Mn", .001875)

if (tolower(input_params$acid_extract) == "mehlich") {

  mlsn_table <- tribble(~mehlich_3, ~ppm,
                        "N", NA,
                        "K", 37,
                        "P", 21,
                        "Ca", 331,
                        "Mg", 47,
                        "S", 6,
                        "Fe", 44,
                        "Mn", 6)
}

if (tolower(input_params$acid_extract) == "olsen") {
  mlsn_table <- tribble(~mehlich_3, ~ppm,
                        "N", NA,
                        "K", 37,
                        "P", 6,
                        "Ca", 331,
                        "Mg", 47,
                        "S", 6,
                        "Fe", 44,
                        "Mn", 6)
}

# Translate warm or cool from YAML into their respective values

if(tolower(input_params$warm_or_cool) == "warm") {
  optimum_growth_temperature <- 88
  optimum_growth_variance <- 12
}

if(tolower(input_params$warm_or_cool) == "cool") {
  optimum_growth_temperature <- 68
  optimum_growth_variance <- 10
}

climate_all_types <- tibble()

soil_type_for_growth_model <- ifelse("GREEN" %in% soil_types,
                                     "GREEN",
                                     soil_types[1])

for(soil_type in soil_types) {

  climate <- nearest_station_data %>%
    { # this allows for climate_station_data in the tests to just be a tibble
      # containing the data we need
      if("sf" %in% class(nearest_station_data)) {
        sf::st_drop_geometry(.)
      } else {
        .
      }
    } %>%
    select(Jan:Dec) %>%
    pivot_longer(Jan:Dec, names_to = "month", values_to = "temperature") %>%
    mutate(month = factor(month, levels = month.abb),
           growth_potential = 100 *
             exp(-0.5 * ((temperature - optimum_growth_temperature)/optimum_growth_variance)^2)) %>%
    mutate(N = growth_potential*grass_max_n_per_month_per_1000sqft[[soil_type]]/100)

  # Only need to do this once, because the Growth Potential values are identical across types
  if(soil_type == soil_type_for_growth_model) {
    growth_potential_plot <-
      climate %>%
      mutate(
        condition = case_when(growth_potential >= 50 ~ "good", growth_potential < 10 ~ "limited", TRUE ~ "stressed"),
        label_a = if_else(round(growth_potential, digits = 1) > 0, paste0(sprintf("%2.1f", growth_potential), "%"), ""),
        label_b = if_else(round(growth_potential, digits = 1) > 0, paste0("(", condition, ")"), ""),
        tooltip = paste0(sprintf("%2.1f", growth_potential), "%<br><span style='font-size:12pt;'>(", condition, ")</span>")
      ) |> 
      ggplot(aes(x = month, y = round(growth_potential, 0))) +
      geom_hline(yintercept = 10, color = "#F9D4C9", linewidth = .4) +
      geom_hline(yintercept = 50, color = "#F2F1C6", linewidth = .4) +
      geom_hline(yintercept = 100, color = "#B5CBA5", linewidth = .4) +
      geom_col(fill = "white", width = .8) +
      geom_col_interactive(aes(fill = condition, data_id = month, tooltip = tooltip), width = .8) +
      geom_hline(yintercept = 0, color = torv_gray_light, linewidth = .6) +
      coord_cartesian(clip = "off", expand = FALSE) +
      scale_fill_manual(values = c("good" = torv_green, "stressed" = "#686617", "limited" = "#7C230D"), guide = "none") +
      labs(x = NULL, y = NULL, title = "Monthly Growth Potential") +
      theme(
        panel.grid.major.x = element_blank(),
        plot.title = element_text(size = rel(.95), margin = margin(5, 0, 10, 0)),
        plot.title.position = "plot",
        axis.text.x = element_text(size = rel(1.2), margin = margin(t = 5)),
        axis.text.y = element_text(color = c("#7C230D", "#686617", torv_green))
      )
    
    if(!html) {
      growth_potential_plot <-
        growth_potential_plot + 
        geom_text(
          aes(label = label_a, color = condition),
          family = typeface_condensed, 
          fontface = "bold", size = 2.7, vjust = -1.5
        ) + 
        geom_text(
          aes(label = label_b),
          family = typeface, 
          size = 1.9, vjust = -.7, color = torv_gray,
        ) +
        scale_y_continuous(
          breaks = c(10, 50, 100), labels = scales::percent_format(scale = 1), expand = expansion(add = c(0, 5))
        ) +
        scale_color_manual(values = c("good" = "#3C6504", "stressed" = "#494802", "limited" = "#5F1400"), guide = "none")
    } else {
      growth_potential_plot <-
        growth_potential_plot +
        scale_y_continuous(
          breaks = c(10, 50, 100),labels = scales::percent_format(scale = 1), expand = c(0, 0)
        )
    }

    ggsave(here(root_figure_location, "monthly_growth_potential_plot.png"), 
           growth_potential_plot, width = 6.5, height = 4.1, bg = "white", dpi = 500)

    annual_N_per_1000sqft <- sum(climate$N)
    
    saveRDS(growth_potential_plot, here(root_figure_location, "growth_potential_plot.Rds"))

  }


  for(elmt in setdiff(elemental_ratios$element, "N")) {
    climate <- mutate(climate, "{elmt}" := filter(elemental_ratios, element == elmt)$ratio * N)
  }

  climate <- climate %>%
    mutate(month = factor(month, levels = month.abb)) %>%
    summarise(across(N:Mn, sum)) %>%
    pivot_longer(
      cols = N:Mn, names_to = "mehlich_3",
      values_to = "total_lb_per_1000sqft"
    ) %>%
    left_join(mlsn_table, by = "mehlich_3") %>% # specified to reduce console output
    mutate(
      removed_from_soil_ppm = total_lb_per_1000sqft * 32.67,
      plus_mlsn_ppm = removed_from_soil_ppm + ppm
    ) %>%
    mutate(soil_type = soil_type)

  climate_all_types <- rbind(climate_all_types, climate)

}


# apply to values db, matching by soil type, and then pull into report and format as desired

deficit_analysis <- filtered_database %>%
  filter(sample_type == "Soil") %>%
  filter(sample_description_number_1 != "OM") %>%
  # the double {}s are required to escape the pipe
  {
    if(tolower(input_params$acid_extract) == "olsen") {
      mutate(., mehlich_3 =
               case_when(measurement_name == "Potassium (ppm)" ~ "K",
                         measurement_name == "Olsen P (ppm)" ~ "P",
                         measurement_name == "Calcium (ppm)" ~ "Ca",
                         measurement_name == "Magnesium (ppm)" ~ "Mg",
                         measurement_name == "Sulfur (ppm)" ~ "S",
                         measurement_name == "Iron (ppm)" ~ "Fe",
                         measurement_name == "Manganese (ppm)" ~ "Mn",
                         TRUE ~ "Other"))
    } else {
      mutate(., mehlich_3 =
               case_when(measurement_name == "Potassium (ppm)" ~ "K",
                         measurement_name == "Phosphorus (ppm)" ~ "P",
                         measurement_name == "Calcium (ppm)" ~ "Ca",
                         measurement_name == "Magnesium (ppm)" ~ "Mg",
                         measurement_name == "Sulfur (ppm)" ~ "S",
                         measurement_name == "Iron (ppm)" ~ "Fe",
                         measurement_name == "Manganese (ppm)" ~ "Mn",
                         TRUE ~ "Other"))
    }
  }%>%
  filter(mehlich_3 %in% climate_all_types$mehlich_3) %>%
  left_join(climate_all_types, by = c("sample_description_number_1" = "soil_type", "mehlich_3")) %>%
  mutate(deficit = measurement_result - plus_mlsn_ppm)

deficit_averages <- deficit_analysis %>%
  group_by(sample_description_number_1, mehlich_3) %>%
  # after discussion with Eric, we want the Averages to bw "Average across areas where there is a deficit"
  summarise(deficit = mean(deficit[deficit < 0]))

mixedrank <- function(x) order(gtools::mixedorder(x))

deficit_table_all <- deficit_analysis %>%
  pivot_wider(
    id_cols = c(sample_description_number_1, sample_description_number_2),
    names_from = mehlich_3, values_from = deficit
  ) %>%
  mutate(Area = as.character(sample_description_number_2)) %>%
  select(-sample_description_number_2) %>%
  # Using mixedrank function to allow for alpha numeric combinations (e.g. "blah 1, blah 2, blah 12")
  arrange(mixedrank(Area)) %>%
  rbind(
    pivot_wider(
      deficit_averages, id_cols = sample_description_number_1,
      names_from = mehlich_3, values_from = deficit
    ) %>%
    mutate(Area = "Average")
  ) %>%
  arrange(sample_description_number_1)

# Turn deficits into fertilizer amount, using formulae in spreadsheet
fertilizer_table_all <- 
  deficit_table_all %>%
  mutate(
    S = -S/33,
    P = -P*2.29/33,
    Ca = -Ca/33,
    Mg = -Mg/33,
    K = -K*1.2/33,
    Fe = -Fe/33,
    Mn = -Mn/33
  ) %>%
  # To keep the order of the elements consisttent across tables
  select(c(sample_description_number_1, Area, setdiff(elemental_ratios$element, "N"))) %>%
  mutate( # only keeping values greater than 0
    across(setdiff(elemental_ratios$element, "N"), 
           function(x) ifelse(is.na(x), NA, ifelse(x > 0, round(x, 2), "—")))
  ) %>%
  rename(
    'P~2~O~5~' = P,
    'K~2~O' = K
  )


# Turn 0s and positive values ( = no deficit) into '-' to display deficits
deficit_table_all <- deficit_table_all %>%
  # using ifelse because case_when doesn't accept the change in type (double to character)
  mutate(
    across(setdiff(elemental_ratios$element, "N"), 
           function(x) ifelse(is.na(x), NA, ifelse(x < 0, round(x, 2), "—")))
  )

# Get the data ready to plot and comment on ----
deficits_graph_data <- 
  deficit_analysis %>%
  group_by(sample_description_number_1, sample_description_number_2, mehlich_3) %>%
  summarise(
    mean_measurement = mean(measurement_result),
    aiming_for = unique(plus_mlsn_ppm)
  ) %>%
  mutate(point_color = case_when(
    mean_measurement > aiming_for * 1.01 ~ "#698960",
    mean_measurement > aiming_for * .99 ~ "#d08c47",
    TRUE ~ "#a65d57")
  ) %>%
  group_by(sample_description_number_1, mehlich_3) %>%
  mutate(
    mean_per_soil_type = mean(mean_measurement),
    proportion_in_deficit = sum(mean_measurement < aiming_for * 1.01) / length(mean_measurement)*100
  ) %>%
  ungroup() %>%
  mutate(mean_line_color = case_when(
    mean_per_soil_type > aiming_for * 1.01 ~ "#698960",
    mean_per_soil_type > aiming_for * .99 ~ "#d08c47",
    TRUE ~ "#a65d57")
  ) %>%
  arrange(mixedrank(sample_description_number_2)) %>%
  mutate(
    sample_description_number_2 = factor(sample_description_number_2,
                                         levels = unique(sample_description_number_2)),
    mehlich_3 = factor(mehlich_3,
                       levels = c("K", "P", "Ca", "Mg", "S", "Fe", "Mn"),
                       labels = c("K<sub>2</sub>O", "P<sub>2</sub>O<sub>5</sub>",
                                  "Ca", "Mg", "S", "Fe", "Mn")),
    tooltip = paste0("<span style='font-size:12pt;'>", mehlich_3, " at ", sample_description_number_1, " ", sample_description_number_2, "</span><br><b style=color:", clr_darken(point_color, .3), ";>", mean_measurement, "</b>")
  )

# Plot MLSN deficits per sample ----
create_deficits_graph <- function(soil_type) {
  
  deficits_graph_data_type <- 
    deficits_graph_data |> 
    filter(sample_description_number_1 == soil_type)
    
  n <- length(unique(deficits_graph_data_type$sample_description_number_2))
    
  deficit_graph <-
    ggplot(deficits_graph_data_type) +
    geom_segment_interactive(
      aes(x = sample_description_number_2,
          xend = sample_description_number_2,
          y = aiming_for, yend = mean_measurement, 
          color = stage(point_color, after_scale = clr_darken(color, .3)),
          tooltip = tooltip, data_id = mehlich_3),
      linewidth = .8
    ) +
    geom_hline(
      aes(yintercept = aiming_for),
      linewidth = .8, color = theme_gray_boxes
    ) +
    geom_hline_interactive(
      aes(yintercept = aiming_for,
          tooltip = paste0("<span style='font-size:12pt;'>MLSN</span><br><b style='color:", clr_darken(torv_orange, .3), ";'>", sprintf("%2.1f", aiming_for), "</b>"), 
          data_id = mehlich_3),
      linewidth = .8, color = torv_orange
    ) +
    geom_hline_interactive(
      aes(yintercept = mean_per_soil_type, color = mean_line_color,
          tooltip = paste0("<span style='font-size:12pt;'>Mean</span><br><b style='color:", clr_darken(mean_line_color, .3), ";'>", sprintf("%2.1f", mean_per_soil_type), "</b>"), 
          data_id = mehlich_3),
      linetype = "32", linewidth = .5
    ) +
    geom_point(
      aes(x = sample_description_number_2, y = mean_measurement),
      color = theme_gray_boxes, size = 2.5
    ) +
    geom_point_interactive(
      aes(x = sample_description_number_2, y = mean_measurement, 
          fill = point_color, color = after_scale(clr_darken(fill, .3)),
         # tooltip = tooltip,
         data_id = mehlich_3),
      shape = 21, size = 2.5, stroke = .8
    ) +
    geom_text(
      aes(x = sample_description_number_2, y = mean_measurement, 
          label = round(mean_measurement, 1)),
      family = typeface_condensed, fontface = "bold",
      size = 2.7, vjust = -1.1, color = torv_gray
    ) +
    ggtext::geom_textbox(
      aes(x = Inf,
          y = aiming_for,
          label = glue::glue("<span style='font-size:6pt;'>MLSN<br></span>**{janitor::round_half_up(aiming_for)}**"),
          vjust = case_when(aiming_for > mean_per_soil_type ~ .2, TRUE ~ .9),
          valign = case_when(aiming_for > mean_per_soil_type ~ 0, TRUE ~ 1)),
      hjust = 1.1, halign = .5, stat = "unique",
      family = typeface, lineheight = .85, alpha = .8, color = clr_darken(torv_orange, .3), size = 3,
      box.color = NA, fill = NA, width = unit(2, "lines")
    ) +
    ggtext::geom_textbox(
      aes(x = Inf,
          y = mean_per_soil_type,
          label = glue::glue("<span style='font-size:6pt;'>MEAN<br></span>**{janitor::round_half_up(mean_per_soil_type)}**"),
          color = stage(mean_line_color, after_scale = clr_darken(color, .3)),
          vjust = case_when(aiming_for > mean_per_soil_type ~ .9, TRUE ~ .2),
          valign = case_when(aiming_for > mean_per_soil_type ~ 1, TRUE ~ 0)),
      hjust = 1.1, halign = .5, stat = "unique",
      family = typeface, lineheight = .85, alpha = .8, size = 3,
      box.color = NA, fill = NA, width = unit(2, "lines")
    ) +
    facet_wrap(
      ~ mehlich_3, 
      ncol = 1, scales = "free_y",
      strip.position = "left"
    ) +
    scale_x_discrete(
      expand = expansion(mult = c(1/n, 2/n)),
      #limits = c(.5, max + 1.5), 
      position = "top",
      labels = function(x) glue::glue("<span style='font-size:7.5pt;'>{soil_type}<br></span>**{x}**")
    ) +
    scale_y_continuous(
      expand = expansion(mult = 1), position = "right"
    ) +
    scale_fill_identity() +
    scale_color_identity() +
    labs(x = NULL, y = NULL) +
    theme(
      strip.background = element_rect(color = torv_orange, fill = torv_orange),
      panel.grid = element_line(color = "white", linewidth = .3),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.background = element_rect(color = theme_gray_boxes, fill = theme_gray_boxes),
      strip.text.y.left = ggtext::element_markdown(color = "white", angle = 0, face = "bold"),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x.top = ggtext::element_markdown(color = torv_gray, family = typeface, size = rel(1.8)),
      panel.spacing = unit(.65, "lines")
    )

  ggsave(here(root_figure_location, "soil_testing", glue::glue("MLSN_deficits_{soil_type}_plot.png")),
         deficit_graph, width = 6.5, height = 6.2, bg = "white", dpi = 500)
  
  saveRDS(deficit_graph, here(root_figure_location, "soil_testing", glue::glue("MLSN_deficits_{soil_type}_plot.Rds")))
}

purrr::map(soil_types, create_deficits_graph)
