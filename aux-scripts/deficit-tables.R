
# Ask Eric for manual input values ----

grass_max_n_per_month_per_1000sqft <- list()

if(testing_report == "No") { # Removes the need to type in values in order to run the test

  happy_with_input <- "n" # Allows user to check values and enter them again if they made a mistake

  while(happy_with_input == "n") {
    message("\n\n** I need some information from you in order to compile the MLSN deficit tables.
Please answer the following questions. I'll give you an opportunity to revisit your answers at the end.**")

    message("\n\nIn the database, we found analyses for the following types of soil: ",
            paste0(soil_types, collapse = ", "),
            "\nI will ask you in turn for the Grass Maximum N/month lb/1000 sq ft value for each of these.
        \nPlease type in the value (e.g. 0.85) and hit ENTER.\n")

    for(soil_type in soil_types){

      grass_max_n_per_month_per_1000sqft[[soil_type]] <- as.numeric(
        readline(paste0("What is the value for soil type ", soil_type, "?   ")))
    }

    message("\n\nThank you.
            \n\n* You have provided the following values. *\n\n",
            paste0(gsub("list\\(|\\)", "", list(grass_max_n_per_month_per_1000sqft))),
            "\n")

    happy_with_input <- readline("Are these correct? Type y for yes or n for no and hit ENTER: ")

    while(!happy_with_input %in% c("y", "n")) {
      message("\n\nSorry, I didn't understand your answer.\n")
      happy_with_input <- readline("Please type y for yes or n for no and hit ENTER: ")
    }

    if(happy_with_input == "n") {
      message("\nNo problem, let's try that again!\n")
    } else{
      message("\nGreat, I'll compile the rest of the report!\n\n")
    }
  }
} else if(testing_report == "MC_202106") {

  grass_max_n_per_month_per_1000sqft <- list(GREEN = 0.8,
                                             TEE = 0.71,
                                             FAIRWAY = 0.97,
                                             ROUGH = 0.75)

} else if(testing_report == "MC_202108") {

  grass_max_n_per_month_per_1000sqft <- list(GREEN = 0.37,
                                             TEE = 0.53,
                                             FAIRWAY = 0.47,
                                             ROUGH = 0.53)

} else if(testing_report == "Saratoga_202109") {

  grass_max_n_per_month_per_1000sqft <- list(GREEN = 0.5,
                                             FAIRWAY = 0.5)

} else if(testing_report == "Wellshire_202111") {

  grass_max_n_per_month_per_1000sqft <- list(FAIRWAY = 0.5)

} else if(testing_report == "Sonnenalp_202111") {

  grass_max_n_per_month_per_1000sqft <- list(GREEN = 0.2)

}

# Use these values and the values in the database to recreate all the rest! ----

# Hard coded values
elemental_ratios <- tribble(~element, ~ratio,
                            "N", 1,
                            "K", 0.5,
                            "P", 0.125,
                            "Ca", 0.1,
                            "Mg", 0.0625,
                            "S", 0.075,
                            "Fe", 0.005,
                            "Mn", 0.001875)

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

  climate_data_to_use <-
    if(testing_report == "MC_202106") {
      # This is to provide the same climate data as the data used in the original
      # Maroon Creek June 2021 Excel calculations for testing
      tibble(station_id = "temp",
             Jan = 19.8,
             Feb = 22.2,
             Mar = 30.9,
             Apr = 39.8,
             May = 48.4,
             Jun = 57.5,
             Jul = 63.5,
             Aug = 61.3,
             Sep = 53.3,
             Oct = 42.2,
             Nov = 29.3,
             Dec = 20.0)
    } else {
      nearest_station_data
    }

  climate <- climate_data_to_use %>%
    { # this allows for climate_station_data in the tests to just be a tibble
      # containing the data we need
      if("sf" %in% class(climate_data_to_use)) {
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
      ggplot(aes(x = month, y = growth_potential)) +
      geom_col(fill = torv_green) +
      geom_text(aes(label = glue("{round(growth_potential, 0)}%")),
                nudge_y = 1, vjust = 0,
                family = "Lato") +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank()) +
      labs(x = NULL,
           y = NULL,
           title = "Monthly Growth Potential") +
      # Allowing space for percentage label to be just above 100
      scale_y_continuous(limits = c(0, 110), breaks = seq(0, 100, 25))

    ggsave(here(root_figure_location, "monthly_growth_potential_plot.png"),
           growth_potential_plot, bg = "#ffffff",
           device = png(width = 6.5, height = 4, units = "in", type = "cairo", res = 144))

    annual_N_per_1000sqft <- sum(climate$N)

  }


  for(elmt in setdiff(elemental_ratios$element, "N")) {
    climate <- mutate(climate, "{elmt}" := filter(elemental_ratios, element == elmt)$ratio * N)
  }

  climate <- climate %>%
    mutate(month = factor(month, levels = month.abb)) %>%
    summarise(across(N:Mn, sum)) %>%
    pivot_longer(cols = N:Mn, names_to = "mehlich_3",
                 values_to = "total_lb_per_1000sqft") %>%
    left_join(mlsn_table, by = "mehlich_3") %>% # specified to reduce console output
    mutate(removed_from_soil_ppm = total_lb_per_1000sqft * 32.67,
           plus_mlsn_ppm = removed_from_soil_ppm + ppm) %>%
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
  pivot_wider(id_cols = c(sample_description_number_1, sample_description_number_2),
              names_from = mehlich_3,
              values_from = deficit) %>%
  mutate(Area = as.character(sample_description_number_2)) %>%
  select(-sample_description_number_2) %>%
  # Using mixedrank function to allow for alpha numeric combinations (e.g. "blah 1, blah 2, blah 12")
  arrange(mixedrank(Area)) %>%
  rbind(pivot_wider(deficit_averages, id_cols = sample_description_number_1,
                    names_from = mehlich_3,
                    values_from = deficit) %>%
          mutate(Area = "Average")) %>%
  arrange(sample_description_number_1)

# Turn deficits into fertilizer amount, using formulae in spreadsheet
fertilizer_table_all <- deficit_table_all %>%
  mutate(S = -S/33,
         P = -P*2.29/33,
         Ca = -Ca/33,
         Mg = -Mg/33,
         K = -K*1.2/33,
         Fe = -Fe/33,
         Mn = -Mn/33) %>%
  # To keep the order of the elements consisttent across tables
  select(c(sample_description_number_1, Area, setdiff(elemental_ratios$element, "N"))) %>%
  mutate(across(setdiff(elemental_ratios$element, "N"), function(x) ifelse(is.na(x), NA,
                                                                           # only keeping values greater than 0
                                                                           ifelse(x > 0, round(x, 2),
                                                                                  "-")))) %>%
  rename('P~2~O~5~' = P,
         'K~2~O' = K)


# Turn 0s and positive values ( = no deficit) into '-' to display deficits
deficit_table_all <- deficit_table_all %>%
  # using ifelse because case_when doesn't accept the change in type (double to character)
  mutate(across(setdiff(elemental_ratios$element, "N"), function(x) ifelse(is.na(x), NA,
                                                                           ifelse(x < 0, round(x, 2),
                                                                                  "-"))))

# Get the data ready to plot and comment on ----
deficits_graph_data <- deficit_analysis %>%
  group_by(sample_description_number_1, sample_description_number_2, mehlich_3) %>%
  summarise(mean_measurement = mean(measurement_result),
            aiming_for = unique(plus_mlsn_ppm)) %>%
  mutate(point_color = case_when(mean_measurement > aiming_for * 1.01 ~ torv_green,
                                 mean_measurement > aiming_for * 0.99 ~ torv_orange,
                                 TRUE ~ "#ec4a35")) %>%
  group_by(sample_description_number_1, mehlich_3) %>%
  mutate(mean_per_soil_type = mean(mean_measurement),
         proportion_in_deficit = sum(mean_measurement < aiming_for * 1.01)/
           length(mean_measurement)*100) %>%
  ungroup() %>%
  mutate(mean_line_color = case_when(mean_per_soil_type > aiming_for * 1.01 ~ torv_green,
                                     mean_per_soil_type > aiming_for * 0.99 ~ torv_orange,
                                     TRUE ~ "#ec4a35")) %>%
  arrange(mixedrank(sample_description_number_2)) %>%
  mutate(sample_description_number_2 = factor(sample_description_number_2,
                                              levels = unique(sample_description_number_2)),
         mehlich_3 = factor(mehlich_3,
                            levels = c("K", "P", "Ca", "Mg", "S", "Fe", "Mn"),
                            labels = c("K<sub>2</sub>O", "P<sub>2</sub>O<sub>5</sub>",
                                       "Ca", "Mg", "S", "Fe", "Mn")))

# Plot MLSN deficits per sample ----
create_deficits_graph <- function(soil_type) {

  deficit_graph <- ggplot(filter(deficits_graph_data, sample_description_number_1 == soil_type)) +
    geom_hline(aes(yintercept = aiming_for),
               color = alpha(torv_orange, 0.5)) +
    geom_hline(aes(yintercept = mean_per_soil_type,
                   color = mean_line_color),
               linetype = 3) +
    geom_segment(aes(y = aiming_for, yend = mean_measurement,
                     x = sample_description_number_2,
                     xend = sample_description_number_2),
                 linetype = 1,
                 size = 0.3,
                 color = alpha(torv_orange, 0.5)) +
    geom_point(aes(x = sample_description_number_2,
                   y = mean_measurement,
                   fill = point_color),
               shape = 21,
               color = alpha(torv_orange, 0.5),
               size = 2) +
    scale_fill_identity() +
    scale_color_identity() +
    labs(x = "",
         y = "") +
    ggtext::geom_textbox(aes(x = Inf,
                             y = aiming_for,
                             label = glue::glue("<span style=\"font-size:5pt\">MLSN<br></span>**{janitor::round_half_up(aiming_for)}**"),
                             vjust = case_when(aiming_for > mean_per_soil_type ~ 0.15,
                                               TRUE ~ 0.85),
                             valign = case_when(aiming_for > mean_per_soil_type ~ 0,
                                                TRUE ~ 1)),
                         hjust = 1,
                         halign = 0.5,
                         family = "Lato",
                         lineheight = 0.85,
                         color = alpha(torv_orange, 0.5),
                         box.color = NA,
                         fill = NA,
                         width = unit(2, "lines"),
                         alpha = 0.8,
                         size = 3) +
    ggtext::geom_textbox(aes(x = Inf,
                             y = mean_per_soil_type,
                             label = glue::glue("<span style=\"font-size:5pt\">MEAN<br></span>**{janitor::round_half_up(mean_per_soil_type)}**"),
                             color = mean_line_color,
                             vjust = case_when(aiming_for > mean_per_soil_type ~ 0.85,
                                               TRUE ~ 0.15),
                             valign = case_when(aiming_for > mean_per_soil_type ~ 1,
                                                TRUE ~ 0)),
                         hjust = 1.2,
                         halign = 0.5,
                         family = "Lato",
                         lineheight = 0.85,
                         box.color = NA,
                         fill = NA,
                         width = unit(4, "lines"),
                         alpha = 0.8,
                         size = 3) +
    facet_wrap(.~mehlich_3, ncol = 1, scales = "free_y",
               strip.position = "left") +
    scale_y_continuous(expand = expansion(mult = 1),
                       position = "right") +
    scale_x_discrete(expand = expansion(add = c(0.5, 2.5)),
                     position = "top",
                     labels = function(x)
                       glue::glue("<span style=\"font-size:5pt\">{soil_type}<br></span>**{x}**")) +
    theme(strip.background = element_rect(color = torv_orange, fill = torv_orange),
          panel.grid = element_line(color = "white", size = 0.3),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(colour = "#FFFFFF", fill = "#EC8C351A"),
          strip.text.y.left = ggtext::element_markdown(color = "#FFFFFF", angle = 0, face = "bold"),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x.top = ggtext::element_markdown(color = torv_gray, family = "Lato"),
          panel.spacing = unit(0.75, "lines"))

  ggsave(deficit_graph,
         filename = here::here(root_figure_location, "soil_testing", glue::glue("MLSN_deficits_{soil_type}_plot.png")),
         width = 6.5, height = 5.5, bg = "#FFFFFF")
}

purrr::map(soil_types, create_deficits_graph)
