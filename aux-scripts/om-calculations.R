
# OM and sand functions - from https://github.com/micahwoods/om246-shiny/ ----

## returns bulk_density given OM
bulk_density <- function(om) {
  bd <- 100 / ((om / .22) + ((100 - om) / 1.56))
  return(bd)
}

## sand effect given om1, om2, depth, and sand added
calculate_accum_rate <- function(sand,
                                 depth,
                                 om1,
                                 om2,
                                 start_date,
                                 end_date) {
  start_mass <- 100 ^ 2 * depth * bulk_density(om1)
  start_om_g <- (om1 * 10) * (start_mass / 1000)
  fraction_remaining <- ((depth * 10) - sand) / (depth * 10)
  end_om_g <- fraction_remaining * start_om_g

  ## if system were steady state, the addition of x amount of sand would leave mass at this
  end_mass <- fraction_remaining * start_mass + (sand * 1.56 * 1000)
  end_om_gkg <- (end_om_g / end_mass) * 1000

  ## this is the crucial value: the expected change in OM for a certain amount of sand
  delta_om <- (om1 * 10) - end_om_gkg

  accum <- om2 * 10 - om1 * 10 + delta_om

  years <- as.numeric((end_date - start_date) / 365)

  accum_rate <- accum / years

  return(accum_rate)
}

## function for topdressing amount
## I adjust this to account for time, om_rate is g/kg/per year
calculate_sand_req <- function(om_now,
                               om_goal,
                               depth,
                               om_rate,
                               now_date,
                               target_date) {
  
  years <- as.numeric(target_date - now_date) / 365
  
  if ((om_goal * 10 - om_rate) < om_now * 10) {
    sand_root <- function(sand) {
      start_mass <- 100 ^ 2 * depth * bulk_density(om_now)
      start_om_g <- (om_now * 10) * (start_mass / 1000)
      fraction_remaining <- ((depth * 10) - sand) / (depth * 10)
      end_mass <- fraction_remaining * start_mass + (sand * 1.56 * 1000)
      end_om_g <- fraction_remaining * start_om_g
      end_om_gkg <- (end_om_g / end_mass) * 1000 + (om_rate * years)
      delta2 <- end_om_gkg - (om_goal * 10)
      return(delta2)
    }
    
    result <- uniroot(
      sand_root,
      lower = 0,
      upper = depth * 10,
      tol = 1e-6
    )
    
    sand_mm <- round(result$root, 1)  # Added rounding to 1 decimal place
    return(sand_mm)
  } else {
    return(0)
  }
}


# This only runs if there is new OM data to report on; of there are fewer than 2 dates, it only returns a sentence to that effect ----

# Create empty tibbles, and add to them with every iteration
om_acc_df <- tibble()
sand_req_df <- tibble()

calculate_om_stats <- function(x, na.rm, fun = input_params$om_stats) {
  if(fun == "average") return(mean(x, na.rm = TRUE))
  if(fun == "median")  return(median(x, na.rm = TRUE))
}

## Set up the data
all_om_data <- 
  full_database %>%
  filter(
    sample_description_number_1 == "OM",
    # The older data in the MASTER DATABASE had OM data as "Soil";
    # Since May 2022 it reads in the sample type as "Physical".
    sample_type %in% c("Soil", "Physical")
  ) %>%
  mutate(
    depth = ifelse(str_detect(sample_description_number_3, "[0-9]-[0-9]"), # depth info either in desc3 or desc4 column
                   sample_description_number_3,
                   sample_description_number_4),
    year = lubridate::year(date_sample_submitted),
    year_date = lubridate::ymd(glue("{year}-01-01")),
    month = lubridate::month(date_sample_submitted),
    month_date = lubridate::ymd(glue("{year}-{month}-01")),
    depth = as.character(glue::glue("{depth} cm"))
  ) %>%
  group_by(measurement_name, depth, sample_description_number_2) %>%
  mutate(torv_avg = calculate_om_stats(measurement_result)) %>%
  ungroup() %>%
  filter(site == input_params$site_name) %>%
  filter(month %in% input_params$season)
  
n_dates <- length(unique(all_om_data$date_sample_submitted))
n_types <- length(unique(all_om_data$sample_description_number_2))
max_n_dates_per_sample <-
  all_om_data |>
  select(date_sample_submitted, sample_description_number_2) |> 
  unique() |> 
  count(sample_description_number_2) |> 
  summarize(n = mean(n)) |> ## > 1 if more than 1 sampling date for any type
  pull(n)

if (n_types == 1 & n_dates > 1) {
  
  message("\n\nI have found several dates for OM data.\n\n❓ Would you like to calculate OM accumulation rates?")
  run_om_accumulation <- readline("Type y for yes or n for no and hit ENTER. ")
  
  while (!run_om_accumulation %in% c("y", "n")) {
    
    message(glue("\n\n❓ I didn't quite get that. Would you like to calculate the OM accumulation rates?\n"))
    run_om_accumulation <- readline("Type y for yes or n for no and hit ENTER. ")
  }

  if (run_om_accumulation == "n") {
    
    message("\n✅️️ Alright, I'll skip the calculation of accumulation rates.")
    
  } else if (run_om_accumulation == "y") {
    
    add_new_depth <- "y"
    
    while(add_new_depth == "y") {
      
      happy_with_input <- "n" # Allows user to check values and enter them again if they made a mistake
      
      while(happy_with_input == "n") {
        
        new_om_accumulation <- list()
        
        message("\n\nPlease answer the following questions using only numbers:")
        
        sample_description_number_2 <- unique(all_om_data$sample_description_number_2)
        
        new_om_accumulation$type <- sample_description_number_2
        new_om_accumulation$soil_depth <- as.numeric(readline("❓ What is the depth of the soil in cm (type the number)? "))
        new_om_accumulation$starting_om <- as.numeric(readline("❓ What is the starting OM%? "))
        new_om_accumulation$ending_om <- as.numeric(readline("❓ What is the ending OM%? "))
        new_om_accumulation$sand_added <- as.numeric(readline("❓ How much sand was added (in mm)? "))
        
        new_om_accumulation$start_date <- readline("❓ What is the start date, corresponding to the starting OM% (as yyyy-mm-dd)? ")
        
        while(is.na(tryCatch(suppressWarnings(lubridate::ymd(new_om_accumulation$start_date))))) {
          
          new_om_accumulation$start_date <- readline("❌ I didn't understand the start date you provided. Please use the format yyyy-mm-dd: ")
          
        }
        
        new_om_accumulation$end_date <- readline("❓ What is the ending date, corresponding to the ending OM% (as yyyy-mm-dd)? ")
        
        while(is.na(tryCatch(suppressWarnings(lubridate::ymd(new_om_accumulation$end_date))))) {
          
          new_om_accumulation$end_date <- readline("❌ I didn't understand the end date you provided. Please use the format yyyy-mm-dd: ")
          
        }
        
        while(as.numeric(lubridate::ymd(new_om_accumulation$end_date) - lubridate::ymd(new_om_accumulation$start_date)) < 1) {
          
          new_om_accumulation$end_date <- readline("❌ The end date is not after the start date. Please type in a new end date (yyyy-mm-dd): ")
        }
        
        message("\n\nThank you.
                \n\nYou have provided the following values:",
                paste0(gsub("list\\(|\\)", "", list(new_om_accumulation))), ".",
                "\n")
        
        happy_with_input <- readline("Are these correct? Type y for yes or n for no and hit ENTER: ")
        
        while(!happy_with_input %in% c("y", "n")) {
          message("\n\n❌ Sorry, I didn't understand your answer.\n")
          happy_with_input <- readline("Please type y for yes or n for no and hit ENTER: ")
        }
        
        if(happy_with_input == "n") {
          message("\n🔁 No problem, let's try that again!\n")
        } else{
          message("\n✅ Great, thank you!\n\n")
        }
      }
      
      new_om_accumulation$accum_rate <- calculate_accum_rate(sand = new_om_accumulation$sand_added,
                                                             depth = new_om_accumulation$soil_depth,
                                                             om1 = new_om_accumulation$starting_om,
                                                             om2 = new_om_accumulation$ending_om,
                                                             start_date = lubridate::ymd(new_om_accumulation$start_date),
                                                             end_date = lubridate::ymd(new_om_accumulation$end_date))
      
      # Add the values and calculation into a tibble for later retrieval
      om_acc_df <- om_acc_df %>%
        bind_rows(new_om_accumulation)
      
      ## Ask about Sand Requirement calculations within the same depth
      ## Ask about Sand Requirement calculations within the same depth
      message(glue("\n\n❓ Would you also like to calculate the sand requirement for {sample_description_number_2}s at depth = ",
                   new_om_accumulation$soil_depth,
                   " cm using the OM accumulation rate I've just calculated (",
                   janitor::round_half_up(new_om_accumulation$accum_rate, 1), " g/kg/year)?\n"))
      
      run_sand_req <- readline("Type y for yes or n for no and hit ENTER. ")
      
      while (!run_sand_req %in% c("y","n")) {
        
        message(glue("\n\n❓ I didn't quite get that. Would you like to calculate the sand requirement for {sample_description_number_2}s?\n"))
        run_sand_req <- readline("Type y for yes or n for no and hit ENTER. ")
        
      }
      
      if (run_sand_req == "n") {
        
        message("\n✅ Alright, I'll skip the calculation of the accumulation rate.")
        
      } else if (run_sand_req == "y") {
        
        new_sand_req <- list()
        
        happy_with_input <- "n" # Allows user to check values and enter them again if they made a mistake
        
        while(happy_with_input == "n") {
          
          message("\n\nPlease answer the following questions using only numbers:\n")
          
          new_sand_req$type <- sample_description_number_2
          new_sand_req$depth <- as.numeric(new_om_accumulation$soil_depth)
          new_sand_req$om_acc_rate <- new_om_accumulation$accum_rate
          new_sand_req$current_om <- as.numeric(readline("❓ What is the current OM%? "))
          new_sand_req$desired_om <- as.numeric(readline("❓ What is the desired OM%? "))
          
          
          new_sand_req$start_date <- readline("❓ What is the start date corresponding to the starting OM% (as yyyy-mm-dd)? ")
          
          while(is.na(tryCatch(suppressWarnings(lubridate::ymd(new_sand_req$start_date))))) {
            
            new_sand_req$start_date <- readline("❌ I didn't understand the start date you provided. Please use the format yyyy-mm-dd: ")
            
          }
          
          new_sand_req$end_date <- readline("❓ What is the target date for achieving the desired OM% (as yyyy-mm-dd)? ")
          
          while(is.na(tryCatch(suppressWarnings(lubridate::ymd(new_sand_req$end_date))))) {
            
            new_sand_req$end_date <- readline("❌ I didn't understand the end date you provided. Please use the format yyyy-mm-dd: ")
            
          }
          
          while(as.numeric(lubridate::ymd(new_sand_req$end_date) - lubridate::ymd(new_sand_req$start_date)) < 1) {
            
            new_sand_req$end_date <- readline("❌ The end date is not after the start date. Please type in a new end date (yyyy-mm-dd): ")
          }
          
          message("\n\nThank you.
                \n\nYou have provided the following values: ",
                  paste0(gsub("list\\(|\\)", "", list(new_sand_req))), ".",
                  "\n")
          
          happy_with_input <- readline("❓ Are these correct? Type y for yes or n for no and hit ENTER: ")
          
          while(!happy_with_input %in% c("y", "n")) {
            message("\n\n❌ Sorry, I didn't understand your answer.\n")
            happy_with_input <- readline("Please type y for yes or n for no and hit ENTER: ")
          }
          
          if(happy_with_input == "n") {
            message("\n🔁 No problem, let's try that again!\n")
          } else{
            
            message("\n✅ Great, thank you!\n\n")
            
            new_sand_req$sand_req <- calculate_sand_req(om_now = new_sand_req$current_om,
                                                        om_goal = new_sand_req$desired_om,
                                                        depth = new_sand_req$depth,
                                                        om_rate = new_sand_req$om_acc_rate,
                                                        now_date = lubridate::ymd(new_sand_req$start_date),
                                                        target_date = lubridate::ymd(new_sand_req$end_date))
            
            # Add the values and calculation into a tibble for later retrieval
            sand_req_df <- sand_req_df %>%
              bind_rows(new_sand_req)
          }
          
        }
      } # close `else if (run_sand_req == "y")`
      
      message(glue("\n\n❓ Would you like to add another OM accumulation calculation for a different depth within {sample_description_number_2}s?\n"))
      
      add_new_depth <- readline("Type y for yes or n for no and hit ENTER: ")
      
      while (!add_new_depth %in% c("y","n")) {
        
        add_new_depth <- readlines("\n\n❌ I didn't quite get that. Type y for yes or n for no and hit ENTER. \n")
        
      }
    } # close `while(add_new_depth == "y")`
  } # else `if (run_om_accumulation == "y")`
  
} else {

  if (max_n_dates_per_sample > 1) {
  
    message("\n\nI have found several dates for OM data.\n\n❓ Would you like to calculate OM accumulation rates?")
    run_om_accumulation <- readline("Type y for yes or n for no and hit ENTER. ")
    
    while (!run_om_accumulation %in% c("y", "n")) {
      
      message(glue("\n\n❓ I didn't quite get that. Would you like to calculate the OM accumulation rates?\n"))
      run_om_accumulation <- readline("Type y for yes or n for no and hit ENTER. ")
    }
    
    if (run_om_accumulation == "n") {
      
      message("\n✅️️ Alright, I'll skip the calculation of accumulation rates.")
      
    } else {
    
      for (sample_description_number_2 in unique(all_om_data$sample_description_number_2)) {
    
        if (n_dates > 1) {
    
          message(glue("\n\n❓ Would you like to calculate the OM accumulation rate for **{sample_description_number_2}**?\n"))
          run_om_accumulation <- readline("Type y for yes or n for no and hit ENTER. ")
          
          while (!run_om_accumulation %in% c("y","n")) {
            
            message(glue("\n\n❌ I didn't quite get that.\n❓ Would you like to calculate the OM accumulation rate for {sample_description_number_2}?\n"))
            run_om_accumulation <- readline("Type y for yes or n for no and hit ENTER. ")
          }
    
          if (run_om_accumulation == "n") {
    
            message("\n✅ No accumulation rates are calculated.")
            
          } else if (run_om_accumulation == "y") {
    
            add_new_depth <- "y"
    
            while(add_new_depth == "y") {
    
              happy_with_input <- "n" # Allows user to check values and enter them again if they made a mistake
    
              while(happy_with_input == "n") {
    
                new_om_accumulation <- list()
    
                new_om_accumulation$type <- sample_description_number_2
    
                message("\n\nPlease answer the following questions using only numbers:")
    
                new_om_accumulation$soil_depth <- as.numeric(readline("❓ What is the depth of the soil in cm (type the number)? "))
                new_om_accumulation$starting_om <- as.numeric(readline("❓ What is the starting OM%? "))
                new_om_accumulation$ending_om <- as.numeric(readline("❓ What is the ending OM%? "))
                new_om_accumulation$sand_added <- as.numeric(readline("❓ How much sand was added (in mm)? "))
    
                new_om_accumulation$start_date <- readline("❓ What is the start date, corresponding to the starting OM% (as yyyy-mm-dd)? ")
    
                while(is.na(tryCatch(suppressWarnings(lubridate::ymd(new_om_accumulation$start_date))))) {
    
                    new_om_accumulation$start_date <- readline("❌ I didn't understand the start date you provided. Please use the format yyyy-mm-dd: ")
      
                  }
      
                  new_om_accumulation$end_date <- readline("❓ What is the ending date, corresponding to the ending OM% (as yyyy-mm-dd)? ")
      
                  while(is.na(tryCatch(suppressWarnings(lubridate::ymd(new_om_accumulation$end_date))))) {
      
                    new_om_accumulation$end_date <- readline("❌ I didn't understand the end date you provided. Please use the format yyyy-mm-dd: ")
      
                  }
      
                  while(as.numeric(lubridate::ymd(new_om_accumulation$end_date) - lubridate::ymd(new_om_accumulation$start_date)) < 1) {
      
                    new_om_accumulation$end_date <- readline("❌ The end date is not after the start date. Please type in a new end date (yyyy-mm-dd): ")
                  }
      
                  message("\n\nThank you.
                  \n\nYou have provided the following values:",
                  paste0(gsub("list\\(|\\)", "", list(new_om_accumulation))), ".",
                  "\n")
      
                  happy_with_input <- readline("Are these correct? Type y for yes or n for no and hit ENTER: ")
      
                  while(!happy_with_input %in% c("y", "n")) {
                    message("\n\n❌ Sorry, I didn't understand your answer.\n")
                    happy_with_input <- readline("Please type y for yes or n for no and hit ENTER: ")
                  }
      
                  if(happy_with_input == "n") {
                    message("\n🔁 No problem, let's try that again!\n")
                  } else{
                    message("\n✅ Great, thank you!\n\n")
                  }
                }
      
                new_om_accumulation$accum_rate <- calculate_accum_rate(sand = new_om_accumulation$sand_added,
                                                                       depth = new_om_accumulation$soil_depth,
                                                                       om1 = new_om_accumulation$starting_om,
                                                                       om2 = new_om_accumulation$ending_om,
                                                                       start_date = lubridate::ymd(new_om_accumulation$start_date),
                                                                       end_date = lubridate::ymd(new_om_accumulation$end_date))
      
                # Add the values and calculation into a tibble for later retrieval
                om_acc_df <- om_acc_df %>%
                  bind_rows(new_om_accumulation)
      
                ## Ask about Sand Requirement calculations within the same depth
                message(glue("\n\n❓ Would you also like to calculate the sand requirement for {sample_description_number_2}s at depth = ",
                             new_om_accumulation$soil_depth,
                             " cm using the OM accumulation rate I've just calculated (",
                             janitor::round_half_up(new_om_accumulation$accum_rate, 1), " g/kg/year)?\n"))
      
                run_sand_req <- readline("Type y for yes or n for no and hit ENTER. ")
      
                while (!run_sand_req %in% c("y","n")) {
      
                  message(glue("\n\n❓ I didn't quite get that. Would you like to calculate the sand requirement for {sample_description_number_2}s?\n"))
                  run_sand_req <- readline("Type y for yes or n for no and hit ENTER. ")
      
                }
      
                if (run_sand_req == "n") {
      
                  message("\n✅ Alright, I'll skip the calculation of the accumulation rate.")
      
                } else if (run_sand_req == "y") {
      
                  new_sand_req <- list()
      
                  happy_with_input <- "n" # Allows user to check values and enter them again if they made a mistake
      
                  while(happy_with_input == "n") {
      
                    message("\n\nPlease answer the following questions using only numbers:\n")
      
                    new_sand_req$type <- sample_description_number_2
                    new_sand_req$depth <- as.numeric(new_om_accumulation$soil_depth)
                    new_sand_req$om_acc_rate <- new_om_accumulation$accum_rate
                    new_sand_req$current_om <- as.numeric(readline("❓ What is the current OM%? "))
                    new_sand_req$desired_om <- as.numeric(readline("❓ What is the desired OM%? "))
      
      
                    new_sand_req$start_date <- readline("❓ What is the start date corresponding to the starting OM% (as yyyy-mm-dd)? ")
      
                    while(is.na(tryCatch(suppressWarnings(lubridate::ymd(new_sand_req$start_date))))) {
      
                      new_sand_req$start_date <- readline("❌ I didn't understand the start date you provided. Please use the format yyyy-mm-dd: ")
      
                    }
      
                    new_sand_req$end_date <- readline("❓ What is the target date for achieving the desired OM% (as yyyy-mm-dd)? ")
      
                    while(is.na(tryCatch(suppressWarnings(lubridate::ymd(new_sand_req$end_date))))) {
      
                      new_sand_req$end_date <- readline("❌ I didn't understand the end date you provided. Please use the format yyyy-mm-dd: ")
      
                    }
      
                    while(as.numeric(lubridate::ymd(new_sand_req$end_date) - lubridate::ymd(new_sand_req$start_date)) < 1) {
      
                      new_sand_req$end_date <- readline("❌ The end date is not after the start date. Please type in a new end date (yyyy-mm-dd): ")
                    }
      
                    message("\n\nThank you.
                  \n\nYou have provided the following values: ",
                  paste0(gsub("list\\(|\\)", "", list(new_sand_req))), ".",
                  "\n")
      
                    happy_with_input <- readline("❓ Are these correct? Type y for yes or n for no and hit ENTER: ")
      
                    while(!happy_with_input %in% c("y", "n")) {
                      message("\n\n❌ Sorry, I didn't understand your answer.\n")
                      happy_with_input <- readline("Please type y for yes or n for no and hit ENTER: ")
                    }
      
                    if(happy_with_input == "n") {
                      message("\n🔁 No problem, let's try that again!\n")
                    } else{
      
                      message("\n✅ Great, thank you!\n\n")
      
                      new_sand_req$sand_req <- calculate_sand_req(om_now = new_sand_req$current_om,
                                                                  om_goal = new_sand_req$desired_om,
                                                                  depth = new_sand_req$depth,
                                                                  om_rate = new_sand_req$om_acc_rate,
                                                                  now_date = lubridate::ymd(new_sand_req$start_date),
                                                                  target_date = lubridate::ymd(new_sand_req$end_date))
      
                      # Add the values and calculation into a tibble for later retrieval
                      sand_req_df <- sand_req_df %>%
                        bind_rows(new_sand_req)
                    }
      
                  }
                } # close `else if (run_sand_req == "y")`
      
                message(glue("\n\n❓ Would you like to add another OM accumulation calculation for a different depth within {sample_description_number_2}s?\n"))
      
                add_new_depth <- readline("Type y for yes or n for no and hit ENTER: ")
      
                while (!add_new_depth %in% c("y","n")) {
      
                  add_new_depth <- readlines("\n\n❌ I didn't quite get that. Type y for yes or n for no and hit ENTER. \n")
      
                }
              } # close `while(add_new_depth == "y")`
            } # else `if (run_om_accumulation == "y")`
          } # close `if (n_dates > 1)`
        } # close `for (sample_description_number_2)`
      } # close `else` for `if (run_om_accumulation == "n")`
    } # close `if (max_n_dates_per_sample > 1)`
  } # close `else` for `if (n_types == 1 & n_dates > 1)`
    
  if (n_dates <= 1) {
    
    message("\nI have only found one date for OM data. I will not ask you about the OM accumulation rate.\n")
    
  }

# Get data for exec summary info

if(length(all_om_data$date_sample_submitted) > 1) {

  all_om_longitudinal_table <- all_om_data %>%
    filter(measurement_name == "OM 440 As Rcvd (%)") %>%
    group_by(sample_description_number_2) %>%
    filter(date_sample_submitted %in% tail(sort(unique(date_sample_submitted)), 2)) %>%
    group_by(sample_description_number_2, date_sample_submitted, depth) %>%
    summarize(avg_measurement_result = calculate_om_stats(measurement_result),
              torv_avg = mean(torv_avg, na.rm = TRUE), # all values same, take mean to collapse
              .groups = "drop")

}
