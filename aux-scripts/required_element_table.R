# Define branded_table function at the top
branded_table <- function(header_colour = torv_orange, 
                          body_colour = "#f6cba2",
                          data) {
  data %>%
    flextable() %>%
    bold(part = "header") %>%
    border_remove() %>%
    width(j = 1:7,
          width = 0.89) %>%
    align(part = "all",
          align = "center") %>%
    font(part = "all",
         fontname = typeface) %>%
    flextable::color(i = 1, color = header_colour, part = "header") %>%
    border(i = 1,
           border.top = officer::fp_border(width = 1.5, color = header_colour), part = "body") %>% 
    {
      if("Area" %in% names(data)) {
        bold(., j = "Area", part = "body") %>%
          border(., i = ~ `Area` == "Average",
                 border.top = officer::fp_border(width = 1.5, color = torv_gray), part = "body") %>%
          bold(., i = ~ `Area` == "Average")
      } else {
        .
      }
    }
}

# Create tables function
create_required_element_tables <- function(soil_type) {
  # Set size variables based on format
  if (html) {
    size_h <- 16
    size_b <- 15
    size_f <- 11.5
    w <- 1
  } else {
    size_h <- 11.5
    size_b <- 10.5
    size_f <- 7
    w <- 1
  }
  
  # Create table
  table <- filter(fertilizer_table_all, sample_description_number_1 == soil_type) %>%
    select(Area:Mn) %>%
    branded_table(data = .) %>%
    add_footer_lines(values = c(
      "\nThe symbol \"—\" means no deficit was found; an empty cell means no data was provided.",
      "",  # empty line for spacing
      "The \"Average\" values are the means across all areas where a deficit was found.\n"
    )) %>%
    ftExtra::colformat_md(part = "header") %>%
    fontsize(size = size_h, part = "header") %>%
    fontsize(size = size_b, part = "body") %>%
    fontsize(size = size_f, part = "footer") %>%
    font(fontname = typeface_condensed_medium, part = "all") %>%
    font(fontname = typeface, part = "footer") %>%
    flextable::color(color = torv_gray, part = "footer") %>%
    width(width = w) %>%
    flextable::align(align = "left", part = "all")
  
  # Save as PNG
  flextable::save_as_image(table, 
                           path = here(root_figure_location, "soil_testing", glue::glue("required_element_{soil_type}.png")))
}

# Map over all soil types
purrr::map(soil_types, create_required_element_tables)