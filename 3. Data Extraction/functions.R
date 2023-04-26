###### data extraction functions #####

extract_census_ced <- function(table_number, census_years, levels) {
  
  age_and_sex <- tibble()
  
  for (x in census_years) {
    if (x == "2006") {
      geo_structure_x <- "CED_2007"
    } else{
      geo_structure_x <- "CED"
    }
    
    censusx <- get_census_summary(
      table_number = table_number,
      geo_structure = geo_structure_x,
      selected_years = as.character(x),
      attribute = levels
    ) |>
      filter(str_detect(Unit, "Shipping", TRUE)) |>
      filter(str_detect(Unit, "Usual", TRUE))   |>
      filter(str_detect(Unit, "Not Applicable", TRUE))
    
    censusx <- censusx |>
      calculate_percentage(
        key_col = "Attribute",
        value_col = "Value",
        key_value = "Total",
        percentage_scale = 100
      )
    
    
    age_and_sex <- bind_rows(age_and_sex, censusx)
    
  }
  return(age_and_sex)
}
