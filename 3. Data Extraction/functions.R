###### data extraction functions #####

#' @export
extract_census_ced <- function(census_tables, census_years, levels,total_level="Total") {
  
  df <- tibble()
  
  for (x in census_years) {
    if (x == "2006") {
      geo_structure_x <- "CED_2007"
    } else{
      geo_structure_x <- "CED"
    }
    message(x)
    censusx <- get_census_summary(
      census_table = census_tables,
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
        key_value = total_level,
        percentage_scale = 100
      )
    
    
    df <- bind_rows(df, censusx)
    
  }
  return(df)
}

#' @export
extract_census_sa1 <- function(census_tables, census_years, levels,codes,ceds,total_value){
  
  df <- tibble()
  
  for(year in census_years){
    
    if(year=="2006"){
      geo_structure_x <- "CD"
    } else{
      geo_structure_x <- "SA1"
    }
    
    df_i <- get_census_summary(census_table = census_tables,
                                         selected_years = year,
                                         geo_structure=geo_structure_x,
                                         attribute = levels) |>
            filter(str_detect(Unit,"Shipping",TRUE))|>
            filter(str_detect(Unit,"Usual",TRUE))   |>
            filter(str_detect(Unit,"Not Applicable",TRUE)) 
    
    df_i <- df_i                                                  |>
               select(-Unit)                                      |>
               rename(!!codes[as.character(year)]:="Census_Code") |>
               geo_aggregate(
                              values_col="Value",
                              original_geo=codes[as.character(year)],
                              new_geo=ceds[as.character(year)],
                              grouping_col = c("Attribute"),
                              year=year)                      |>
              rename("Unit"=unname(ceds[as.character(year)])) |>
              filter(!is.na(Unit))                            |>
              filter(str_detect(Unit,"Shipping",TRUE))        |>
              filter(str_detect(Unit,"Usual",TRUE))           |>
              filter(str_detect(Unit,"[Aa]pplicable",TRUE))   |>
              calculate_percentage(key_col = "Attribute",
                                      value_col = "Value",
                                      key_value = total_value,
                                      percentage_scale = 100)  |>
              mutate(Year=year)
    
    df <- bind_rows(df,df_i)
    
  }
  
  return(df)
  
  
}
