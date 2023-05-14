library(aussiemaps)
library(sf)
library(leaflet)
library(htmlwidgets)
library(glue)
library(here)
library(dplyr)

years <- as.numeric(colnames(aussiemaps::list_attributes() |> select(-attributes)))

for(year in years){
  message(year)
  map <- get_map(year=year,
          aggregation = glue("CED_NAME_{year}"),
          filter_table = list_structure(year),
          new_crs = "EPSG:4326",
          simplification_factor = 0.05,
          use_cache = TRUE) |>
          rename("DivisionNm"= glue("CED_NAME_{year}")) |>
          select(any_of(c("DivisionNm")),matches("STE|STATE"))
  
  
  map_l <- map |>
          leaflet()  |>
          addTiles() |>
          addPolygons(fillColor = "blue",
                      fillOpacity = 0.4,
                      label=~DivisionNm)
  
  saveWidget(map_l,
             here("4. Data",glue("CED_{year}.html")),
             libdir = here("4. Data","lib"),
             selfcontained = FALSE)
  
  st_write(map,here("4. Data",glue("CED_{year}.gpkg")))
  
}