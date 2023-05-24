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
          fill_holes = FALSE,
          use_cache = TRUE) |>
          rename("DivisionNm"= glue("CED_NAME_{year}")) |>
          select(any_of(c("DivisionNm")),matches("STE|STATE"))
  
  
  map_l <- map |>
          leaflet() |>
          addTiles() |>
          addPolygons(fillColor = "blue",
                      fillOpacity = 0.4,
                      label=~DivisionNm)
  
  saveWidget(map_l,
             here("4. Data",glue("CED_{year}.html")),
             libdir = here("4. Data","lib"),
             selfcontained = FALSE)
  
  st_write(map,here("4. Data",glue("CED_{year}.gpkg")),delete_dsn = TRUE)
  
}


## redo for 2021, carving ACT out of Eden-Monaro

ced2021 <- st_read(here("4. Data",glue("CED_2021.gpkg")))

divisions_act <- c("Canberra","Fenner","Bean")

eden <- ced2021 |>
            filter(DivisionNm=="Eden-Monaro")

act <- ced2021 |>
          filter(DivisionNm %in% divisions_act)
act <- act |> st_cast("POLYGON")


act <- act[st_covered_by(act,eden,sparse = FALSE),]
act <- act |> summarise()

eden <- st_difference(eden,act |> st_make_valid()) 

ced2021 <-
  ced2021 |>
  filter(DivisionNm!="Eden-Monaro") |>
  bind_rows(eden)
  
map_l <- ced2021 |>
  leaflet()  |>
  addTiles() |>
  addPolygons(fillColor = "blue",
              fillOpacity = 0.4,
              label=~DivisionNm)

saveWidget(map_l,
           here("4. Data",glue("CED_2021.html")),
           libdir = here("4. Data","lib"),
           selfcontained = FALSE)

st_write(ced2021,here("4. Data",glue("CED_2021.gpkg")),delete_dsn = TRUE)


#upload

library(piggyback)
library(zip)
library(fs)
library(here)

repo           <- "carlosyanez/MSc_DA_Project"
version       <- "data"

#create new release
tryCatch(pb_new_release(repo,version),
         error=function(e){cat("ERROR :",conditionMessage(e), "\n")})


files <- dir_ls(here("4. Data"),regexp = "gpkg")
for(file in files) {
  #list and zip files
  run_folder <- path_dir(file)
  file_base  <- path_file(file)
  
  zip_file <- path(glue::glue("{file}.zip"))
  
  zip(zip_file, file, mode = "cherry-pick")
  
  # upload catalogue items ---
  pb_upload(file = zip_file, repo, version)
  file_delete(zip_file)
}

