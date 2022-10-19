
install.packages("geogrid")
install.packages("geojsonsf")

library(sf)
library(tidyverse)
library(here)
library(fs)
library(geojsonsf)
library(sugarbag)
library(sfheaders)
library(echarts4r)
library(geojson)

vic_electorates <- st_read(path(here(),"2. Meetings","1. Meeting","vic-july-2021-esri","E_VIC21_region.shp")) %>%
                   st_zm() %>%
                   select(Elect_div)


#get order with sugar bag

centroids <- create_centroids(vic_electorates, sf_id = "Elect_div")
grid <- create_grid(centroids = centroids, hex_size = 0.2, buffer_dist = 1.2)

hex_allocated <- allocate(centroids = centroids,
                          sf_id = "Elect_div",
                          hex_grid = grid,
                          hex_size = 0.2, # same size used in create_grid
                          hex_filter = 10,
                          focal_points = capital_cities,
                          width = 30, verbose = TRUE) 

h1 <- hex_allocated %>%
  fortify_hexagon(hex_size = 0.2, sf_id = "Elect_div") 

h2 <- sf_polygon(h1,
           x = "long",
           y = "lat",
           polygon_id = "Elect_div") %>%
      st_buffer(0)


ggplot(h2,aes(geometry=geometry,label=Elect_div)) +
  geom_sf(colour="blue",fill=NA) +
  geom_sf_label(colour="blue",size=2.5)+
  theme_void() 
 

# Manual corrections


json <- geojsonio::geojson_list(vic_electorates %>% rename("name"="Elect_div"))

tibble(Elect=vic_electorates$Elect_div)|>
  mutate(dummy=row_number()) |>
  e_charts(Elect) |>
  e_map_register("VIC", json) |>
  e_map(dummy, map = "VIC") |>
  e_tooltip()|>
  e_datazoom(x_index = 0)

Melbourne -> Menzies
Wills      -> Melbourne
           -> Wills
  
Menzies    ->
Cooper ->




st_write(resulthex, path(here(),"2. Meetings","1. Meeting","vic_hex.geojson"))
