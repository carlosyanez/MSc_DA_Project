
install.packages("geogrid")
install.packages("geojsonsf")

library(sf)
library(tidyverse)
library(here)
library(fs)
library(geogrid)
library(geojsonsf)



vic_electorates <- st_read(path(here(),"2. Meetings","1. Meeting","vic-july-2021-esri","E_VIC21_region.shp")) %>%
                   st_zm()

new_cells <- calculate_grid(shape = vic_electorates, grid_type = "hexagonal", seed = i)
resulthex <- assign_polygons(vic_electorates, new_cells)
                    

st_write(resulthex, path(here(),"2. Meetings","1. Meeting","vic_hex.geojson"))
