### This file downloads samples of all the data mentioned in the proposal

#just for reference
library(tidyverse)

box::use(
  cropgrowdays[get_silodata]
)


### Historical weather data from BOM ----
## Note: The Libs were founded on October 1944

sample_historical_melb <- get_silodata(latitude = "-37.8136", 
                           longitude = "144.9631",
                           email = "carlos.yanez.s@gmail.com", 
                           START = "19441001", FINISH = "20200531")




### Census Data ----

abs_catalogues <- tibble(catalogue=readabs::show_available_catalogues())



### Electoral Data ----
## https://results.aec.gov.au/27966/Website/HouseDownloadsMenu-27966-Csv.htm

#Download data for Victoria ----

temp <- tempfile()
download.file("https://results.aec.gov.au/27966/Website/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-27966-VIC.txt",temp)

vic_elections <- read_tsv(temp,skip = 1)
preferences_flow <- read_tsv("https://results.aec.gov.au/27966/Website/Downloads/HouseTcpFlowByDivisionDownload-27966.txt",skip=1)
