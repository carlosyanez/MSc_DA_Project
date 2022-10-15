### This file downloads samples of all the data mentioned in the proposal

#just for reference
library(tidyverse)

box::use(
  here[here],
  fs[path],
  cropgrowdays[get_silodata],
  readabs[show_available_catalogues]
)

this_dir <- path(here(),"2. Meetings","1. Meeting","0. data sample")


### Historical weather data from BOM ----
## Note: The Libs were founded on October 1944

sample_historical_melb <- get_silodata(latitude = "-37.8136", 
                           longitude = "144.9631",
                           email = "carlos.yanez.s@gmail.com", 
                           START = "19441001", FINISH = "20200531")

write_csv(sample_historical_melb,path(this_dir,"sample_bom_mel.csv"))


### Census Data ----

#Census 2021
write_csv(Census2021.DataPack::CED__Ancestry,path(this_dir,"census_ancestry.csv"))
write_csv(Census2021.DataPack::CED__medianTotalHouseholdIncome,path(this_dir,"household_income.csv"))
write_csv(Census2021.DataPack::CED__Age.min_Sex,path(this_dir,"census_age.csv"))
write_csv(Census2021.DataPack::CED__Religion_Denomination_Sex,path(this_dir,"census_religion.csv"))
write_csv(Census2021.DataPack::CED__OnlyEnglishSpokenHome,path(this_dir,"census_english.csv"))





### ABS Data ----

abs_catalogues <- tibble(catalogue=show_available_catalogues())
#CPI data
cpi_data <- read_abs("6401.0")
write_csv(cpi_data,path(this_dir,"sample_cpi_data.csv"))


#migration for Victoria
migration <- download_abs_data_cube("overseas-migration","1.3")
migration_vic <- readxl::read_xlsx(migration,"Table 1.3",skip=13)
write_csv(migration_vic,path(this_dir,"migration_vic.csv"))

### Electoral Data ----
## https://results.aec.gov.au/27966/Website/HouseDownloadsMenu-27966-Csv.htm

#Download data for Victoria ----"

temp <- tempfile()
download.file("https://results.aec.gov.au/27966/Website/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-27966-VIC.txt",temp)

vic_elections <- read_tsv(temp,skip = 1)
write_csv(vic_elections,path(this_dir,"sample_fed_elections_vic_2022.csv"))


preferences_flow <- read_tsv("https://results.aec.gov.au/27966/Website/Downloads/HouseTcpFlowByDivisionDownload-27966.txt",skip=1)
write_csv(preferences_flow,path(this_dir,"sample_fed_elections_preference_flows.csv"))

