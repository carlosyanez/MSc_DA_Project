## script to create consolidated dataset with selected variables
   

#load libraries, tidyverse, here and DBI ------
library(tidyverse)
library(here)
library(DBI)

#load data from 4.Data/consolidated_data.duckdb (duckdb database)
source_db <- dbConnect(duckdb::duckdb(), here("4. Data","consolidated_data.duckdb"))

clusters <- read_csv(here("4. Data","clusters.csv"))         |>
            select(-any_of(c("Metro_Area")))                 |>
            mutate(DivisionNm=if_else(DivisionNm=="Fraser (I)","Fraser",DivisionNm)) |>
            mutate(Year=as.character(Year))       |>
            left_join(tbl(source_db,"year_equivalency") |> collect(),
                      by=c("Year"="election_years")) |>
            mutate(Year=as.numeric(census_years),.keep="unused")


duckdb::duckdb_register(source_db, "clusters", clusters)

duckdb::duckdb_unregister(source_db,"clusters")


## list available tables
#dbListTables(source_db)

## add citizenship ------


cluster <- tbl(source_db,"citizenship")|>
          filter(Year!=2021)           |>
          left_join(tbl(source_db,"clusters"),
                    by=c("Unit"="DivisionNm","Year"="Year")) |>
          group_by(Year,cluster) |>
          summarise(Value=sum(Value),
                    Total=sum(Total),
                    .groups="drop")   |>
          mutate(Australian_Citizens=100*Value/Total,.keep="unused") |>
          filter(!is.na(cluster))
  


# add age groups ------

cluster  <- cluster |>
          left_join(
            tbl(source_db,"age") |>
              left_join(tbl(source_db,"clusters"),
                        by=c("Unit"="DivisionNm","Year"="Year")) |>
              group_by(Year,cluster,Attribute)  |>
              summarise(Value=sum(Value),
                        Total=sum(Total),
                        .groups="drop")   |>
              mutate(Percentage=100*Value/Total,.keep="unused") |>
              pivot_wider(names_from=Attribute,values_from=Percentage) |>
              filter(!is.na(cluster)),
            by=c("cluster","Year"))


# add selected languages ------
#"Lang - English Only"                
#[13] "Lang - Chinese"                      "Lang - South Asian"                  "Lang - Arabic"                       "Lang - East Asian"


cluster   <- cluster |>
  left_join(
    tbl(source_db,"language") |>
       left_join(tbl(source_db,"clusters"),
                by=c("Unit"="DivisionNm","Year"="Year")) |>
      group_by(Year,cluster,Attribute)  |>
      summarise(Value=sum(Value),
                Total=sum(Total),
                .groups="drop")   |>
      mutate(Percentage=100*Value/Total,.keep="unused") |>
      pivot_wider(names_from=Attribute,values_from=Percentage) |>
      filter(!is.na(cluster)),
    by=c("cluster","Year"))

#religion ------
#[17] "Rel - Anglican.Uniting.Presbyterian" "Rel - Buddhism"                      "Rel - Catholic"                      "Rel - Christian.Orthodox"           
#[21] "Rel - Hinduism"                      "Rel - Islam"  

cluster   <- cluster |>
  left_join(
    tbl(source_db,"religion") |>
       left_join(tbl(source_db,"clusters"),
                by=c("Unit"="DivisionNm","Year"="Year")) |>
      group_by(Year,cluster,Attribute)  |>
      summarise(Value=sum(Value),
                Total=sum(Total),
                .groups="drop")   |>
      mutate(Percentage=100*Value/Total,.keep="unused") |>
      pivot_wider(names_from=Attribute,values_from=Percentage)|>
      filter(!is.na(cluster)),
    by=c("cluster","Year"))


# income -----


cluster   <- cluster |>
  left_join(
    tbl(source_db,"income") |>
       left_join(tbl(source_db,"clusters"),
                by=c("Unit"="DivisionNm","Year"="Year")) |>
      group_by(Year,cluster,Attribute)  |>
      summarise(Value=sum(Value),
                Total=sum(Total),
                .groups="drop")   |>
      mutate(Percentage=100*Value/Total,.keep="unused") |>
      pivot_wider(names_from=Attribute,values_from=Percentage)|>
      filter(!is.na(cluster)),
    by=c("cluster","Year"))

# housing ------

cluster   <- cluster |>
  left_join(
    tbl(source_db,"household_tenure") |>
       left_join(tbl(source_db,"clusters"),
                by=c("Unit"="DivisionNm","Year"="Year")) |>
      group_by(Year,cluster,Attribute)  |>
      summarise(Value=sum(Value),
                Total=sum(Total),
                .groups="drop")   |>
      mutate(Percentage=100*Value/Total,.keep="unused") |>
      pivot_wider(names_from=Attribute,values_from=Percentage)|>
      filter(!is.na(cluster)),
    by=c("cluster","Year"))

#education ------

cluster   <- cluster |>
  left_join(
    tbl(source_db,"ed_level") |>
       left_join(tbl(source_db,"clusters"),
                by=c("Unit"="DivisionNm","Year"="Year")) |>
      group_by(Year,cluster,Attribute)  |>
      summarise(Value=sum(Value),
                Total=sum(Total),
                .groups="drop")   |>
      mutate(Percentage=100*Value/Total,.keep="unused") |>
      pivot_wider(names_from=Attribute,values_from=Percentage)|>
      filter(!is.na(cluster)),
    by=c("cluster","Year"))


#relationships --------

cluster   <- cluster |>
  left_join(
    tbl(source_db,"relationship") |>
       left_join(tbl(source_db,"clusters"),
                by=c("Unit"="DivisionNm","Year"="Year")) |>
      group_by(Year,cluster,Attribute)  |>
      summarise(Value=sum(Value),
                Total=sum(Total),
                .groups="drop")   |>
      mutate(Percentage=100*Value/Total,.keep="unused") |>
      pivot_wider(names_from=Attribute,values_from=Percentage)|>
      filter(!is.na(cluster)),
    by=c("cluster","Year"))

cluster |>
  collect() |>
  write_csv(here("4. Data","cluster_values.csv"))

#disconnect
dbDisconnect(source_db, shutdown=TRUE)

##upload dataset

rm(list=ls())

library(piggyback)
library(fs)
library(zip)

repo           <- "carlosyanez/MSc_DA_Project"
version       <- "data"

#create new release
tryCatch(pb_new_release(repo,version),
         error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

files <- dir_ls(here("4. Data"),regexp = "csv")
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
