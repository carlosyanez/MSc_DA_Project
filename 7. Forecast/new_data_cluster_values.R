## script to create consolidated dataset with selected variables


#load libraries, tidyverse, here and DBI ------
library(tidyverse)
library(here)
library(DBI)

#load data from 4.Data/consolidated_data.duckdb (duckdb database)
source_db <- dbConnect(duckdb::duckdb(), here("4. Data","consolidated_data.duckdb"))


clusters <- readRDS(here("7. Forecast","results.rds"))
clusters <- clusters$prediction |>
            select(Division,cluster) |>
            mutate(DivisionNm = str_remove_all(Division,"-2022"),
                   election_years="2022",
                   .keep="unused") |>
            left_join(tbl(source_db,"year_equivalency") |> collect(),
                     by=c("election_years"))


clusters <- clusters |>
  mutate(Year=as.numeric(census_years),.keep="unused")

duckdb::duckdb_register(source_db, "clusters", clusters)

#duckdb::duckdb_unregister(source_db,"clusters")


## list available tables
#dbListTables(source_db)

## add citizenship ------


cluster <- tbl(source_db,"citizenship")|>
  filter(Year==2021)           |>
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

cluster_values <- cluster |>
  collect() |>
  pivot_longer(-c(Year,cluster),
               names_to = "attribute",
               values_to="cluster.value")
  
division <- readRDS(here("7. Forecast","results.rds"))
division <- division$new_data |>
             mutate(DivisionNm = str_remove_all(Division,"-2022"),
                     Year=2021,
             .keep="unused",
             .before=1) 

division <- division |>
            pivot_longer(-c(DivisionNm,Year,cluster,StateAb,Metro_Area,Metro), names_to="attribute",values_to = "value") |>
            mutate(cluster=as.numeric(as.character(cluster)))


division <- division |>
            left_join(cluster_values |>
                        select(-Year) |>
                        mutate(attribute=str_replace_all(attribute," - ","_"),
                               attribute=str_replace_all(attribute," ","_")),
                      by=c("cluster","attribute")) |>
            mutate(diff = value-cluster.value) |>
            select(-value,-cluster.value)   |>
            pivot_wider(names_from = attribute,values_from = diff)
  

division |>  
  write_csv(here("4. Data","cluster_values_2021.csv"))
