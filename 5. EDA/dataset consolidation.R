## script to create consolidated dataset with selected variables
   

#load libraries, tidyverse, here and DBI ------
library(tidyverse)
library(here)
library(DBI)

#load data from 4.Data/consolidated_data.duckdb (duckdb database)
source_db <- dbConnect(duckdb::duckdb(), here("4. Data","consolidated_data.duckdb"))
dbListTables(source_db)

#tbl(source_db,"citizenship") |> collect() |> View()
#get parties table, filter PartyAb keeping only ALP, COAL and GRN
consolidated <-  tbl(source_db, "primary_vote_relative") |>
  filter(PartyAb %in% c("ALP", "COAL", "GRN","Other")) |>
  arrange(DivisionNm,Year) |>
  select(Year,DivisionNm,PartyAb,StateAb,Percentage_Diff_National) |>
  pivot_wider(names_from = PartyAb,values_from=Percentage_Diff_National)  |>
  left_join(tbl(source_db,"year_equivalency"),
            by=c("Year"="election_years"))  |>
  rename("election_year"="Year","Year"="census_years") |>
  mutate(DivisionNm=case_when(
    Year %in% c(2006,2011) & DivisionNm=="Fraser" ~ "Fraser (I)",
    Year %in% c(2019,2021) & DivisionNm=="Fraser" ~ "Fraser (II)",
    TRUE ~ DivisionNm
  ))  |>
  mutate(Unit=str_to_lower(DivisionNm))  
  
## electorate + state for state summaries

electorate_state <- consolidated |> 
                    select(DivisionNm,StateAb,Year) |>
                    collect()

duckdb::duckdb_register(source_db, "electorate_state", electorate_state)



## list available tables
#dbListTables(source_db)

## add citizenship ------
consolidated <- 
consolidated |>
  left_join(tbl(source_db,"citizenship") |> 
              mutate(Unit=str_to_lower(Unit)) |> 
              rename("Australian_Citizens"="Percentage") |>
              select(Unit,Year,Australian_Citizens),
            by=c("Year"="Year","Unit"="Unit")) 

nationals <-  tbl(source_db,"citizenship") |>
               group_by(Year) |>
               summarise(Value=sum(Value),
                         Total=sum(Total),
                         .groups="drop")   |>
               mutate(Australian_Citizens=100*Value/Total,.keep="unused")

state <- tbl(source_db,"citizenship")|>
          left_join(tbl(source_db,"electorate_state"),
                    by=c("Unit"="DivisionNm","Year"="Year")) |>
          group_by(Year,StateAb) |>
          summarise(Value=sum(Value),
                    Total=sum(Total),
                    .groups="drop")   |>
          mutate(Australian_Citizens=100*Value/Total,.keep="unused")
  


# add age groups ------
age_groups <-  tbl(source_db,"age") |>
                mutate(Unit=str_to_lower(Unit)) |>
                select(Unit,Year,Attribute,Percentage)  |>
                pivot_wider(names_from = Attribute,values_from = Percentage) |>
                rename_with(.fn= ~ str_replace(.x," - ","_") |> str_replace_all(" ","_"),
                            .cols=contains("Age")) |>
                select(-Age_Greatest_Gen,-Age_Gen_Alpha)

consolidated <- consolidated |>
                left_join(age_groups,by=c("Year"="Year","Unit"="Unit"))

consolidated |> collect() |> nrow()


nationals   <- nationals |>
               left_join(
                 tbl(source_db,"age") |>
                   group_by(Year,Attribute) |>
                   summarise(Value=sum(Value),
                             Total=sum(Total),
                             .groups="drop")   |>
                   mutate(Percentage=100*Value/Total,.keep="unused") |>
                   pivot_wider(names_from=Attribute,values_from=Percentage),
                 by="Year")


state  <- state |>
          left_join(
            tbl(source_db,"age") |>
              left_join(tbl(source_db,"electorate_state"),
                        by=c("Unit"="DivisionNm","Year"="Year")) |>
              group_by(Year,StateAb,Attribute)  |>
              summarise(Value=sum(Value),
                        Total=sum(Total),
                        .groups="drop")   |>
              mutate(Percentage=100*Value/Total,.keep="unused") |>
              pivot_wider(names_from=Attribute,values_from=Percentage),
            by=c("StateAb","Year"))


# add selected languages ------
#"Lang - English Only"                
#[13] "Lang - Chinese"                      "Lang - South Asian"                  "Lang - Arabic"                       "Lang - East Asian"

language <- tbl(source_db,"language") |>
  mutate(Unit=str_to_lower(Unit)) |> 
  mutate(Attribute=str_replace_all(Attribute," - ","_"),
         Attribute=str_replace_all(Attribute," ","_")) |>
  select(Unit,Attribute,Year,Percentage)|>
  pivot_wider(names_from = "Attribute",values_from = "Percentage")

consolidated <- consolidated |>
  left_join(language,by=c("Year"="Year","Unit"="Unit")) 

consolidated |> collect() |> nrow()


nationals   <- nationals |>
  left_join(
    tbl(source_db,"language") |>
      group_by(Year,Attribute) |>
      summarise(Value=sum(Value),
                Total=sum(Total),
                .groups="drop")   |>
      mutate(Percentage=100*Value/Total,.keep="unused") |>
      pivot_wider(names_from=Attribute,values_from=Percentage),
    by="Year")


state   <- state |>
  left_join(
    tbl(source_db,"language") |>
      left_join(tbl(source_db,"electorate_state"),
                by=c("Unit"="DivisionNm","Year"="Year")) |>
      group_by(Year,StateAb,Attribute)  |>
      summarise(Value=sum(Value),
                Total=sum(Total),
                .groups="drop")   |>
      mutate(Percentage=100*Value/Total,.keep="unused") |>
      pivot_wider(names_from=Attribute,values_from=Percentage),
    by=c("StateAb","Year"))

#religion ------
#[17] "Rel - Anglican.Uniting.Presbyterian" "Rel - Buddhism"                      "Rel - Catholic"                      "Rel - Christian.Orthodox"           
#[21] "Rel - Hinduism"                      "Rel - Islam"  

religion <- tbl(source_db,"religion") |>
  mutate(Unit=str_to_lower(Unit)) |>
  mutate(Attribute=str_replace_all(Attribute," - ","_"),
         Attribute=str_replace_all(Attribute,"-","_"),
         Attribute=str_replace_all(Attribute," ","_")) |>
  filter(Attribute %in% c(
                          "Religion_Anglican_Uniting_Presbyterian",
                          "Religion_Buddhism",
                          "Religion_Catholic","Religion_Christian_Orthodox",
                          "Religion_Hinduism","Religion_Other_Christianity",
                          "Religion_No_Religion_Secular","Religion_Islam")) |>
  select(Unit,Attribute,Year,Percentage)|>
  pivot_wider(names_from = "Attribute",values_from = "Percentage") 
  
consolidated <- consolidated |>
  left_join(religion,by=c("Year"="Year","Unit"="Unit")) 

consolidated |> collect() |> nrow()

nationals   <- nationals |>
  left_join(
    tbl(source_db,"religion") |>
      group_by(Year,Attribute) |>
      summarise(Value=sum(Value),
                Total=sum(Total),
                .groups="drop")   |>
      mutate(Percentage=100*Value/Total,.keep="unused") |>
      pivot_wider(names_from=Attribute,values_from=Percentage),
    by="Year")

state   <- state |>
  left_join(
    tbl(source_db,"religion") |>
      left_join(consolidated |> select(DivisionNm,StateAb,Year),
                by=c("Unit"="DivisionNm","Year"="Year")) |>
      group_by(Year,StateAb,Attribute)  |>
      summarise(Value=sum(Value),
                Total=sum(Total),
                .groups="drop")   |>
      mutate(Percentage=100*Value/Total,.keep="unused") |>
      pivot_wider(names_from=Attribute,values_from=Percentage),
    by=c("StateAb","Year"))


# income -----
income <- tbl(source_db,"income") |>
  mutate(Unit=str_to_lower(Unit)) |> 
  mutate(Attribute=str_replace_all(Attribute," - ","_"),
         Attribute=str_replace_all(Attribute," ","_")) |>
  select(Unit,Attribute,Year,Percentage)|>
  pivot_wider(names_from = "Attribute",values_from = "Percentage")

consolidated <- consolidated |>
  left_join(income,by=c("Year"="Year","Unit"="Unit")) 

consolidated |> collect() |> nrow()

nationals   <- nationals |>
  left_join(
    tbl(source_db,"income") |>
      group_by(Year,Attribute) |>
      summarise(Value=sum(Value),
                Total=sum(Total),
                .groups="drop")   |>
      mutate(Percentage=100*Value/Total,.keep="unused") |>
      pivot_wider(names_from=Attribute,values_from=Percentage),
    by="Year")

state   <- state |>
  left_join(
    tbl(source_db,"income") |>
      left_join(consolidated |> select(DivisionNm,StateAb,Year),
                by=c("Unit"="DivisionNm","Year"="Year")) |>
      group_by(Year,StateAb,Attribute)  |>
      summarise(Value=sum(Value),
                Total=sum(Total),
                .groups="drop")   |>
      mutate(Percentage=100*Value/Total,.keep="unused") |>
      pivot_wider(names_from=Attribute,values_from=Percentage),
    by=c("StateAb","Year"))

# housing ------
housing <-
  tbl(source_db,"household_tenure") |>
  mutate(Unit=str_to_lower(Unit)) |> 
  mutate(Attribute=str_replace_all(Attribute," - ","_"),
         Attribute=str_replace_all(Attribute," ","_")) |>
  select(Unit,Attribute,Year,Percentage)|>
  pivot_wider(names_from = "Attribute",values_from = "Percentage")
  
consolidated <- consolidated |>
  left_join(housing,by=c("Year"="Year","Unit"="Unit")) 

consolidated |> collect() |> nrow()

nationals   <- nationals |>
  left_join(
    tbl(source_db,"household_tenure") |>
      group_by(Year,Attribute) |>
      summarise(Value=sum(Value),
                Total=sum(Total),
                .groups="drop")   |>
      mutate(Percentage=100*Value/Total,.keep="unused") |>
      pivot_wider(names_from=Attribute,values_from=Percentage),
    by="Year")

state   <- state |>
  left_join(
    tbl(source_db,"household_tenure") |>
      left_join(consolidated |> select(DivisionNm,StateAb,Year),
                by=c("Unit"="DivisionNm","Year"="Year")) |>
      group_by(Year,StateAb,Attribute)  |>
      summarise(Value=sum(Value),
                Total=sum(Total),
                .groups="drop")   |>
      mutate(Percentage=100*Value/Total,.keep="unused") |>
      pivot_wider(names_from=Attribute,values_from=Percentage),
    by=c("StateAb","Year"))

#education ------
education <-
tbl(source_db,"ed_level") |>
  mutate(Unit=str_to_lower(Unit)) |> 
  mutate(Attribute=str_replace_all(Attribute," - ","_"),
         Attribute=str_replace_all(Attribute," ","_")) |>
  filter(Attribute!="Education_Inadequately_Described") |>
  select(Unit,Attribute,Year,Percentage) |>
  pivot_wider(names_from = "Attribute",values_from = "Percentage") 

consolidated <- consolidated |>
  left_join(education,by=c("Year"="Year","Unit"="Unit")) 

consolidated |> collect() |> nrow()

nationals   <- nationals |>
  left_join(
    tbl(source_db,"ed_level") |>
      group_by(Year,Attribute) |>
      summarise(Value=sum(Value),
                Total=sum(Total),
                .groups="drop")   |>
      mutate(Percentage=100*Value/Total,.keep="unused") |>
      pivot_wider(names_from=Attribute,values_from=Percentage),
    by="Year")

state   <- state |>
  left_join(
    tbl(source_db,"ed_level") |>
      left_join(consolidated |> select(DivisionNm,StateAb,Year),
                by=c("Unit"="DivisionNm","Year"="Year")) |>
      group_by(Year,StateAb,Attribute)  |>
      summarise(Value=sum(Value),
                Total=sum(Total),
                .groups="drop")   |>
      mutate(Percentage=100*Value/Total,.keep="unused") |>
      pivot_wider(names_from=Attribute,values_from=Percentage),
    by=c("StateAb","Year"))


#relationships --------
relationships <-
tbl(source_db,"relationship") |>
  mutate(Unit=str_to_lower(Unit)) |> 
  mutate(Attribute=str_replace_all(Attribute," - ","_"),
         Attribute=str_replace_all(Attribute," ","_"),
         Attribute=str_replace_all(Attribute,"-","_")) |>
  filter(Attribute!="Relationship_Visitor") |>
  select(Unit,Attribute,Year,Percentage) |>
  pivot_wider(names_from = "Attribute",values_from = "Percentage") 


consolidated <- consolidated |>
  left_join(relationships,by=c("Year"="Year","Unit"="Unit")) 

consolidated |> collect() |> nrow()

nationals   <- nationals |>
  left_join(
    tbl(source_db,"relationship") |>
      group_by(Year,Attribute) |>
      summarise(Value=sum(Value),
                Total=sum(Total),
                .groups="drop")   |>
      mutate(Percentage=100*Value/Total,.keep="unused") |>
      pivot_wider(names_from=Attribute,values_from=Percentage),
    by="Year")


state   <- state |>
  left_join(
    tbl(source_db,"relationship") |>
      left_join(consolidated |> select(DivisionNm,StateAb,Year),
                by=c("Unit"="DivisionNm","Year"="Year")) |>
      group_by(Year,StateAb,Attribute)  |>
      summarise(Value=sum(Value),
                Total=sum(Total),
                .groups="drop")   |>
      mutate(Percentage=100*Value/Total,.keep="unused") |>
      pivot_wider(names_from=Attribute,values_from=Percentage),
    by=c("StateAb","Year"))

#fix CED capitalisation

consolidated <-
  consolidated |> 
  mutate(DivisionNm=case_when(
    DivisionNm=="Eden-monaro" ~ "Eden-Monaro",
    DivisionNm=="Kingsford smith" ~ "Kingsford Smith",
    DivisionNm=="La trobe" ~ "La Trobe",
    DivisionNm=="Mcewen" ~ "McEwen",
    DivisionNm=="Mcmahon" ~ "McMahon",
    DivisionNm=="Mcmillan" ~ "McMillan",
    DivisionNm=="Mcpherson" ~ "McPherson",
    DivisionNm=="Melbourne ports" ~ "Melbourne Ports",
    DivisionNm=="New england" ~ "New England",
    DivisionNm=="North sydney" ~ "North Sydney",
    DivisionNm=="O'connor" ~ "O'Connor",
    DivisionNm=="Port adelaide" ~ "Port Adelaide",
    DivisionNm=="Wide bay" ~ "Wide Bay",
    DivisionNm=="Fraser (i)" ~ "Fraser (I)",
    DivisionNm=="Fraser (ii)" ~ "Fraser (II)",
    TRUE ~ DivisionNm
  )) |>
  select(-Unit)



# add metro flag, GCCSA for metro electorates
# bases on 2021, then manually review

metro_non_metro <- tbl(source_db,"metro_electorates") |>
  select(DivisionNm=CED_NAME_2021,Metro_Area=GCCSA_NAME_2021) |>
  distinct() |>
  mutate(initial = str_sub(DivisionNm,1,1),
         DivisionNm = str_remove(DivisionNm,"^[A-z]"),
         DivisionNm = paste0(toupper(initial),DivisionNm)) |>
  select(-initial) |>
  mutate(DivisionNm=case_when(
    DivisionNm=="Eden-monaro" ~ "Eden-Monaro",
    DivisionNm=="Kingsford smith" ~ "Kingsford Smith",
    DivisionNm=="La trobe" ~ "La Trobe",
    DivisionNm=="Mcewen" ~ "McEwen",
    DivisionNm=="Mcmahon" ~ "McMahon",
    DivisionNm=="Mcmillan" ~ "McMillan",
    DivisionNm=="Mcpherson" ~ "McPherson",
    DivisionNm=="Melbourne ports" ~ "Melbourne Ports",
    DivisionNm=="New england" ~ "New England",
    DivisionNm=="North sydney" ~ "North Sydney",
    DivisionNm=="O'connor" ~ "O'Connor",
    DivisionNm=="Port adelaide" ~ "Port Adelaide",
    DivisionNm=="Wide bay" ~ "Wide Bay",
    DivisionNm=="Fraser (ii)" ~ "Fraser (II)",
    DivisionNm=="Fraser" ~ "Fraser (II)",
    TRUE ~ DivisionNm)) |>
  mutate(Metro="Yes") |> 
  collect() |>
  bind_rows(
    tbl(source_db,"non_metro_electorates") |>
      distinct() |>
      select(DivisionNm=CED_NAME_2021)   |>
      mutate(initial = str_sub(DivisionNm,1,1),
             DivisionNm = str_remove(DivisionNm,"^[A-z]"),
             DivisionNm = paste0(toupper(initial),DivisionNm)) |>
      select(-initial) |>
      mutate(DivisionNm=case_when(
        DivisionNm=="Eden-monaro" ~ "Eden-Monaro",
        DivisionNm=="Kingsford smith" ~ "Kingsford Smith",
        DivisionNm=="La trobe" ~ "La Trobe",
        DivisionNm=="Mcewen" ~ "McEwen",
        DivisionNm=="Mcmahon" ~ "McMahon",
        DivisionNm=="Mcmillan" ~ "McMillan",
        DivisionNm=="Mcpherson" ~ "McPherson",
        DivisionNm=="Melbourne ports" ~ "Melbourne Ports",
        DivisionNm=="New england" ~ "New England",
        DivisionNm=="North sydney" ~ "North Sydney",
        DivisionNm=="O'connor" ~ "O'Connor",
        DivisionNm=="Port adelaide" ~ "Port Adelaide",
        DivisionNm=="Wide bay" ~ "Wide Bay",
        DivisionNm=="Fraser (ii)" ~ "Fraser (II)",
        DivisionNm=="Fraser" ~ "Fraser (II)",
        TRUE ~ DivisionNm)) |>
      mutate(Metro="No",Metro_Area="Non Metropolitan") |>
      collect()
  ) |>
  bind_rows(
    tribble(~DivisionNm,~Metro,~Metro_Area,
            "Batman","Yes","Greater Melbourne",
            "Charlton","No","Non Metropolitan",
            "Denison","Yes","Greater Hobart",
            "Fraser (I)","Yes","Australian Capital Territory",
            "Kalgoorlie", "No","Non Metropolitan",
            "Lowe","Yes","Greater Sydney",
            "McMillan","No","Non Metropolitan",
            "Melbourne Ports","Yes","Greater Melbourne",
            "Murray","No","Non Metropolitan",
            "Port Adelaide", "Yes","Greater Adelaide",
            "Prospect","Yes","Greater Sydney",
            "Stirling","Yes","Greater Perth",
            "Throsby", "No","Non Metropolitan",
            "Wakefield","No","Non Metropolitan",
            ) 
  ) |> 
  mutate(Metro = if_else(is.na(Metro),"No",Metro),
         Metro_Area =if_else(is.na(Metro_Area),"Non Metropolitan",Metro_Area))


dbWriteTable(source_db,"metro_non_metro",metro_non_metro,overwrite=TRUE)

consolidated <-
consolidated |> left_join(
                      tbl(source_db,"metro_non_metro") ,
                      by="DivisionNm"
)


## get into a DB view
# select_text <- consolidated |>
#   dbplyr::sql_render()
# 
# sql_query <- glue::glue("CREATE VIEW analysis_dataset AS\n {select_text};")
# 
# dbExecute(source_db,"DROP VIEW analysis_dataset;")
# dbExecute(source_db,sql_query)

#copy to files
consolidated |>
  collect() |>
  write_csv(here("4. Data","consolidated.csv"))

nationals |>
  collect() |>
  write_csv(here("4. Data","national_values.csv"))

state |>
  collect() |>
  write_csv(here("4. Data","state_values.csv"))

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
