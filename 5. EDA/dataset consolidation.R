## script to create consolidated dataset with selected variables
# [1] "Year"                                "census_years"                        "StateAb"                             "DivisionNm"                         
#[5] "PartyAb"                             "Percentage_Diff_National"            "Baby Boomers"                        "Gen X"                              
#[9] "Gen Y"                               "Gen Z"                               "Australian Citizens"                 "Lang - English Only"                
#[13] "Lang - Chinese"                      "Lang - South Asian"                  "Lang - Arabic"                       "Lang - East Asian"                  
#[17] "Rel - Anglican.Uniting.Presbyterian" "Rel - Buddhism"                      "Rel - Catholic"                      "Rel - Christian.Orthodox"           
#[21] "Rel - Hinduism"                      "Rel - Islam"                         "Income - 1 to.999"                   "Income - 1000 to.1999"              
#[25] "Income - 2000 or.more"               "Housing - Flat"                      "Housing - Social or.community"       "Ed University"                      
#[29] "Ed Vocational"                       "Rel - Child under.15"                "Rel - Group Household"               "Rel - De Facto"                     
#[33] "Rel - Living Alone"     

#load libraries, tidyverse, here and DBI
library(tidyverse)
library(here)
library(DBI)

#load data from 4.Data/processed_data.duckdb (duckdb database)
source_db <- dbConnect(duckdb::duckdb(), here("4. Data","processed_data.duckdb"))


#tbl(source_db,"citizenship") |> collect() |> View()
#get parties table, filter PartyAb keeping only ALP, COAL and GRN
consolidated <-  tbl(source_db, "primary_vote") |>
  filter(PartyAb %in% c("ALP", "COAL", "GRN")) |>
  select(-OrdinaryVotes) |>
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
  mutate(DivisionNm=str_to_lower(DivisionNm))  
  

## list available tables
#dbListTables(source_db)
# [1] "age_and_sex"               "citizenship"               "citizenship_granular"      "citizenship_sd"            "correspondence_2006"       "correspondence_2011"      
# [7] "correspondence_2016"       "correspondence_2021"       "country_of_birth"          "country_of_birth_granular" "country_of_birth_sd"       "household_tenure"         
# [13] "household_tenure_granular" "household_tenure_sd"       "income_level"              "income_level_granular"     "income_level_sd"           "language"                 
# [19] "language_granular"         "language_sd"               "metro_electorates"         "non_metro_electorates"     "occupation"                "occupation_granular"      
# [25] "occupation_sd"             "parties"                   "primary_vote"              "prior"                     "prior_granular"            "relationship"             
# [31] "relationship_granular"     "relationship_sd"           "religion"                  "religion_granular"         "religion_sd"               "school_level"             
# [37] "school_level_granular"     "school_level_sd"           "year_equivalency"   

## add citizenship
consolidated <- 
consolidated |>
  left_join(tbl(source_db,"citizenship") |> mutate(Unit=str_to_lower(Unit)) |> 
            rename("Australian_Citizens"="Australian.Citizens"),
            by=c("Year"="Year","DivisionNm"="Unit")) 

# add age groups
age_groups <-  tbl(source_db,"age_and_sex") |>
                mutate(Unit=str_to_lower(Unit)) |>
                select(Unit,Year,Attribute,Percentage)  |>
                pivot_wider(names_from = Attribute,values_from = Percentage) |>
                rename_with(.fn= ~ str_replace(.x," - ","_") |> str_replace_all(" ","_"),
                            .cols=contains("Age")) |>
                select(-Age_Greatest_Gen,-Age_Gen_Alpha)

consolidated <- consolidated |>
                left_join(age_groups,by=c("Year"="Year","DivisionNm"="Unit")) 

# add selected languages
#"Lang - English Only"                
#[13] "Lang - Chinese"                      "Lang - South Asian"                  "Lang - Arabic"                       "Lang - East Asian"

language <- tbl(source_db,"language") |> select(any_of(c("Unit","Year",
                                           "Language...English.Only",
                                           "Language...Chinese",
                                           "Language...South.Asian",
                                           "Language...Arabic",
                                           "Language...East.Asian"
                                           ))) |>
  mutate(Unit=str_to_lower(Unit)) |>
  rename_with(.fn= ~ str_replace(.x,"\\.\\.\\.","_") |> str_replace_all("\\.","_"),
              .cols=contains("Language"))


consolidated <- consolidated |>
  left_join(language,by=c("Year"="Year","DivisionNm"="Unit")) 

#religion
#[17] "Rel - Anglican.Uniting.Presbyterian" "Rel - Buddhism"                      "Rel - Catholic"                      "Rel - Christian.Orthodox"           
#[21] "Rel - Hinduism"                      "Rel - Islam"  

religion <- tbl(source_db,"religion") |>
  mutate(Unit=str_to_lower(Unit)) |>
  rename_with(.fn= ~ str_replace(.x,"\\.\\.\\.","_") |> str_replace_all("\\.","_"),
              .cols=contains("Religion")) |>
  select(any_of(c("Unit","Year",
                  "Religion_Anglican_Uniting_Presbyterian",
                  "Religion_Buddhism",
                  "Religion_Catholic","Religion_Christian_Orthodox",
                  "Religion_Hinduism","Religion_Other_Christianity",
                  "Religion_No_Religion_Secular","Religion_Islam")))

consolidated <- consolidated |>
  left_join(religion,by=c("Year"="Year","DivisionNm"="Unit")) 

# income
income <- tbl(source_db,"income_level") |>
  mutate(Unit=str_to_lower(Unit)) |>
  rename_with(.fn= ~ str_replace(.x,"\\.\\.\\.","_") |> str_replace_all("\\.","_"),
              .cols=contains("Income")) |>
  select(-Income_Not_Stated)

consolidated <- consolidated |>
  left_join(income,by=c("Year"="Year","DivisionNm"="Unit")) 


# housing
housing <-
  tbl(source_db,"household_tenure") |>
  mutate(Unit=str_to_lower(Unit)) |>
  rename_with(.fn= ~ str_replace(.x,"\\.\\.\\.","_") |> str_replace_all("\\.","_"),
              .cols=contains("Household"))
  
consolidated <- consolidated |>
  left_join(housing,by=c("Year"="Year","DivisionNm"="Unit")) 

#education
education <-
tbl(source_db,"school_level") |>
  mutate(Unit=str_to_lower(Unit)) |>
  rename_with(.fn= ~ str_replace(.x,"\\.\\.\\.","_") |> str_replace_all("\\.","_"),
              .cols=contains("Education")) |>
  select(-Education_Inadequately_Described)

consolidated <- consolidated |>
  left_join(education,by=c("Year"="Year","DivisionNm"="Unit")) 


#relationships
relationships <-
tbl(source_db,"relationship") |>
  mutate(Unit=str_to_lower(Unit)) |>
  rename_with(.fn= ~ str_replace(.x,"\\.\\.\\.","_") |> str_replace_all("\\.","_"),
              .cols=contains("Relationship")) |>
  select(-Relationship_Visitor)

consolidated <- consolidated |>
  left_join(relationships,by=c("Year"="Year","DivisionNm"="Unit")) 

#fix CED capitalisation

consolidated <-
  consolidated |>
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
    DivisionNm=="Fraser (i)" ~ "Fraser (I)",
    DivisionNm=="Fraser (ii)" ~ "Fraser (II)",
    TRUE ~ DivisionNm
  )) 



# add metro flag, GCCSA for metro electorates
# bases on 2021, then manually review

metro_non_metro <- tbl(source_db,"metro_electorates") |>
  select(DivisionNm=CED_NAME_2021,Metro_Area=GCCSA_NAME_2021) |>
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
select_text <- consolidated |>
  dbplyr::sql_render()

sql_query <- glue::glue("CREATE VIEW analysis_dataset AS\n {select_text};")

dbExecute(source_db,"DROP VIEW analysis_dataset;")
dbExecute(source_db,sql_query)

#copy to file
tbl(source_db,"analysis_dataset") |>
  collect() |>
  write_csv(here("4. Data","consolidated.csv"))

#disconnect
dbDisconnect(source_db, shutdown=TRUE)

tbl(source_db,"analysis_dataset") |>
  collect() |>
  filter(is.na(Australian_Citizens))
