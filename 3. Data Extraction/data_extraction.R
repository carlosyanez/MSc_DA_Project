######## Clean data extraction script ###########
######## This script extracts data from {auspol} and {auscensus}
######## data is either extracted by CED, or extracted at SA1/CD level and then aggregated to CED

#### Setup -----

# load libraries 
box::use(
  readr[read_csv],
  dplyr[...],
  tidyr[...],
  stringr[...],
  auspol[...],
  auscensus[...],
  aussiemaps[...],
  here[here],
  fs[...],
  DBI[...],
  dbplyr[...],
  duckdb[...],
  aussiemaps[geo_aggregate],
  sf[...],
  cropgrowdays[get_silodata]
  
  
)

# get folders
run_folder <- here("3. Data Extraction")
data_folder <- here("4. Data")

# load custom functions
source(path(run_folder,"functions.R"))

# get names of aux files
aux_files        <- dir_ls(path(run_folder,"aux_files"),regexp = "edited\\.csv")
names(aux_files) <- path_file(aux_files)

# other variables

state_acronyms <- c("NSW","QLD","NT","WA","SA","VIC","TAS","ACT")
election_years <- as.character(c(2007,2010,2016,2022))
census_years <-c(2006,2011,2016,2021)
year_equivalency <- tibble(election_years=election_years,census_years=census_years)
codes <- c("CD_CODE_2006","SA1_7DIGITCODE_2011","SA1_7DIGITCODE_2016","SA1_CODE_2021")
ceds  <- c("CED_NAME_2006","CED_NAME_2011","CED_NAME_2016","CED_NAME_2021")

names(codes) <- census_years
names(ceds)  <- census_years

# Open database -----
mydb <- dbConnect(duckdb(), path(data_folder,"consolidated_data.duckdb"),read_only=FALSE)
dbWriteTable(mydb, "year_equivalency", year_equivalency,overwrite=TRUE)


# Get Primary Vote -----

parties <- read_csv(aux_files["parties_edited.csv"])
dbWriteTable(mydb, "parties", parties,overwrite=TRUE)

# get list of unique party names by group
parties <- parties |>
            select(Grouping,PartyAb) |> 
            group_by(Grouping)       |>
            summarize(Parties=list(unique(PartyAb)))

parties_merge <-list()

for(i in 1:nrow(parties)){
  parties_merge[length(parties_merge)+1] <- parties[i,]$Parties 
}
names(parties_merge) <- parties$Grouping

# extract primary vote by party grouping, using auspol::house_primary_vote_summary()

primary_vote <- house_primary_vote_summary(state= state_acronyms, 
                                           parties=names(parties_merge),
                                           merge_parties = parties_merge,
                                           include_names=FALSE,
                                           year=election_years) |>
                select(Year,StateAb,DivisionNm,PartyAb,OrdinaryVotes,Percentage)

dbWriteTable(mydb, "primary_vote", primary_vote,overwrite=TRUE)

### Census Data - Age distribution -----

#get list of variables to aggregate, convert age at census to generation 

var04 <- list_census_attributes(number="04") |>
         filter(str_detect(Attribute,"\\d{1,3}_\\d{1,3}_",TRUE)) |>
         filter(str_detect(Attribute,"\\d{1,3}-\\d{1,3}",TRUE))  |>
         filter(str_detect(Attribute,"[Mm]ale",TRUE))  |>
         pivot_longer(c(-Table,-Attribute), names_to="Year",values_to = "Value") |>
         mutate(Year=as.numeric(Year)) |>
         filter(!is.na(Value)) |>
         mutate(Age_at_census=str_extract(Attribute,"\\d{1,2}") |> as.numeric(),
                YOB = Year-Age_at_census)

generations <- bind_rows(tibble(YOB=1883:1900,Group="Age - Lost Gen"),
                         tibble(YOB=1901:1927,Group="Age - Greatest Gen"),
                         tibble(YOB=1928:1945,Group="Age - Silent Gen"),
                         tibble(YOB=1946:1964,Group="Age - Baby Boomers"),
                         tibble(YOB=1965:1980,Group="Age - Gen X"),
                         tibble(YOB=1981:1996,Group="Age - Gen Y"),
                         tibble(YOB=1997:2012,Group="Age - Gen Z"),
                         tibble(YOB=2013:2030,Group="Age - Gen Alpha"))

var04 <- var04 |>
          left_join(generations,by="YOB")|>
          mutate(Group=if_else(str_detect(Attribute,"Total"),"Total",Group)) |>
          select(Attribute,Group,Year)

# convert to list structure used by auscensus /using auscensus::attribute_tibble_to_list()

levels <- attribute_tibble_to_list(var04,"Attribute","Group")
census_tables  <- list_census_tables() |>
  filter(Number %in% c("04"))

#extract

age <- extract_census_ced(census_tables,census_years,levels)
dbWriteTable(mydb, "age", age,overwrite=TRUE)


### Census Data - Education Level  -----

census_tables <- list_census_tables() |>
                   filter(Number %in% c("39","40","46","49")) |>
                   filter(str_detect(`Table Name`,"Non-School")) |>             
                   filter(str_detect(`Table Name`,"Level of Education by Age by Sex"))
attributes    <- read_csv(aux_files["ed_levels_edited.csv"])
levels        <- attribute_tibble_to_list(attributes)
total_level       <- names(levels)[str_detect(names(levels),"Total")]

ed_level      <- extract_census_sa1(census_tables, census_years, levels,codes,ceds,total_level)
dbWriteTable(mydb, "ed_level", ed_level,overwrite=TRUE)


### Census Data - Income           ------ 

census_tables      <- list_census_tables() |>
                        filter(Number %in% c("16","17")) |>
                        filter(str_detect(`Table Name`,"School",TRUE)) |>
                        filter(str_detect(`Table Name`,"Assistance",TRUE))
attributes        <- read_csv(aux_files["income_levels_edited.csv"])
levels            <- attribute_tibble_to_list(attributes)
total_level       <- names(levels)[str_detect(names(levels),"Total")]


income            <- extract_census_sa1(census_tables, census_years, levels,codes,ceds,total_level)
dbWriteTable(mydb, "income", income,overwrite=TRUE)

### Census Data - Place of Birth           ------ 

census_tables <- list_census_tables() |>
                  filter(Number %in% c("09")) |>
                  filter(str_detect(`Table Name`,"School",TRUE)) |>
                  filter(str_detect(`Table Name`,"Assistance",TRUE))
attributes        <- read_csv(aux_files["country_of_birth_levels_edited.csv"])
levels            <- attribute_tibble_to_list(attributes)
total_level       <- names(levels)[str_detect(names(levels),"Total")]

COB               <- extract_census_sa1(census_tables, census_years, levels,codes,ceds,total_level)
dbWriteTable(mydb, "COB", COB,overwrite=TRUE)

### Census Data - Religion           ------ 

census_tables <- list_census_tables() |>
                  filter(Number %in% c("13","14")) |>
                  filter(str_detect(`Table Name`,"Religious"))
attributes        <- read_csv(aux_files["religion_levels_edited.csv"])
levels            <- attribute_tibble_to_list(attributes)
total_level       <- names(levels)[str_detect(names(levels),"Total")]

religion          <- extract_census_sa1(census_tables, census_years, levels,codes,ceds,total_level)
dbWriteTable(mydb, "religion", religion,overwrite=TRUE)

### Census Data - Language           ------ 

census_tables     <- list_census_tables() |>
                      filter(Number %in% c("12","13")) |>
                      filter(str_detect(`Table Name`,"Language")) |>
                      filter(`Table Population`=="Persons")
attributes        <- read_csv(aux_files["language_levels_edited.csv"])
levels            <- attribute_tibble_to_list(attributes)
total_level       <- names(levels)[str_detect(names(levels),"Total")]

language          <- extract_census_sa1(census_tables, census_years, levels,codes,ceds,total_level)
dbWriteTable(mydb, "language", language,overwrite=TRUE)

### Census Data - Citizenship           ------ 

census_tables  <- list_census_tables() |>
                 filter(Number %in% c("01")) 
attributes        <- read_csv(aux_files["citizenship_levels_edited.csv"])
levels            <- attribute_tibble_to_list(attributes)
total_level       <- names(levels)[str_detect(names(levels),"Total")]

citizenship        <- extract_census_ced(census_tables, census_years, levels,total_level)
dbWriteTable(mydb, "citizenship", citizenship,overwrite=TRUE)

### Census Data - Household Tenure       ------ 

census_tables     <- list_census_tables() |>
                      filter(str_detect(`Table Name`,"[Tt]enure")) 
attributes        <- read_csv(aux_files["household_tenure_levels_edited.csv"])
levels            <- attribute_tibble_to_list(attributes)
total_level       <- names(levels)[str_detect(names(levels),"Total")]

household_tenure <- extract_census_sa1(census_tables, census_years, levels,codes,ceds,total_level)
dbWriteTable(mydb, "household_tenure", household_tenure,overwrite=TRUE)

### Census Data - Occupation       ------ 

census_tables     <- list_census_tables() |>
                      filter(str_detect(`Table Name`,"[In]ndustry")) |>
                      filter(str_detect(`Table Name`,"[Oo]ccupation"))
attributes        <- read_csv(aux_files["occupation_levels_edited.csv"])
levels            <- attribute_tibble_to_list(attributes)
total_level       <- names(levels)[str_detect(names(levels),"Total")]

occupation         <- extract_census_sa1(census_tables, census_years, levels,codes,ceds,total_level)
dbWriteTable(mydb, "occupation", occupation,overwrite=TRUE)

### Census Data - Relationship Status      ------ 

census_tables     <- list_census_tables() |>
                      filter(str_detect(`Table Name`,"Relationship"))
attributes        <- read_csv(aux_files["relationship_levels_edited.csv"])
levels            <- attribute_tibble_to_list(attributes)
total_level       <- names(levels)[str_detect(names(levels),"Total")]

relationship       <- extract_census_sa1(census_tables, census_years, levels,codes,ceds,total_level)
dbWriteTable(mydb, "relationship", relationship,overwrite=TRUE)

### Census Data - Prior Residence      ------ 

census_tables     <- list_census_tables() |>
                      filter(str_detect(`Table Name`,"5 Years"))
attributes        <- read_csv(aux_files["prior_res_edited.csv"])
levels            <- attribute_tibble_to_list(attributes)
total_level       <- names(levels)[str_detect(names(levels),"Total")]

relationship       <- extract_census_sa1(census_tables, census_years, levels,codes,ceds,total_level)
dbWriteTable(mydb, "relationship", relationship,overwrite=TRUE)

### Climate Data      ------ 


map_structure <- list_structure(2021) |>
  select(GCCSA_CODE_2021,GCCSA_NAME_2021,
         CED_CODE_2021,CED_NAME_2021) |>
  mutate(Year=2021)


#electorates and and metro areas

metro <-  map_structure |> 
  distinct(GCCSA_CODE_2021,GCCSA_NAME_2021) |>
  filter(str_detect(GCCSA_NAME_2021,"^Rest",TRUE)) |>
  filter(str_detect(GCCSA_NAME_2021,"^No usual",TRUE)) |>
  filter(str_detect(GCCSA_NAME_2021,"^Migratory",TRUE)) |>
  filter(str_detect(GCCSA_NAME_2021,"^Other",TRUE))

metro_electorates <- map_structure |>
  count(Year,CED_CODE_2021, CED_NAME_2021,GCCSA_CODE_2021,GCCSA_NAME_2021) |>
  group_by(Year,CED_CODE_2021, CED_NAME_2021) |>
  filter(n==max(n)) |>
  ungroup()  |>
  select(-n) |>
  filter(GCCSA_CODE_2021 %in% metro$GCCSA_CODE_2021) |>
  filter(str_detect(CED_NAME_2021,"^Rest",TRUE)) |>
  filter(str_detect(CED_NAME_2021,"^No usual",TRUE)) |>
  filter(str_detect(CED_NAME_2021,"^Migratory",TRUE)) |>
  filter(str_detect(CED_NAME_2021,"^Other",TRUE))

non_metro <- map_structure |>
  filter(!(CED_CODE_2021 %in% metro_electorates$CED_CODE_2021)) |>
  filter(str_detect(CED_NAME_2021,"^Rest",TRUE)) |>
  filter(str_detect(CED_NAME_2021,"^No usual",TRUE)) |>
  filter(str_detect(CED_NAME_2021,"^Migratory",TRUE)) |>
  filter(str_detect(CED_NAME_2021,"^Other",TRUE))      |>
  select(CED_NAME_2021,CED_CODE_2021)



metro_areas <-  bind_cols(metro,
                          get_map(filters = list(GCCSA_CODE_2021 = metro$GCCSA_CODE_2021),
                                  year=2021,
                                  aggregation=c("GCCSA_NAME_2021"),
                                  use_cache=TRUE) |>
                            st_make_valid() |>
                            st_centroid() |> 
                            st_coordinates()) |>
  rename("longitude"="X","latitude"="Y")


non_metro_map <-get_map(filters = list(CED_CODE_2021 = non_metro$CED_CODE_2021),
                        year=2021,
                        aggregation=c("CED_NAME_2021"),
                        use_cache=TRUE) 


non_metro_areas <-  bind_cols(non_metro_map |> st_drop_geometry() |> select(CED_NAME_2021,CED_CODE_2021),
                              non_metro_map |>st_make_valid() |>
                                st_centroid() |> 
                                st_coordinates() ) |> 
  rename("longitude"="X","latitude"="Y")



silodata_metro <- tibble()

for(i in 1:nrow(metro_areas)){
  message(i)
  data_i  <- get_silodata(latitude = metro_areas[i,]$latitude,
                          longitude = metro_areas[i,]$longitude,
                          email = "carlos.yanez.s@gmail.com",
                          START = "19700101",
                          FINISH = "20220731") |>
    mutate(GCCSA_NAME_2021 = metro_areas[i,]$GCCSA_NAME_2021,
           GCCSA_CODE_2021 = metro_areas[i,]$GCCSA_CODE_2021
    )
  
  silodata_metro <- bind_rows(silodata_metro,data_i)
  
  
}


silodata_non_metro <- tibble()

for(i in 1:nrow(non_metro_areas)){
  
  message(i)
  tryCatch({
  data_i  <- get_silodata(latitude = non_metro_areas[i,]$latitude,
                          longitude = non_metro_areas[i,]$longitude,
                          email = "carlos.yanez.s@gmail.com",
                          START = "19700101",
                          FINISH = "20220731") |>
    mutate(CED_NAME_2021 = non_metro_areas[i,]$CED_NAME_2021,
           CED_CODE_2021 = non_metro_areas[i,]$CED_CODE_2021
    )
  
  silodata_non_metro <- bind_rows(silodata_non_metro,data_i)},
  error = function(e){message(e)})
  
  
}

rm(data_i,i)

dbWriteTable(mydb, "metro_electorates", metro_electorates,overwrite=TRUE)
dbWriteTable(mydb, "non_metro_electorates", non_metro,overwrite=TRUE)


dbWriteTable(mydb, "silodata_metro", silodata_metro,overwrite=TRUE)
dbWriteTable(mydb, "silodata_non_metro", silodata_non_metro,overwrite=TRUE)



### Proportions Tables -------

for(year in census_years){
  
  if(year==2006){
    original_geo <- "CD_CODE_2006"
    new_geo      <- "CED_NAME_2006"
  }
  if(year %in% c(2011,2016)){
    original_geo <- str_c("SA1_7DIGITCODE_",year)
    new_geo      <- str_c("CED_NAME_",year)
  }
  if(year==2021){
    original_geo <- str_c("SA1_CODE_",year)
    new_geo      <- str_c("CED_NAME_",year)
  }
  
  
  correspondence_table <- list_structure(year=year) |>
    select(any_of(c("id",original_geo,new_geo)))
  
  proportions_table <-   list_proportions(original_geo) |>
    mutate(prop=as.numeric(area/sum_area)) |>
    collect()  |>
    select("id","geo_col","prop") |>
    rename(!!original_geo := "geo_col")
  
  correspondence_table <- correspondence_table |>
    left_join(proportions_table,by=c("id",original_geo)) |>
    group_by(across(any_of(c(original_geo,new_geo)))) |>
    summarise(across(any_of(c("prop")), ~ sum(.x,na.rm=TRUE)),.groups = "drop") |>
    filter(prop!=0) |>
    mutate(Year=year)|>
    mutate(across(any_of(new_geo), ~ case_when(
      Year %in% c(2006,2011) & str_detect(.x,"Fraser") ~ "Fraser (I)",
      Year %in% c(2019,2021) & str_detect(.x,"Fraser") ~ "Fraser (II)",
      TRUE ~ .x
    )))
  
  dbWriteTable(mydb, str_c("correspondence_",year), correspondence_table,overwrite=TRUE)
  
}

### Fix CED Names ----
## Census Data
tables <- DBI::dbListTables(mydb)
tables <- tables[str_detect(tables,"granular",TRUE)]
tables <- tables[str_detect(tables,"year_equivalency",TRUE)]
tables <- tables[str_detect(tables,"correspondence_",TRUE)]
tables <- tables[str_detect(tables,"silodata",TRUE)]
#tables <- tables[str_detect(tables,"electorates",TRUE)]
#tables <- "language"
tables <- tables[str_detect(tables,"metro",TRUE)]
for(i in 1:length(tables)){
  cols <- colnames(tbl(mydb,tables[i])) 
  ced_col <- cols[str_detect(cols,"Unit|CED")]
  
  df <- tbl(mydb,tables[i]) |>
    mutate(across(any_of(ced_col), ~ case_when(
      Year %in% c(2006,2011) & str_detect(.x,"Fraser") ~ "Fraser (I)",
      Year %in% c(2019,2021) & str_detect(.x,"Fraser") ~ "Fraser (II)",
      TRUE ~ .x
    ))) |>
    collect()
  
  dbWriteTable(mydb, tables[i], df,overwrite=TRUE)
  
}
### Disconnect, clean, upload DB -----

dbDisconnect(mydb, shutdown=TRUE)

box::use(
  piggyback[pb_new_release,pb_upload],
  fs[file_delete],
  zip[zip]
)

file_name      <- here("4. Data","consolidated_data.duckdb")
repo           <- "carlosyanez/MSc_DA_Project"
version       <- "data"

#create new release
tryCatch(pb_new_release(repo,version),
         error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

#list and zip files
zip_file <- path(run_folder,"data_orig.zip")

zip(zip_file,file_name,mode="cherry-pick")

# upload catalogue items ---
pb_upload(file=zip_file,repo,version)

file_delete(zip_file)

rm(list=ls())
