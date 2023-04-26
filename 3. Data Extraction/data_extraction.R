######## Clean data extraction script ###########
######## This script extracts data from {auspol} and {auscensus}
######## data is either extracted by CED, or extracted at SA1/CD level and then aggregated to CED

#### Setup -----

# load libraries 
box::use(
  dplyr[...],
  tidyr[...],
  stringr[...],
  auspol[...],
  auscensus[...],
  here[here],
  fs[...],
  DBI[...],
  dbplyr[...],
  duckdb[...]
  
)

# load custom functions

# get folders
run_folder <- here("3. Data Extraction")
data_folder <- here("4. Data")

# get names of aux files
aux_files        <- dir_ls(run_folder,regexp = "edited\\.csv")
names(aux_files) <- path_file(aux_files)

# other variables

state_acronyms <- c("NSW","QLD","NT","WA","SA","VIC","TAS","ACT")
election_years <- as.character(c(2007,2010,2016,2022))
census_years <-c(2006,2011,2016,2021)
year_equivalency <- tibble(election_years=election_years,census_years=census_years)
codes <- c("CD_CODE_2006","SA1_7DIGITCODE_2011","SA1_7DIGITCODE_2016","SA1_CODE_2021")
ceds  <- c("CED_NAME_2006","CED_NAME_2011","CED_NAME_2016","CED_NAME_2021")

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
         filter(str_detect(Attribute,"\\d{1,3}_persons$",TRUE))  |>
         filter(str_detect(Attribute,"[Oo]ver",TRUE))  |>
         filter(str_detect(Attribute,"[Tt]otal.*[Mm]ales",TRUE)) |>
         pivot_longer(c(-Table,-Attribute), names_to="Year",values_to = "Value") |>
         mutate(Year=as.numeric(Year)) |>
         filter(!is.na(Value)) |>
         mutate(Age_at_census=str_extract(Attribute,"\\d{1,2}") |> as.numeric(),
                YOB = Year-Age_at_census,
                Gender=case_when(
                str_detect(Attribute, "_[Mm]ales$") ~ "Males",
                str_detect(Attribute, "_[Ff]emales$") ~ "Females",
                TRUE ~"-"))

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

#extract

extract_census_ced("04",census_years,levels)

