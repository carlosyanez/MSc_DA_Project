library(DBI)
library(tidyverse)
library(dbplyr)
library(here)
library(fs)
library(auspol)
library(patchwork)

base_theme <- customthemes::custom_plot_theme_md(google_font = "Roboto",background_colour = "white", 
                                                 legend_pos = "botom",legend_dir = "horizontal",
                                                 title_size = 40,
                                                 subtitle_size = 36,
                                                 caption_size = 30,
                                                 axis_size = 30)+
        theme(legend.text = ggtext::element_markdown(size=30),
              strip.text.x = element_text(size = 30),
              strip.text.y = element_text(size = 30))
              

map_theme <- base_theme +
  theme(axis.title = element_blank(),
        axis.ticks = element_line(colour="grey"))

#default theming for plots ####
theme_set(base_theme) 

customthemes::set_plot_colours(line_colour ="#0085c7",
                               fill_colour = tinter::lighten("#0085c7",0.5),
                               HVline_colour ="grey90")

state_acronyms <- c("NSW","QLD","NT","WA","SA","VIC","TAS","ACT")
election_years <- as.character(c(2007,2010,2016,2022))
census_years <-c(2006,2011,2016,2021)
year_equivalency <- tibble(election_years=election_years,census_years=census_years)
#get aussiemaps tables
codes <- c("CD_CODE_2006","SA1_7DIGITCODE_2011","SA1_7DIGITCODE_2016","SA1_CODE_2021")
ceds  <- c("CED_NAME_2006","CED_NAME_2011","CED_NAME_2016","CED_NAME_2021")

mydb <- dbConnect(duckdb::duckdb(), here("4. Data","consolidated_data.duckdb"),read_only=FALSE)
this_dir <- here("5. EDA")
bulk_dir <- here("5. EDA","bulk")
dir_create(bulk_dir)

tables <- dbListTables(mydb)


## Party Primary vote -----

primary_vote <- tbl(mydb,"primary_vote") |>
                filter(Year!=2022)       |>
                mutate(name=str_c(DivisionNm, " - ",Year))

parties <- unique(primary_vote |> collect() |> distinct(PartyAb) |> pull())

primary_vote_plot <- list()

for(i in 1:length(parties)){
  
  my_palette <-RColorBrewer::brewer.pal(8,"Set2")
  names(my_palette) <- state_acronyms
  
  primary_vote_plot[[i]] <-primary_vote |> 
        collect() |>
        filter(PartyAb==parties[i]) |>
        ggplot(aes(x=PartyAb,y=Percentage,colour=StateAb)) +
        ggbeeswarm::geom_quasirandom() +
        facet_grid(. ~ Year) +
        labs(title=parties[i]) + 
    base_theme +
    scale_colour_manual(values=my_palette)

  if(i==(length(parties)-1)){
    primary_vote_plot[[i]] <- primary_vote_plot[[i]] +
    theme(legend.position = "bottom") 
      
  }
}

patchwork::wrap_plots(primary_vote_plot) |> customthemes::save_image(path(bulk_dir,"primary_vote.png"))


## Histograms ----

tables_ced <- tables[str_detect(tables,"granular",TRUE)]
tables_ced <- tables_ced[str_detect(tables_ced,"correspondence",TRUE)]
tables_ced <- tables_ced[str_detect(tables_ced,"year_equivalency",TRUE)]
tables_ced <- tables_ced[str_detect(tables_ced,"silo|parties|primary",TRUE)]

corr <- tibble()
all_covariates <- tibble()


for(table in tables_ced){
  attr <- dbListFields(mydb,table)
  t <- tbl(mydb,table)

  is_formatted <- any(str_detect(attr,"Attribute"))
  
  if(!is_formatted){
    t_collected <- t |>
      filter(Year!=2021) |>
      collect() |>
      mutate(across(!any_of(c("Unit","Year","Census_Code")), as.numeric)) |>
      pivot_longer(-any_of(c("Unit","Year","Census_Code")),names_to = "Attribute",values_to = "Percentage") 
    
  }else{
    t_collected <- t |>
      filter(Year!=2021) |>
      collect()
  }
  
  attr_values <- unique(t_collected$Attribute)
  
  p <- list()
  
  for(i in 1:length(attr_values)){
    print(attr_values[i])
  
    p[[i]] <- t_collected |>
      filter(Attribute==attr_values[i]) |>
      ggplot(aes(x=Percentage)) +
      geom_density()          +
      coord_flip() +
      facet_grid(Year ~.)   +
      labs(title=attr_values[i]) + 
      base_theme 
    
    if(i==(length(parties)-1)){
      primary_vote_plot[[i]] <- primary_vote_plot[[i]] +
        theme(legend.position = "bottom")
      
    }
      
      against_response <- t_collected |>
        filter(Attribute==attr_values[i]) |>
        rename("Census"="Percentage") |>
        left_join(year_equivalency |> mutate(election_years=as.numeric(election_years)),
                  by=c("Year"="census_years")) |>
        select(-Attribute) |>
        left_join(primary_vote |> collect(),
                  by=c("election_years"="Year","Unit"="DivisionNm")
        ) |>
        filter(!is.na(PartyAb))
               
    
    p_against_response <- against_response |>
      ggplot(aes(x=Census,y=Percentage,colour=StateAb)) +
      geom_point() +
      facet_grid(Year ~ PartyAb) +
      labs(x=attr_values[i],title=attr_values[i],y="Primary Vote") +
      base_theme +
    #  scale_colour_manual(values=auspol::manage_colours(extra_values = parties))
      scale_colour_manual(values=my_palette) + 
      theme(legend.position = "bottom")
    
    customthemes::save_image(p_against_response, path(bulk_dir,str_c(attr_values[i],"_against_reponse.png")))
    
    corr_i <- round(cor(against_response |>
                        select(Census,PartyAb,Percentage) |>
                        distinct()                        |>
                        filter(Census!=0)                 |>
                        pivot_wider("Census",names_from = PartyAb,values_from = Percentage) |>
                        rename(!!attr_values[i]:="Census") 
    ), 1) |>
      as_tibble() |>
      pivot_longer(-any_of(c(attr_values[i])), names_to="party",values_to="corr") |>
      rename("A"=attr_values[i]) |>
      filter(A==1)               |>
      mutate(Attribute=attr_values[i])
    
    
    corr <- bind_rows(corr,corr_i)
    
    all_covariates <- bind_rows(all_covariates,t_collected)
    
  }
  patchwork::wrap_plots(p) |> customthemes::save_image(path(bulk_dir,str_c(table,"_hist.png")))
  
}

corr


# correlation with each primary vote

all_covariates <- all_covariates |>
    select(-Value,-Census_Code,-Total) |>
    distinct()                         |>
    pivot_wider(c("Unit","Year"), names_from = Attribute,values_from = Percentage) |>
    select(-Unit,-Year) 


   (all_covariates |>
    GGally::ggpairs(upper = list(continuous = GGally::wrap("cor", size = 30)))  +
    base_theme 
    )|>
  customthemes::save_image(path(bulk_dir,"covariates.png"))


corr2 <- round(cor(all_covariates), 1)

ggcorrplot::ggcorrplot(corr2, hc.order = TRUE, type = "lower",
                       lab = TRUE) |>
  customthemes::save_image(path(bulk_dir,"covariates_correlation.png"))

#granular




dbDisconnect(mydb, shutdown=TRUE)


