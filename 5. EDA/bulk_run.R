###QQ
## setup ----
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

#default theming for plots #
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

processed_db <- dbConnect(duckdb::duckdb(), here("4. Data","processed_data.duckdb"),read_only=FALSE)

this_dir <- here("5. EDA")
bulk_dir <- here("5. EDA","bulk")
dir_create(bulk_dir)

tables <- dbListTables(processed_db)
tables <- tables[str_detect(tables,"electorates",TRUE)]


keep_vars <- ls()
keep_var <- c(keep_vars,"keep_vars")
## Party Primary vote -----

primary_vote <- tbl(processed_db,"primary_vote") |>
                filter(Year!=2022)       |>
                mutate(name=str_c(DivisionNm, " - ",Year))

parties <- unique(primary_vote |> collect() |> distinct(PartyAb) |> pull())

my_palette <-RColorBrewer::brewer.pal(8,"Set2")
names(my_palette) <- state_acronyms

keep_vars <- c(keep_var,"parties","primary_vote","my_palette")

primary_vote_plot <- list()

for(i in 1:length(parties)){
  
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

#by party, faceted by Year/State

for(i in 1:length(parties)){
  
  my_palette <-RColorBrewer::brewer.pal(8,"Set2")
  names(my_palette) <- state_acronyms
  
  primary_vote_plot <-primary_vote |> 
    collect() |>
    filter(PartyAb==parties[i]) |>
    ggplot(aes(x=PartyAb,y=Percentage,colour=StateAb)) +
    ggbeeswarm::geom_quasirandom() +
    facet_grid( Year ~ StateAb) +
    labs(title=parties[i]) + 
    base_theme +
    scale_colour_manual(values=my_palette) +
    theme(legend.position = "bottom") 
  
  customthemes::save_image(primary_vote_plot,path(bulk_dir,str_c("primary_vote_",parties[[i]],".png")))
  
  
}


rm(list=ls()[!(ls() %in% keep_vars)])
## Party Primary vote - Difference Against National Averages-----


primary_vote_plot <- list()
for(i in 1:length(parties)){
  
  my_palette <-RColorBrewer::brewer.pal(8,"Set2")
  names(my_palette) <- state_acronyms
  
  primary_vote_plot[[i]] <-primary_vote |> 
    collect() |>
    filter(PartyAb==parties[i]) |>
    ggplot(aes(x=PartyAb,y=Percentage_Diff_National,colour=StateAb)) +
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

patchwork::wrap_plots(primary_vote_plot) |> customthemes::save_image(path(bulk_dir,"primary_vote_national_diff.png"))


for(i in 1:length(parties)){
  
  my_palette <-RColorBrewer::brewer.pal(8,"Set2")
  names(my_palette) <- state_acronyms
  
  primary_vote_plot <-primary_vote |> 
    collect() |>
    filter(PartyAb==parties[i]) |>
    ggplot(aes(x=PartyAb,y=Percentage_Diff_National,colour=StateAb)) +
    ggbeeswarm::geom_quasirandom() +
    facet_grid( Year ~ StateAb) +
    labs(title=parties[i]) + 
    base_theme +
    scale_colour_manual(values=my_palette) +
    theme(legend.position = "bottom") 
  
  customthemes::save_image(primary_vote_plot,path(bulk_dir,str_c("primary_vote_national_diff_",parties[[i]],".png")))
  
  
}

rm(list=ls()[!(ls() %in% keep_vars)])
## Party Primary vote - Difference Against State Averages-----


primary_vote_plot <- list()
for(i in 1:length(parties)){
  
  my_palette <-RColorBrewer::brewer.pal(8,"Set2")
  names(my_palette) <- state_acronyms
  
  primary_vote_plot[[i]] <-primary_vote |> 
    collect() |>
    filter(PartyAb==parties[i]) |>
    ggplot(aes(x=PartyAb,y=Percentage_Diff_State,colour=StateAb)) +
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

patchwork::wrap_plots(primary_vote_plot) |> customthemes::save_image(path(bulk_dir,"primary_vote_state_diff.png"))


for(i in 1:length(parties)){
  
  my_palette <-RColorBrewer::brewer.pal(8,"Set2")
  names(my_palette) <- state_acronyms
  
  primary_vote_plot <-primary_vote |> 
    collect() |>
    filter(PartyAb==parties[i]) |>
    ggplot(aes(x=PartyAb,y=Percentage_Diff_State,colour=StateAb)) +
    ggbeeswarm::geom_quasirandom() +
    facet_grid( Year ~ StateAb) +
    labs(title=parties[i]) + 
    base_theme +
    scale_colour_manual(values=my_palette) +
    theme(legend.position = "bottom") 
  
  customthemes::save_image(primary_vote_plot,path(bulk_dir,str_c("primary_vote_state_diff_",parties[[i]],".png")))
  
  
}


rm(list=ls()[!(ls() %in% keep_vars)])
## Histograms and relation with responses ----

tables_ced <- tables[str_detect(tables,"granular",TRUE)]
tables_ced <- tables_ced[str_detect(tables_ced,"correspondence",TRUE)]
tables_ced <- tables_ced[str_detect(tables_ced,"year_equivalency",TRUE)]
tables_ced <- tables_ced[str_detect(tables_ced,"silo|parties|primary|sd$",TRUE)]

keep_vars <- c(keep_vars,"tables_ced")

corr_abs <- tibble()
corr_state <- tibble()
corr_nat   <- tibble()
all_covariates <- tibble()


for(table in tables_ced){
  attr <- dbListFields(processed_db,table)
  t <- tbl(processed_db,table)

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
    
    t_collected_i <-  t_collected |>
      filter(Attribute==attr_values[i])
  
    p[[i]] <- t_collected_i |>
      ggplot(aes(x=Percentage)) +
      geom_density()          +
      coord_flip() +
      facet_grid(Year ~.)   +
      labs(title=attr_values[i]) + 
      base_theme 
    
    if(i==(length(parties)-1)){
      p[[i]] <- p[[i]] +
        theme(legend.position = "bottom")
      
    }
      
      against_response <- t_collected_i |>
        rename("Census"="Percentage") |>
        left_join(year_equivalency |> mutate(election_years=as.numeric(election_years)),
                  by=c("Year"="census_years")) |>
        select(-Attribute) |>
        left_join(primary_vote |> collect(),
                  by=c("election_years"="Year","Unit"="DivisionNm")
        ) |>
        filter(!is.na(PartyAb))
               
    
    p_against_response_abs <- against_response |>
      ggplot(aes(x=Census,y=Percentage,colour=StateAb)) +
      geom_point() +
      facet_grid(Year ~ PartyAb) +
      labs(x=attr_values[i],title=str_c(attr_values[i], " - ABS"),y="Primary Vote") +
      base_theme +
    #  scale_colour_manual(values=auspol::manage_colours(extra_values = parties))
      scale_colour_manual(values=my_palette) + 
      theme(legend.position = "bottom")
    
    customthemes::save_image(p_against_response_abs, path(bulk_dir,str_c(attr_values[i],"_against_reponse.png")))
    
    p_against_response_state <- against_response |>
      ggplot(aes(x=Census,y=Percentage_Diff_State,colour=StateAb)) +
      geom_point() +
      facet_grid(Year ~ PartyAb) +
      labs(x=attr_values[i],title=str_c(attr_values[i], " - STATE"),y="Primary Vote") +
      base_theme +
      #  scale_colour_manual(values=auspol::manage_colours(extra_values = parties))
      scale_colour_manual(values=my_palette) + 
      theme(legend.position = "bottom")
    
    customthemes::save_image(p_against_response_state, path(bulk_dir,str_c(attr_values[i],"_against_reponse_state_diff.png")))
    
    
    p_against_response_nat <- against_response |>
      ggplot(aes(x=Census,y=Percentage_Diff_National,colour=StateAb)) +
      geom_point() +
      facet_grid(Year ~ PartyAb) +
      labs(x=attr_values[i],title=str_c(attr_values[i], " - NATIONAL"),y="Primary Vote") +
      base_theme +
      #  scale_colour_manual(values=auspol::manage_colours(extra_values = parties))
      scale_colour_manual(values=my_palette) + 
      theme(legend.position = "bottom")
    
    customthemes::save_image(p_against_response_nat, path(bulk_dir,str_c(attr_values[i],"_against_reponse_nat_diff.png")))
    
    #by State
    
    for(state in state_acronyms){
      
      p_against_response_abs <- against_response |>
        filter(StateAb==state)                   |>
        ggplot(aes(x=Census,y=Percentage,colour=StateAb)) +
        geom_point() +
        facet_grid(Year ~ PartyAb) +
        labs(x=attr_values[i],title=attr_values[i],y="Primary Vote") +
        base_theme +
        #  scale_colour_manual(values=auspol::manage_colours(extra_values = parties))
        scale_colour_manual(values=my_palette) + 
        theme(legend.position = "bottom")
      
      customthemes::save_image(p_against_response_abs, path(bulk_dir,str_c(attr_values[i],"_",state,"_against_reponse.png")))
      
      p_against_response_state <- against_response |>
        filter(StateAb==state)                   |>
        ggplot(aes(x=Census,y=Percentage_Diff_State,colour=StateAb)) +
        geom_point() +
        facet_grid(Year ~ PartyAb) +
        labs(x=attr_values[i],title=attr_values[i],y="Primary Vote") +
        base_theme +
        #  scale_colour_manual(values=auspol::manage_colours(extra_values = parties))
        scale_colour_manual(values=my_palette) + 
        theme(legend.position = "bottom")
      
      customthemes::save_image(p_against_response_state, path(bulk_dir,str_c(attr_values[i],"_",state,"_against_reponse_state_diff.png")))
      
      
      p_against_response_nat <- against_response |>
        filter(StateAb==state)                   |>
        ggplot(aes(x=Census,y=Percentage_Diff_State,colour=StateAb)) +
        geom_point() +
        facet_grid(Year ~ PartyAb) +
        labs(x=attr_values[i],title=attr_values[i],y="Primary Vote") +
        base_theme +
        #  scale_colour_manual(values=auspol::manage_colours(extra_values = parties))
        scale_colour_manual(values=my_palette) + 
        theme(legend.position = "bottom")
      
      customthemes::save_image(p_against_response_nat, path(bulk_dir,str_c(attr_values[i],"_",state,"_against_reponse_nat_diff.png")))
      
    }
    

    corr_bas <- against_response |>
                    select(Census,PartyAb,Percentage,
                           Percentage_Diff_State,
                           Percentage_Diff_National) |>
                   distinct()                        |>
                   filter(Census!=0)  
    
    for(party in parties){
      
      cb_abs <- corr_bas |>
        filter(PartyAb==party) |>
        filter(!is.na(Percentage)) |>
        distinct(Census,Percentage)
      
      corr_abs_i <- tibble(party=party,Attribute=attr_values[i],corr=cor(cb_abs$Census,cb_abs$Percentage))
      
      cb_nat <- corr_bas |>
        filter(PartyAb==party) |>
        filter(!is.na(Percentage_Diff_National)) |>
        distinct(Census,Percentage_Diff_National)
      
      corr_nat_i <- tibble(party=party,Attribute=attr_values[i],corr=cor(cb_nat$Census,cb_nat$Percentage_Diff_National))
      
      cb_state <- corr_bas |>
        filter(PartyAb==party) |>
        filter(!is.na(Percentage_Diff_State)) |>
        distinct(Census,Percentage_Diff_State)
      
      corr_state_i <- tibble(party=party,Attribute=attr_values[i],corr=cor(cb_state$Census,cb_state$Percentage_Diff_State))
      
      corr_abs <- bind_rows(corr_abs,corr_abs_i)
      corr_nat <- bind_rows(corr_nat,corr_nat_i)
      corr_state <- bind_rows(corr_state,corr_state_i)
      
      
  }
    
    all_covariates <- bind_rows(all_covariates,t_collected)
    
  }
  patchwork::wrap_plots(p) |> customthemes::save_image(path(bulk_dir,str_c(table,"_hist.png")))
  
}

(corr_abs |> 
  ggplot(aes(x=party,y=Attribute,fill=corr)) +
  geom_tile() +
  scale_fill_gradient2(low="red",mid="white", high="blue") +
  base_theme) |> 
  customthemes::save_image(path(bulk_dir,str_c("correlations_abs.png")))

(corr_nat |> 
    ggplot(aes(x=party,y=Attribute,fill=corr)) +
    geom_tile() +
    scale_fill_gradient2(low="red",mid="white", high="blue") +
    base_theme) |> 
  customthemes::save_image(path(bulk_dir,str_c("correlations_nat.png")))

(corr_state |> 
    ggplot(aes(x=party,y=Attribute,fill=corr)) +
    geom_tile() +
    scale_fill_gradient2(low="red",mid="white", high="blue") +
    base_theme) |> 
  customthemes::save_image(path(bulk_dir,str_c("correlations_state.png")))


# correlation with bewteen predictors

all_attributes <- unique(all_covariates$Attribute)

attribute_grid <- expand.grid(all_attributes,all_attributes)

all_corr <- tibble()
for(i in 1:nrow(attribute_grid)){
  
  attr1 <- attribute_grid[i,]$Var1
  attr2 <- attribute_grid[i,]$Var2
  
  if(attr1!=attr2){
  
  cov_i <- all_covariates |> 
           filter(Attribute %in% c(attr1,attr2))|>
           select(any_of(c("Unit","Year","Attribute","Percentage"))) |>
           distinct(Unit,Year,Attribute,Percentage)                  |>
           pivot_wider(c(Unit,Year),names_from = Attribute,values_from = Percentage) |>
           filter(if_any(c(attr1), ~ !is.na(.x))) |>
           filter(if_any(c(attr2), ~ !is.na(.x))) |>
           select(all_of(c(attr1,attr2))) |>
           distinct() |>
           rename("attr1"=attr1,"attr2"=attr2)
  
  corr_i <- cor(cov_i$attr1,cov_i$attr2)
  
  corr_i <- tibble(attr1=attr1,attr2=attr2,corr=corr_i)
  }else{
    
    corr_i <- tibble(attr1=attr1,attr2=attr2,corr=1)
    
    
  }
  
  all_corr <- bind_rows(all_corr,corr_i)
}

saveRDS(all_corr,here("5. EDA","correlations_predictors.rds"))
(all_corr |> 
    ggplot(aes(x=attr1,y=attr2,fill=corr)) +
    geom_tile() +
    scale_fill_gradient2(low="red",mid="white", high="blue") +
    base_theme +
    theme(axis.text.x =element_text(angle=90,hjust=1))) |> 
  customthemes::save_image(path(bulk_dir,str_c("correlations_predictors.png")))


rm(list=ls()[!(ls() %in% keep_vars)])


## Histograms and relation with responses - SD ----
## using ony primary vote diffrence from national average
tables_ced <- tables[str_detect(tables,"sd$")]

corr_nat   <- tibble()
all_covariates <- tibble()
keep_vars <- c(keep_vars,"corr_nat","all_covariates","tables_ced","all_corr")

for(table in tables_ced){
  attr <- dbListFields(processed_db,table)
  t <- tbl(processed_db,table)
  
  is_formatted <- any(str_detect(attr,"Attribute"))
  
  if(!is_formatted){
    t_collected <- t |>
      filter(Year!=2021) |>
      collect() |>
      mutate(across(!any_of(c("DivisionNm","Year")), as.numeric)) |>
      pivot_longer(-any_of(c("DivisionNm","Year")),names_to = "Attribute",values_to = "Percentage") 
    
  }else{
    t_collected <- t |>
      filter(Year!=2021) |>
      collect()
  }
  
  attr_values <- unique(t_collected$Attribute)
  
  p <- list()
  
  for(i in 1:length(attr_values)){
    print(attr_values[i])
    
    t_collected_i <-  t_collected |>
      filter(Attribute==attr_values[i])
    
    p[[i]] <- t_collected_i |>
      ggplot(aes(x=Percentage)) +
      geom_density()          +
      coord_flip() +
      facet_grid(Year ~.)   +
      labs(title=attr_values[i]) + 
      base_theme 
    
    
    against_response <- t_collected_i |>
      rename("Census"="Percentage") |>
      left_join(year_equivalency |> mutate(election_years=as.numeric(election_years)),
                by=c("Year"="census_years")) |>
      select(-Attribute) |>
      left_join(primary_vote |> collect(),
                by=c("election_years"="Year","DivisionNm"="DivisionNm")
      ) |>
      filter(!is.na(PartyAb))
  
    
    p_against_response_nat <- against_response |>
      ggplot(aes(x=Census,y=Percentage_Diff_National,colour=StateAb)) +
      geom_point() +
      facet_grid(Year ~ PartyAb) +
      labs(x=attr_values[i],title=str_c(attr_values[i], " - NATIONAL"),y="Primary Vote") +
      base_theme +
      #  scale_colour_manual(values=auspol::manage_colours(extra_values = parties))
      scale_colour_manual(values=my_palette) + 
      theme(legend.position = "bottom")
    
    customthemes::save_image(p_against_response_nat, path(bulk_dir,str_c("2_sigma_",attr_values[i],"_against_reponse_nat_diff.png")))
    
    #by State
    
    for(state in state_acronyms){
      
      p_against_response_nat <- against_response |>
        filter(StateAb==state)                   |>
        ggplot(aes(x=Census,y=Percentage_Diff_State,colour=StateAb)) +
        geom_point() +
        facet_grid(Year ~ PartyAb) +
        labs(x=attr_values[i],title=attr_values[i],y="Primary Vote") +
        base_theme +
        #  scale_colour_manual(values=auspol::manage_colours(extra_values = parties))
        scale_colour_manual(values=my_palette) + 
        theme(legend.position = "bottom")
      
      customthemes::save_image(p_against_response_nat, path(bulk_dir,str_c("2_sigma_",attr_values[i],"_",state,"_against_reponse_nat_diff.png")))
      
    }
    
    
    corr_bas <- against_response |>
      select(Census,PartyAb,Percentage,
             Percentage_Diff_State,
             Percentage_Diff_National) |>
      distinct()                        |>
      filter(Census!=0)  
    
    for(party in parties){

      cb_nat <- corr_bas |>
        filter(PartyAb==party) |>
        filter(!is.na(Percentage_Diff_National)) |>
        distinct(Census,Percentage_Diff_National)
      
      corr_nat_i <- tibble(party=party,Attribute=attr_values[i],corr=cor(cb_nat$Census,cb_nat$Percentage_Diff_National))
    
      corr_nat <- bind_rows(corr_nat,corr_nat_i)

    }
    
    all_covariates <- bind_rows(all_covariates,t_collected)
    
  }
  patchwork::wrap_plots(p) |> customthemes::save_image(path(bulk_dir,str_c("2_sigma_",table,"_hist.png")))
  rm(list=ls()[!(ls() %in% keep_vars)])
  
}


(corr_nat |> 
    ggplot(aes(x=party,y=Attribute,fill=corr)) +
    geom_tile() +
    scale_fill_gradient2(low="red",mid="white", high="blue") +
    base_theme) |> 
  customthemes::save_image(path(bulk_dir,str_c("2_sigma_correlations_nat.png")))



# correlation with between predictors

all_attributes <- unique(all_covariates$Attribute)

attribute_grid <- expand.grid(all_attributes,all_attributes)

all_corr <- tibble()
keep_vars <- c(keep_vars,"all_attributes","attribute_grid","all_corr")
for(i in 1:nrow(attribute_grid)){
  
  attr1 <- attribute_grid[i,]$Var1
  attr2 <- attribute_grid[i,]$Var2
  
  if(attr1!=attr2){
    
    cov_i <- all_covariates |> 
      filter(Attribute %in% c(attr1,attr2))|>
      select(any_of(c("Year","DivisionNm","Attribute","Percentage"))) |>
      distinct(Year,DivisionNm,Attribute,Percentage)                  |>
      pivot_wider(c(Year,DivisionNm),names_from = Attribute,values_from = Percentage) |>
      filter(if_any(c(attr1), ~ !is.na(.x))) |>
      filter(if_any(c(attr2), ~ !is.na(.x))) |>
      select(all_of(c(attr1,attr2))) |>
      distinct() |>
      rename("attr1"=attr1,"attr2"=attr2)
    
    corr_i <- cor(cov_i$attr1,cov_i$attr2)
    
    corr_i <- tibble(attr1=attr1,attr2=attr2,corr=corr_i)
  }else{
    
    corr_i <- tibble(attr1=attr1,attr2=attr2,corr=1)
    
    
  }
  
  all_corr <- bind_rows(all_corr,corr_i)
  
  rm(list=ls()[!(ls() %in% keep_vars)])
  
}

saveRDS(all_corr,here("5. EDA","2_sigma_correlations_predictors.rds"))
(all_corr |> 
    ggplot(aes(x=attr1,y=attr2,fill=corr)) +
    geom_tile() +
    scale_fill_gradient2(low="red",mid="white", high="blue") +
    base_theme +
    theme(axis.text.x =element_text(angle=90,hjust=1))) |> 
  customthemes::save_image(path(bulk_dir,str_c("2_sigma_correlations_predictors.png")))


## disconnect ----

dbDisconnect(processed_db, shutdown=TRUE)


