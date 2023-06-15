# number formatting ----
knitr::opts_chunk$set(warning=FALSE,message = FALSE,echo=FALSE)
knitr::knit_hooks$set(inline = function(x) { if(!is.numeric(x)){ x }else{ formatC(x,big.mark = " ",digits = 2,format="f") } })

##ggplot default theming ----

ggplot2::theme_set(ggplot2:::theme_minimal())
customthemes::set_plot_colours(line_colour = "#00843D",
                               fill_colour = "#00843D",
                               HVline_colour = "#00843D",
                               alt_fill_colour = "#00843D")
##use theme_update to add things

##default flextable theming -----

flex_default <- function(x,caption,id){
  
x |>
    flextable::flextable() |>
    flextable::set_caption(caption, 
                autonum = officer::run_autonum(seq_id = "tab",
                                               bkm = id))|>
flextable::theme_booktabs()
}


## colour palettes ----
clusters_colours <- colRoz::colRoz_pal(name = "ngadju", n = 3, type = "discrete")
names(clusters_colours) <- as.character(0:2)

party_cols <- c("GRN","ALP","COAL","Other")
party_palette <- auspol::party_colours()[party_cols]

### map of australian electorates -----

australian_ced_map <- function(data, ced_gpkg,fill_col,colour_palette,plot_title){
  
  map      <- st_read(ced_gpkg,quiet=TRUE) |>
              left_join(data,by="DivisionNm")
  
  ##remove Lord Howe Island in Sydney and Ext. Territories from Lingiari, Fenner, Bean
  syd <- map |> 
    filter(DivisionNm=="Sydney") |>
    st_cast("POLYGON")
  syd$area <- st_area(syd)
  syd <- syd |> filter(area==max(area))
  
  lin <- map |> 
    filter(DivisionNm=="Lingiari") |>
    st_cast("POLYGON")
  lin$area <- st_area(lin)
  lin <- lin |> filter(area==max(area))
  
  fen <- map |> 
    filter(DivisionNm=="Fenner") |>
    st_cast("POLYGON")
  fen$area <- st_area(fen)
  fen <- fen |> filter(area==max(area))

  bean <- map |> 
    filter(DivisionNm=="Bean") |>
    st_cast("POLYGON")
  bean$area <- st_area(bean)
  bean <- bean |> filter(area==max(area))
  
  map <- map |>
    filter(DivisionNm!="Sydney") |>
    filter(DivisionNm!="Lingiari") |>
    filter(DivisionNm!="Fenner") |>
    filter(DivisionNm!="Bean") |>
    bind_rows(syd,lin,fen,bean)
  
  
  melb <-  map |>
    filter(str_detect(Metro_Area,"Melbourne")) |>
    select(any_of(c("DivisionNm",fill_col))) |>
    rename("filler"=fill_col)|>
    ggplot(aes(fill=filler)) +
    geom_sf(colour="grey70") +
    theme_void() +
    scale_fill_manual(values = colour_palette) +
    labs(subtitle = "Melbourne") +
    theme(legend.position = "none")
  
  syd <-  map |>
    filter(str_detect(Metro_Area,"Sydney")) |>
    filter(!(DivisionNm %in% c("Hume"))) |>
    select(any_of(c("DivisionNm",fill_col))) |>
    rename("filler"=fill_col)|>
    ggplot(aes(fill=filler)) +
    geom_sf(colour="grey70") +
    theme_void() +
    scale_fill_manual(values = colour_palette) +
    labs(subtitle = "Sydney")  +
    theme(legend.position = "none")
  
  can <-  map |>
    filter(str_detect(Metro_Area,"Capital")) |>
    select(any_of(c("DivisionNm",fill_col))) |>
    rename("filler"=fill_col)|>
    ggplot(aes(fill=filler)) +
    geom_sf(colour="grey70") +
    theme_void() +
    scale_fill_manual(values = colour_palette) +
    labs(subtitle = "Canberra")  +
    theme(legend.position = "none") 
  
  
  bris <-  map |>
    filter(str_detect(Metro_Area,"Brisbane")) |>
    select(any_of(c("DivisionNm",fill_col))) |>
    rename("filler"=fill_col)|>
    ggplot(aes(fill=filler)) +
    geom_sf(colour="grey70") +
    theme_void() +
    scale_fill_manual(values = colour_palette) +
    labs(subtitle = "Brisbane")  +
    theme(legend.position = "none")
  
  ade <- map |>
    filter(str_detect(Metro_Area,"Adelaide")) |>
    select(any_of(c("DivisionNm",fill_col))) |>
    rename("filler"=fill_col)|>
    ggplot(aes(fill=filler)) +
    geom_sf(colour="grey70") +
    theme_void() +
    scale_fill_manual(values = colour_palette) +
    labs(subtitle = "Adelaide")  +
    theme(legend.position = "none")
  
  
  perth <- map |>
    filter(str_detect(Metro_Area,"Perth")) |>
    select(any_of(c("DivisionNm",fill_col))) |>
    rename("filler"=fill_col)|>
    ggplot(aes(fill=filler)) +
    geom_sf(colour="grey70") +
    theme_void() +
    scale_fill_manual(values = colour_palette) +
    labs(subtitle = "Perth")  +
    theme(legend.position = "none")
  
  metros <- bind_rows(bris$data,ade$data,perth$data,can$data,syd$data,melb$data) |>
            st_drop_geometry() |>
            pull(DivisionNm)
  
  national <- map |>
    filter(!(DivisionNm %in% metros)) |>
    select(any_of(c("DivisionNm",fill_col))) |>
    rename("filler"=fill_col)|>
    ggplot(aes(fill=filler)) +
    geom_sf(colour="grey70") +
    theme_void() +
    scale_fill_manual(values = colour_palette,name=fill_col) +
    theme(legend.position = "bottom",
          legend.direction = "horizontal") +
    labs(subtitle="Non Metropolitan")
  
  
  
  bris + ade + perth +
    can + syd + melb +
    national +
    plot_layout(design = "11225555
                        33445555
                        77776666
                        77776666
                        77776666") +
    plot_annotation(title=plot_title)
  
}


## cluster names ----
cluster_names <- tibble::tribble(~cluster, ~name_simple,~name_composite,
                                 0,       "Inner Metropolitan","Cluster 0: Inner Metropolitan",
                                 1,        "Regional","Cluster 1: Regional",
                                 2,        "Suburban","Cluster 2: Suburban")


##compare results ----
compare_results <- function(ced_name,parties=3,results,...){
  p<- auspol::house_primary_historic_plot(ced_name,
                                      include_others = TRUE,
                                      merge_parties=list("COAL"=c("LP","LNP","NAT","LIB")),
                                      parties=parties,
                                      ...) +
    ggnewscale::new_scale("colour") +
    ggnewscale::new_scale("shape")  +
    geom_point(data=results$prediction_compared |>
                 filter(str_detect(DivisionNm,ced_name)) |>
                 select(PartyAb,Predicted) |>
                 mutate(Type="Prediction")      |>
                 mutate(Year=2022),
               aes(x=Year,y=Predicted,colour=PartyAb,shape=Type),
               size=3,
               inherit.aes = FALSE) +
    scale_colour_manual(values=party_palette,name="Predictions") +
    scale_shape_manual(values=c("Prediction"=18,"Result"=1),name="Predictions") + 
    guides(color = FALSE) +
    labs(subtitle = ced_name)
}

census_plot <- function(demographic_data,ced_name,attributes){
  demographic_data |>
    filter(DivisionNm==ced_name) |>
    select(any_of(c("DivisionNm","Year",attributes)))                     |>
    pivot_longer(-c(DivisionNm,Year),names_to = "Attribute",values_to="Percentage") |>
    ggplot(aes(x=Year, y=Percentage,colour=Attribute,linetype=Attribute,shape=Attribute)) +
    geom_hline(yintercept = 0,colour="grey80",linewidth=1.1) +
    geom_point()+
    geom_line() +
    labs(subtitle = ced_name)
}


plot_grid <- function(p,titletext=NULL,...){  
  
  max_y <-c()
  min_y <-c()
  
  for(i in 1:length(p)){
    max_y <- max(p[[i]]$data$Percentage)
    min_y <- min(p[[i]]$data$Percentage)
  }
  
  max_y <- ceiling(max(max_y))
  min_y <- floor(min(min_y))
  
  for(i in 1:length(p)){
    p[[i]] <- p[[i]] + ylim(min_y,max_y)
    
  }
  
  wrap_plots(p,guides = 'collect',widths = c(3,3,1),...) +
    plot_annotation(title=titletext)
}
