# number formatting ----
knitr::opts_chunk$set(warning=FALSE,message = FALSE,echo=FALSE)
knitr::knit_hooks$set(inline = function(x) { if(!is.numeric(x)){ x }else{ formatC(x,big.mark = " ",digits = 2,format="f") } })

##ggplot default theming ----

ggplot2::theme_set(ggplot2:::theme_minimal())
##use theme_update to add things

##default flextable theming -----

flex_default <- function(x,caption,id){
  
x |>
    flextable::flextable() |>
    flextable::set_caption(caption, 
                autonum = officer::run_autonum(seq_id = "tab",
                                               bkm = id))|>
flextable::theme_alafoli()
}


## colour palettes ----
clusters_colours <- colRoz::colRoz_pal(name = "ngadju", n = 3, type = "discrete")
names(clusters_colours) <- as.character(0:2)

### map of australian electorates -----

australian_ced_map <- function(data, ced_gpkg,fill_col,colour_palette,plot_title){
  
  map      <- st_read(ced_gpkg,quiet=TRUE) |>
              left_join(data,by="DivisionNm")
  
  ##remove Lord Howe Island in Sydney and Ext. Territories from Lingiari
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
  
  map <- map |>
    filter(DivisionNm!="Sydney") |>
    filter(DivisionNm!="Lingiari") |>
    bind_rows(syd,lin)
  
  national <- map |>
    select(any_of(c("DivisionNm",fill_col))) |>
    rename("filler"=fill_col)|>
    ggplot(aes(fill=filler)) +
    geom_sf(colour="grey70") +
    theme_void() +
    scale_fill_manual(values = colour_palette,name=fill_col) +
    theme(legend.position = "bottom",
          legend.direction = "horizontal")
  
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
  
  
  bris + ade + perth +
    can + syd + melb +
    national +
    plot_layout(design = "11225555
                        33445555
                        77776666
                        77776666
                        77776666") +
    plot_annotation(title="Clustering of 2016 Electoral Divisions")
  
}
