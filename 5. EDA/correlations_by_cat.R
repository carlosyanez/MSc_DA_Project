library(tidyverse)
library(here)

base_theme <- customthemes::custom_plot_theme_md(google_font = "Roboto",background_colour = "white", 
                                                 legend_pos = "botom",legend_dir = "horizontal",
                                                 title_size = 90,
                                                 subtitle_size = 36,
                                                 caption_size = 30,
                                                 axis_size = 30)+
  theme(legend.text = ggtext::element_markdown(size=30),
        axis.text = ggtext::element_markdown(size=50),
        strip.text.x = element_text(size = 30),
        strip.text.y = element_text(size = 30))

correlations_predictors <- readRDS(here("5. EDA","correlations_predictors.rds"))

correlations_predictors <- correlations_predictors |>
                           mutate(cat1=str_extract(attr1,"^([^-...])+"),
                                  cat2=str_extract(attr2,"^([^-...])+")) |>
                           mutate(across(starts_with("cat"), ~ str_squish(.x)))

categories <- correlations_predictors|>distinct(cat1) |> pull()

for(category in categories){
  (correlations_predictors |> 
     filter(cat1==category & cat2==category) |>
     ggplot(aes(x=attr1,y=attr2,fill=corr)) +
     geom_tile() +
     geom_text(aes(label=round(corr,3)),size=20)+
     scale_fill_gradient2(low="red",mid="white", high="blue") +
     base_theme +
     theme(axis.text.x =element_text(angle=90,hjust=1,size=40),
           axis.text.y = element_text(size=40))+ 
     labs(title=glue::glue("Correlation against {category} attributes"),
          caption="Source: ABS - Censuses")) |> 
    customthemes::save_image(glue::glue("5. EDA/correlations_by_group/{category}.png"))
  
}
