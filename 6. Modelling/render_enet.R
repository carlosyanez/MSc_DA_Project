library(quarto)
library(here)
library(glue)


input_files  <- c(here("6. Modelling","regression_cluster.qmd"),
                  here("6. Modelling","regression_cluster_delta.qmd"))

tuning_grid <- seq(0,1,0.0002)


setwd(here("6. Modelling"))
  
for(c in c(0,1,2)){
  for(input_file in input_files){
    quarto_render(input = input_file,
                  output_file = glue("regression_cluster_{c}.html"),
                  execute_params =list(cluster=c,a_grid))
  }
}             