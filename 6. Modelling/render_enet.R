library(quarto)
library(here)
library(glue)
library(fs)
library(stringr)


input_files  <- c(here("6. Modelling","regression_cluster.qmd"),
                  here("6. Modelling","regression_cluster_delta.qmd"),
                  here("6. Modelling","regression_cluster_delta_state.qmd"),
                  here("6. Modelling","regression_cluster_delta_cluster.qmd"),
                  here("6. Modelling","regression_cluster_delta_cluster_univariate.qmd"))

input_files  <- c(
                  
                  #here("6. Modelling","regression_cluster_delta_state_state.qmd"),
                  here("6. Modelling","regression_cluster_delta_cluster_cluster.qmd"))

tuning_grid <- seq(0,1,0.0004)


setwd(here("6. Modelling"))
  
for(c in c(0,1,2)){
  for(input_file in input_files){
    
    out_filename <- fs::path_file(input_file)
    out_filename <- str_remove(out_filename,"\\.qmd")
    
    quarto_render(input = input_file,
                  output_file = glue("{out_filename}_{c}.html"),
                  execute_params =list(cluster=c,a_grid=tuning_grid))
  }
}             
