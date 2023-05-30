library(quarto)
library(here)
library(glue)


input_file  <- here("6. Modelling","regression_cluster.qmd")

setwd(here("6. Modelling"))
  
for(c in c(0,1,2)){
  quarto_render(input = input_file,
                output_file = glue("regression_cluster_{c}.html"),
                execute_params =list(cluster=c))
}             