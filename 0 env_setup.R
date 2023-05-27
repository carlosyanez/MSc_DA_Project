# renv::init(bare=TRUE)
# .rs.restartR()
options(repos = c(
  CRAN = "https://cloud.r-project.org",
  carlos="https://carlosyanez.r-universe.dev"
))

install.packages("just.install", repos ="https://carlosyanez.r-universe.dev")
install.packages("remotes")
install.packages("tidyverse")

packages_to_install <- tibble::tribble(
  ~package,            ~source,      ~url,
### general packages
  "here",                    "CRAN",        "",
  "fs",                      "CRAN",        "",
  "gitignore",               "CRAN",        "",
  "usethis",                 "CRAN",        "",
  "box",                     "CRAN",        "",
  "reticulate",              "CRAN",        "",
  "customthemes",            "Github",      "carlosyanez/customthemes", # custom theme package
  "uofgdataanalyticsreport", "Github",      "carlosyanez/uofgdataanalyticsreport", # this template
  "ggspatial",               "CRAN",        "",
  "sf",                      "CRAN",        "",
  "patchwork",               "CRAN",        "",
  "yaml",                    "CRAN",        "",
  "rmarkdown",               "CRAN",        "",
  "openxlsx",                "CRAN",        "",
  "flextable",               "CRAN",        "",
  "kableExtra",              "CRAN",        "",
  "rhandsontable",              "CRAN",        "",
### used for this project
  "ganttrify",               "Github",     "giocomai/ganttrify", # gantt charts
  "ochRe",                   "Github",       "ropenscilabs/ochRe",
### 
##  "bomrang","Github","ropensci/bomrang",   # get data from BOM # bomrang is not working anymore, due to changes on bom's website
 "cropgrowdays",              "CRAN",        "",  #Weather data from SiLO
 "readabs",                   "CRAN",        "",  #read ABS time series and data cubes
"auspol",                     "Github",       "carlosyanez/auspol",
"auscensus",                  "Github",       "carlosyanez/auscensus",
"aussiemaps",                 "Github",       "carlosyanez/aussiemaps", 
"ggbeeswarm",                 "CRAN",        "",
"plotly",                     "CRAN",        "",
"tinter",                     "CRAN",        "",
"patchwork",                  "CRAN",        "",
"DBI",                        "CRAN",        "",
"bookdown",                   "CRAN",         "",
"servr",                      "CRAN",         "",
"glmnet",                     "CRAN",         "",
"tidymodels",                 "CRAN",         "",
"rpart",                      "CRAN",         "",
"rpart.plot",                 "CRAN",         "",
"randomForest",               "CRAN",         "",
"ranger",                     "CRAN",         "",
"xgboost",                    "CRAN",         "",
"discrim",                    "CRAN",         "",
"klaR",                       "CRAN",         "",
"naivebayes",                 "CRAN",         "",
"nnet",                       "CRAN",         "",
"vip",                        "CRAN",         "",
)


#duckdb does not guarantee backwards compatility,install the right version:
remotes::install_version("duckdb", version = "0.6.0", repos = "http://cran.us.r-project.org")

just.install::justinstall(packages_to_install)


# set up python environment
#conda_env <- "msc_project"

#a <-reticulate::conda_create(
#  envname = conda_env 
#)

#reticulate::use_condaenv(conda_env)

#reticulate::conda_install(conda_env,
#                       c("pandas"))





## CREATE SNAPSHOT
renv::snapshot(prompt = FALSE)
#remove prefix from environment.yml (removing local reference, which is not needed)
#env_yml <- readLines("environment.yml")
#env_yml <- env_yml[1:(length(env_yml)-1)]
#write(env_yml,"environment.yml")

gitignore.file <- here::here(".gitignore")
new_lines <- gitignore::gi_fetch_templates("r")
gitignore::gi_write_gitignore(fetched_template = new_lines, gitignore_file = gitignore.file)
usethis::git_vaccinate()


rm(list = ls())


