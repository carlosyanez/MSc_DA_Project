#install.packages("renv")
renv::init()
renv::activate()

options(repos = c(CRAN = 'https://cloud.r-project.org'))

install.packages(c("remotes","tidyverse","just.install"))
remotes::install_github("carlosyanez/just.install")
#remotes::install_local("/Volumes/GoogleDrive/My Drive/GitHub/just.install")
.rs.restartR()

packages_to_install  <- tibble::tribble(~package,       ~source,~url,
                                  "here",                    "CRAN",       "",
                                  "fs",                      "CRAN",       "",
                                  "box",                     "CRAN",       "",
                                  "rmarkdown",               "CRAN",       "",
                                  "markdown",                "CRAN",       "",
                                  "knitr",                   "CRAN",       "",
                                  "gitignore",               "CRAN",       "",
                                  "usethis",                 "CRAN",       "",
                                  "customthemes",            "r-universe", "https://carlosyanez.r-universe.dev",
                                  "trackdown",               "CRAN",       "",
                                  "echarts4r",               "CRAN",       "",
                                  "ganttrify",               "github",    "giocomai/ganttrify",
                                  "xaringan",                "CRAN",      "",
                                  
                                  

)


just.install::justinstall(packages_to_install)
renv::snapshot(prompt = FALSE)

gitignore.file <- here::here(".gitignore")
new_lines <- gitignore::gi_fetch_templates("r")
gitignore::gi_write_gitignore(fetched_template = new_lines, gitignore_file = gitignore.file)
usethis::git_vaccinate()
#write("files", gitignore.file, append = TRUE)

#create folders
fs::dir_create(c("summary","presentation"))


rm(list = ls())
