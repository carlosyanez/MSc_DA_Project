#script to download data from github repo

library(here)
library(fs)
library(zip)
library(piggyback)

repo           <- "carlosyanez/MSc_DA_Project"
version       <- "data"
files_dir      <- here("4. Data")


pb_download(file="data.zip",
            dest=files_dir,
            repo=repo,
            tag=version,
            overwrite = TRUE)

unzip(zipfile=path(files_dir,"data.zip"),
      junkpaths = TRUE,
      exdir = files_dir
      )

file_delete(path(files_dir,"data.zip"))
