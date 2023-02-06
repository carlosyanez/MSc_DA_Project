library(here)
library(fs)
library(zip)
library(piggyback)
library(stringr)

#.rs.restartR()

data_dir <- here("4. Data")

#clean data dir
files <- dir_ls(data_dir)
files <- files[str_detect(files,"duckdb")]
file_delete(files)

#download files

pb_download(dest=data_dir,repo="carlosyanez/MSc_DA_Project")

files <- dir_ls(data_dir)
files <- files[str_detect(files,"zip")]


for (file in files){
  unzip(file, junkpaths = TRUE,exdir = data_dir)
}
file_delete(files)

rm(list=ls())
