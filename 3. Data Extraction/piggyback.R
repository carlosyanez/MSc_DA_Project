library(piggyback)
library(fs)
library(zip)

files_dir      <- here("0. Data")
repo           <- "carlosyanez/MSc_DA_Project"
version       <- "data"

#create new release
#pb_new_release(repo,version)


#list and zip files
files <- dir_ls(files_dir)
zip_file <- "data.zip"

zip(zip_file,files,mode="cherry-pick")

# upload catalogue items ---
pb_upload(file=zip_file,repo,version)

file_delete(zip_file)