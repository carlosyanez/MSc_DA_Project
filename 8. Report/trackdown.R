library(trackdown)
library(fs)
## Settings #####
gdrive_path <- "msc_project_proofreading"              # the folder on google drive


#upload for first time
filenames <- dir_ls(regexp = "Rmd")
  
  
filename <- filenames[9]
  
#upload for first time
trackdown::upload_file(filename, gpath=gdrive_path,hide_code=TRUE) 
#update google file
trackdown::update_file(filename, gpath=gdrive_path,hide_code=TRUE) 
#sync RMD back with google
trackdown::download_file(filename, gpath=gdrive_path) 

