###########
# This script will download the relevant vignettes from auspol,auscensus and aussiemaps and will clean them up to be added as appendices
###########


library(tidyverse)
library(here)
library(fs)
library(glue)
library(lubridate)

packages <- tribble(~url, ~destination, ~title, ~source_url,
                    "https://raw.githubusercontent.com/carlosyanez/auscensus/main/precompile/complex_case.Rmd",
                    here("7. Report","81-auscensus.Rmd"),
                    "# (APPENDIX) {auscensus} Vignette",
                    "https://gh.carlosyanez.id.au/auscensus/articles/complex_case.html",
                    
                    "https://github.com/carlosyanez/auspol/raw/main/precompile/house_primary_vote.Rmd",
                    here("7. Report","82-auspol.Rmd"),
                    "# (APPENDIX) {auspol} Vignette",
                    "https://gh.carlosyanez.id.au/auspol/articles/house_primary_vote.html",
                    
                    
                    
                    "https://github.com/carlosyanez/aussiemaps/raw/master/precompile/aussiemaps.Rmd",
                    here("7. Report","83-aussiemaps.Rmd"),
                    "# (APPENDIX) {aussiemaps} Vignette",
                    "https://gh.carlosyanez.id.au/aussiemaps/articles/aussiemaps.html",
                    
                    
                    
                    )



for(m in 1:nrow(packages)){

url <- packages[m,]$url
destination <- packages[m,]$destination
title <- packages[m,]$title
source_url <- packages[m,]$source_url



temp_file <- tempfile()
download.file(url,temp_file)

# from https://blog.autarkaw.com/2021/03/10/removing-yaml-from-an-rmd-file-through-an-r-script/
# modified version

# Open the file to read
Input_File <- file(temp_file,open="r")
linn <-readLines(Input_File)
file_lines <- length(linn)

# icapture is a vector which will check the two lines 
# that have --- in them.
icapture <- vector(,10)

# Just printing the lines in the rmd file, not needed.
#for (i in 1:length(linn)){
# print(linn[i])
#}

#The name of the file which will store YAML free RMD file.
YAML_Remove_File <- file(destination,open="w")

#add initial lines to file


writeLines(title,YAML_Remove_File)

today <- now()

note <- glue("*Extracted from {source_url} on {wday(today, label = TRUE, abbr = FALSE)} {day(today)} {month(today, label=TRUE, abbr=FALSE)} {year(today)}*")
writeLines(note,YAML_Remove_File)


j <- 0
#Capturing the two line numbers where --- exists
for (i in 1:file_lines){
  if(strtrim(linn[i],3) == "---"){
    j <- j+1
    icapture[j] <- i
  }}
  
#find setup
setup_start <- TRUE
setup_end   <- TRUE
i<-icapture[2]+1
while(setup_start){
    detected <- str_detect(linn[i],"r setup")
    
    if(detected){
      chunk_start <- i
      setup_start <- FALSE
    }
    i <- i+ 1
}

i<-chunk_start+1
while(setup_end){
  detected <- str_detect(linn[i],"```")
  
  if(detected){
    chunk_end <- i
    setup_end <- FALSE
  }
  i <- i+ 1
}  
  
excluded_range <- chunk_start:chunk_end

for(n in icapture[2]+1:file_lines){
  if(!(n %in% excluded_range)){
    line <- linn[n]
    if(!is.na(line)){   
      writeLines(line,YAML_Remove_File) 
    }
  }
  
  
}


#close the input and output files
close(Input_File)
close(YAML_Remove_File)
file_delete(temp_file)
}
rm(list=ls())
.rs.restartR()
