library(here)
library(renderthis)
library(fs)


to_html(here("index.Rmd"),here("index.html"),self_contained = FALSE)

book_path <- path(here(),"..","8. Report","_book")
presentation_path <- dir_create(path(book_path,"presentation"))
img_path          <- dir_create(path(presentation_path,"img"))

#move html and lib folder
html_files <- dir_ls(here(),regexp = "html$")
file_move(html_files,presentation_path)
file_move(here("libs"),presentation_path)

#copy css
css_files <- dir_ls(here(),regexp = "css$")
file_copy(css_files,presentation_path)
#copy img
img_files <- dir_ls(here("img"))
file_copy(img_files,img_path)

#uncomment lines in presentation.Rmd
to_pdf(here("index.Rmd"),here("Viva - Carlos YANEZ SANTIBANEZ.pdf"),keep_intermediates=FALSE,complex_slides = TRUE,delay=20)


?to_pdf
