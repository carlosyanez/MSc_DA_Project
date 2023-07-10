library(here)
library(renderthis)

to_html(here("index.Rmd"),here("index.html"))
#uncomment lines in presentation.Rmd
to_pdf(here("index.Rmd"),here("Viva - Carlos YANEZ SANTIBANEZ.pdf"),keep_intermediates=FALSE,complex_slides = TRUE,delay=20)


?to_pdf
