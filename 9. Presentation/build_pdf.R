library(here)
library(xaringanBuilder)

build_html(here("presentation","presentation.Rmd"),here("presentation","presentation.html"))
#uncomment lines in presentation.Rmd
build_pdf(here("presentation","presentation.Rmd"),here("presentation","presentation.pdf"))


?build_pdf
