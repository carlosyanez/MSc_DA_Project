# number formatting
knitr::opts_chunk$set(warning=FALSE,message = FALSE,echo=FALSE)
knitr::knit_hooks$set(inline = function(x) { if(!is.numeric(x)){ x }else{ formatC(x,big.mark = " ",digits = 2,format="f") } })

##ggplot default theming

ggplot2::theme_set(ggplot2:::theme_minimal())
##use theme_update to add things

##default flextable theming

flex_default <- function(x,caption,id){
  
x |>
    flextable::flextable() |>
    flextable::set_caption(caption, 
                autonum = officer::run_autonum(seq_id = "tab",
                                               bkm = id))|>
flextable::theme_alafoli()
}
