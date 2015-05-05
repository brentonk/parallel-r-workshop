library("knitr")
library("rmarkdown")

render("handout.Rmd")
purl("handout.Rmd", output = "handout-code.r")
