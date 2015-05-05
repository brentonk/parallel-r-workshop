library("knitr")
library("rmarkdown")

render("handout.Rmd", "pdf_document")
purl("handout.Rmd", output = "handout-code.r")
