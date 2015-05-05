all : handout.pdf handout-code.r

handout.pdf handout-code.r : handout.Rmd render.r template.tex
	Rscript render.r
