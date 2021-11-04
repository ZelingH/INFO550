report: Rmd/example.Rmd 
	Rscript -e "rmarkdown::render('Rmd/example.Rmd')"

retore: renv.lock
	Rscript -e 'renv::restore(prompt = FALSE)'

.PHONY: report restore

