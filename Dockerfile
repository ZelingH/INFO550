From rocker/tidyverse

# install R packages
Run Rscript -e "install.packages('pander','ggplot2','glmnet','ordinalNet','doParallel')"

# make a project directory in the container
Run mkdir /project

# copy contents of local folder to project folder in container
COPY ./ /project/

# make container entry point build report
CMD Rscript -e "rmarkdown::render('Rmd/example.Rmd')"