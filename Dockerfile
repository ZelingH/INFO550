FROM rocker/tidyverse

# install R packages
RUN Rscript -e "install.packages(c('rmarkdown','pander','ggplot2','glmnet','ordinalNet','doParallel','here'),repos = 'http://cran.us.r-project.org')"

# make a project directory in the container
RUN mkdir /project

# copy contents of local folder to project folder in container
COPY ./ /project/

# make R scripts executable
RUN chmod +x /project/

WORKDIR /project

# make container entry point build report
CMD make -f Makefile