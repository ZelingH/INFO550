## ALMI: Automated Large-scale Imputation Pipline

### Introduction

As rich genetic and phenotypic data are increasingly being captured in large-scale real world data (RWD), they become promising resources for discovery novel genotype-phenotype associations. However, despite the potential of such large-scale RWD, missing data is a common problem that can have a substantial impact on the results of association analyses.

ALMI implements accelerated large-scale multiple imputation algorithm for RWD that can efficiently impute a large number of variables. ALMI begins with an observational data set with missing values, and a data dictionary classified all variables in the data set into binary, ordinal and continuous. The standardized steps include:

* filter out variables with high missing rates and low minority class frequencies.
* select potential predictors for building the imputation models.
* running automated procedures for paralleled multiple imputation.


The data we used for illustration is called `sample_data.csv`, with its dictionary file `dict.csv`. They are both included in this repository.


 ## Run the Analysis

Step 1.  Download the  `INFO550` folder from Github repository:

```bash
git clone https://github.com/ZelingH/INFO550.git
cd INFO550
```
Step 2. Lauch Docker on your local desktop and pull the container image from Dockerhub:

```bash
docker pull liddyhe/info550 
```

Step 3. Get the current working directory first, then mount directories to retrieve analysis result `example.html`. The result will be saved under `/Rmd` subfolder.

```bash
pwd
docker run -v path_to_folder:/project liddyhe/info550
cd Rmd
```

Step 3(a). Alternatively, you could take a look at the analysis result in the Docker container by running

```bash
docker run -it liddyhe/info550 /bin/bash
```
Then you may go to the `Rmd` folder to see the result

```bash
cd Rmd
```



