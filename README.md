## ALMI: Automated Large-scale Imputation Pipline



As rich genetic and phenotypic data are increasingly being captured in large-scale real world data (RWD), they become promising resources for discovery novel genotype-phenotype associations. However, despite the potential of such large-scale RWD, missing data is a common problem that can have a substantial impact on the results of association analyses.

ALMI implements accelerated large-scale multiple imputation algorithm for RWD that can efficiently impute a large number of variables. ALMI begins with an observational data set with missing values, and a data dictionary classified all variables in the data set into binary, ordinal and continuous. The standardized steps include:

* filter out variables with high missing rates and low minority class frequencies.
* select potential predictors for building the imputation models.
* running automated procedures for paralleled multiple imputation.


The data we used for illustration is called `sample_data.csv`, with its dictionary file `dict.csv`. They are both included in this repository.

To analyze the data you will first need to syhcronize your local directory: open R in under the project folder and run

```R
renv::restore()
```

After setting up your environment, go back to the project folder in your terminal and type

```bash
make
```

It will call the `Makefile` and generate `example.html` under Rmd subfolder. Please be patient - it takes a few minutes to impute around 100 mixed types of variables. You may see some warning messages regarding the ties when computing p values of correlation tests - it will not affect the results and please ignore them.



