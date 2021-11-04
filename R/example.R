rm(list = ls())
here::i_am("R/example.R") 

source(here::here('R', 'Fun.R')) 
library(ggplot2)
library(glmnet)
library(ordinalNet)
library(doParallel)

#######################
### Input
#######################
sample_data = read.csv(here::here('data','sample_data.csv'))
dictionary = read.csv(here::here('data', 'dict.csv'))


#######################
### Filtering
#######################
#### Filter out variables with high missing rates and low minority frequencies
#### Default:
#### Binary & Ordinal : missing rates >= 0.95 and low of minority class frequencies <= 0.05
#### Continuous: missing rates > 0.95 ----
#### If want to process without screening, set missing_rate_control = 1, low_frequency_control = 0.05  
stepI_filtering_list = 
  stepI_filtering(data = sample_data, dict = dictionary, missing_rate_control = 0.90, low_frequency_control = 0.05)
sample_data_filtered = stepI_filtering_list$data_filtered
dictionary_updated = stepI_filtering_list$dictionary

#### Summary of Missingness
summary_missingness(data = sample_data, dict = dictionary)
summary_missingness(data = sample_data_filtered, dict = dictionary_updated)

#######################
### Variable Screening
#######################
##### Identity variables that would be included as predictors in the imputation model
##### based on Pearson correlation. For each variable waiting to be imputed, exclude predictors with the same missing patterns.
##### Default:
##### no adjusted for false discovery rate, see ?p.adjust for adjustement methods
##### screening variables with p values (adjusted p-values) > 0.05.
##### if no adjustment needed, set adjust_method = "none"
##### The function will return a list of the name of variables used for each imputation model.
screened_list = stepII_screening(Ymat = sample_data_filtered, Xmat = sample_data_filtered)


#######################
### Imputation Models
#######################
##### parallel imputation
##### The function will return a matrix consists of imputed results and observed values
imputed_result_mat = impute_scale(Ymat = sample_data_filtered, Xmat = sample_data_filtered, dict = dictionary_updated, vars_list = screened_list)

summary_missingness(data = imputed_result_mat, dict = dictionary)

