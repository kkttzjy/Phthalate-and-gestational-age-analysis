# Associations between Mixtures of Urinary Phthalate Metabolites with Gestational Age at Delivery: A Time to Event Analysis using Summative Phthalate Risk Scores
## Data
"rawdata.rds" is the simulated data set with similar characteristics as the original data set. All codes can be ran on the data "rawdata.rds".

## Methods
### Analysis with Average Exposure
#### Single-Pollutant Models
Logistic regression, Cox model and accelerated failure time (AFT) model are used with r codes:  
"mean logistic.r", "mean cox.r" and "mean AFT.r".

In addition, we also fit logitic regression without log transformation on phthalte exposures with "mean logisitc without log transformation.R".

#### Multi-pollutant Models
ERS-Corr, ERS-Stepwise, WQS-Corr, WQS-Stepwise are fitted with logistic regression, Cox model and AFT model. Codes include:  
"mean ERS-Corr.R", "mean ERS-Stepwise.R" and "mean WQS.R".  

The corresponding weights used for ERS-Corr, ERS-Stepwise, WQS-Corr, WQS-Stepwise are calculated in "ERS Weights.R".

In addition, we also try ERS and WQS with all phthalates with "WQS/ERS with all phthalates.R".

Furthermore, we also use Cox model/AFT model to generate weights for ERS with "ERS Weight Cox and AFT.R"and "mean ERS with Cox and AFT weight.R".

Finally, we also try to split 70% as training set and 30% as test set to run sensitivity analysis on WQS with "mean WQS 70% train and 30% test.R".

### Analysis with Repeated Measures of Exposure
#### Single-Pollutant Models
Two stage models are fitted. The first stage model is a linear mixed model with random intercepts to the repeated measures of the phthalate levels. The second stage model is logistic regression, Cox model and accelerated failure time (AFT) model with the estimated random intercepts from the first stage model. R codes contain:  
"time-variying single phthalate.r".

Also, we calculate the correlation between random intercepts and average phthalate exposures with "correlation between mean phthalte and random intercept.R".

#### Multi-pollutant Models
Similar with single-pollutatnt model, time-varying ERS-Corr and ERS-Stepwise are fitted with linear mixed model with random intercept in the first stage model. The same kind of second stage model is used. Codes include:  
"time-varying ERS-Corr.R", "time-varying ERS-stepwise.R". 

## Codes for figures and tables
"figure 1.R", "Table S1&Figure S1.R", "figure S2.R", "figure S3.R"  
"phthalate descriptive statistics.R", "scatter plot check concordance ERS from different weights.R", "scatter plot ERS-All ERS-Corr ERS-Stepwise.R", "check similarity between Visit 4 and first three.R".

## R function used in WQS calculation
"quantile_f.R" is used in WQS calculation to transform continuous phthalate exposures into quantiles indicators.

## Software information
### Programming Language: R
### Software Version: R version 3.4.4
### Operating System: macOS

