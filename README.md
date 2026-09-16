# Insurance Claims Frequency & Severity Analysis

## Overview

This project analyzes automobile insurance policy data using R to investigate factors
associated with claim frequency and evaluate the substainability of the availability claim
amount variable for severity modeling.

This analyses uses generalized linear models (GLMs), model diagnostics, out-of-sample 
validation, and actuarial risk relativities to evaluate claim frequency,

A key dat-quality finding was that the available claim_amt variable does not behave as a 
conventional insurance claim severity measure. This limitation is documented as part of 
the analyses rather than using an innapropriate severity model. 

## Objectives 

- Explore the distribution of insurance claim frequency
- Investigate the relationship between policyholder, vehicle, policy characteristics
and claim frequency.
- Fit and compare Poisson and Negative Binomial frequency models.
- Diagnose and address overdispersion.
- Evaluate model performance using a holdout test set.
- Calculate interpretable claim frequency risk relatives
- Assess whether the available claim amount variable is suitable for severity modeling

## Methodology

Exploratory Data Analysis:

The analysis examined claim frequency across
- Age
- Gender
- Marital status
- Car use
- Number of children driving
- Parent status
- Education 
- Vehicle age
- Coverage zone
- Vehicle manufacturer
- Household income

The data was checked for missing values, duplicate policy IDs, invalid or inconsistent 
categorical values, and character-encoding issues


Claim Frequency Modeling: 

Claim frequency was initially modeled using a Poisson GLM.

The variance of the frequency was approximately twice its mean, indicating significant
overdispersion. The Poisson model had a Pearson dispersion statistic of approximately 2.02

Therefore a Negative Binomial GLM was fitted to account for additional dispersion 

The final model consolidated vehicle manufacturer with fewer than 100 policies into an "Other"
category to reduce instability from sparse manufacturer groups.


Model Validation: 

The final model was evaluated using an 80/20 training and test split.

Out-of-sample performance was assessed using

- Mean Absolute Error (MAE)
- Root Mean Squared Error (RMSE)
- Aggregate observed versus predicted claim frequency 
- Prediction-decile calibration 


##Results

Frequency Model:
  Metric                            Result 
- Poisson AIC                       80,713.20
- Final Negative Binomial AIC       70,835.33
- Final Pearson dispersion          0.878
- Test observed mean frequency      0.521
- Test predicted mean frequency     0.508
- Test RMSE                         1.030
- Test MAE                          0.744

The Negative Binomial model substantially reduced AIC relative to the Poisson model 
and reduced the Pearson dispersion statistic to below 1, indicating that substantial 
residual overdispersion was no longer present. 


Risk Relativities: 

Using Ford as the reference manufacturer, the final model produced the following 
manufacturer relativities

Manufacturer    Relativity vs. Ford     95% Confidence Interval
Geo             1.61                    1.07-2.42
Lexus           0.818                   0.678-0.988
Saab            0.727                   0.554-0.954

These estimated represent associations with the analyzed dataset after contrlling for 
other variables included in the model. They should not be interpreted as causal effects
or direclty applicable insurance pricing factors


Severity Data Validation:

The available claim_amt variable was assessed as a potential severity measure.

Policies with no reported claims had positive claim amoounts, and average claim amounts
were nearly identical between policies with and without reported claims.

No reported claim: approx. 49,992
At least on reported claim: approx. 50,125

The correlation between claim frequency and claim amount was approx. 0.002.

The characteristic indicate that claim_amt does not appear to represent conventional claim
severity. Therefore a gamma severity model was not applied:


## Limitations
- The dataset does not contain a conventional exposure variable such as policy-years.
Therefore the frequency model treats each observation as an equivalant policy-period 
rather than applying exposure effect.
- The available claim amount variable does not appear suitable for conventional severity
modeling.
- Vehicle manufacturer is represented by many categories, requiring consolidation of 
sparse groups.
- The dataset is observational, so model coefficients should be interpreted as associations
rather than causal effects.
- Test-set performance depends on the selected random train/test split/

## Tools

- R
- RStudio 
- tidyverse
- ggplot2
- MASS
- broom
- scales
- patchwork
- readxl
- lubridate
- Git
- GitHub 


## Project Structure
```text
Insurance-Claims-Analysis/
├── data/
│   └── Car Insurance Policies.xlsx
├── R/
│   ├── 01_data_cleaning.R
│   ├── 02_eda.R
│   ├── 03_frequency_model.R
│   └── 04_severity_analysis.R
├── figures/
│   ├── average_claim_frequency_by_age_group.png
│   ├── average_claim_frequency_by_vehicle_age.png
│   └── negative_binomial_calibration.png
├── output/
│   ├── claims_clean.rds
│   ├── frequency_model_results.csv
│   ├── frequency_model_relativities.csv
│   ├── severity_validation.csv
│   └── severity_correlation.csv
├── .gitignore
├── Insurance-Claims-Analysis.Rproj
├── LICENSE
└── README.md

## Dataset

This project uses the Car Insurance Policies dataset by Nidhi Yadav from Kaggle.

The dataset is distributed under the Apache License, Version 2.0 (Apache-2.0).

Source: https://www.kaggle.com/datasets/nidhiy07/car-insurance-policies
License: https://www.apache.org/licenses/LICENSE-2.0

## Reproducibility

The analysis uses a fixed random seed for the train/test split so that the model 
validation results can be reproduced. 

All analysis is organized into sequential R scripts covering data cleaning,
exploratroy analysis, frequency modeling, and severity-data validation.
