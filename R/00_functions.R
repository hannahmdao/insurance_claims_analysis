# ============================================================
# Insurance Claims Frequency & Severity Analysis
# Author: Hannah Dao
# Purpose: Function Creation 
# ============================================================

# Load packages.
library(dplyr)

# Calculate the mean claim frequency for a given policy characteristic.

summarise_freq <- function(data, group_var, summary_var) {
  data %>%
    group_by({{ group_var }}) %>%
    summarise(
      policies = n(),
      mean_claim_freq = mean({{ summary_var }}),
    )
} 
  

# Calculate the mean claim frequency AND claim probability for a given policy characteristic. 
summarise_claim <- function(data, group_var, summary_var) {
  data %>%
    group_by({{ group_var }}) %>%
    summarise(
      policies = n(),
      mean_claim_freq = mean({{ summary_var }}),
      claim_probability = mean({{summary_var}} >0)
    )
} 
