# ============================================================
# Insurance Claims Frequency & Severity Analysis
# Author: Hannah Dao
# Purpose: Data cleaning and preparation
# ============================================================

# load packages

library(tidyverse)
library(MASS)
library(broom)
library(scales)
library(patchwork)
library(readxl)
library(lubridate)


# ============================================================
# 1. import data
# ============================================================

# import raw insurance policy data from excel file (from kaggle)
claims <- read_excel("data/Car Insurance Policies .xlsx")

# ============================================================
# 2. data type conversion
# ============================================================

# convert birthdate from character format to date format

claims <- claims %>%
  mutate(
    birthdate = as.Date(birthdate, format = "%m/%d/%Y")
  )

# ============================================================
# 3. categorical value preparation 
# ============================================================

# standardize spelling of martial status categories
# convert categorical predictors to factors for statistical modeling
claims <- claims %>%
  mutate(
    marital_status = recode(
      marital_status,
      "Seperated" = "Separated"
    ),
    marital_status = factor(marital_status),
    car_use = factor(car_use),
    gender = factor(gender),
    parent = factor(parent),
    education = factor(education),
    coverage_zone = factor(coverage_zone)
  )

# ============================================================
# 4. numeric variable validation 
# ============================================================

# review summary statistics for numerical variables to 
# identify potential outliers, invalid values, or 
# data-entry issues.

summary(claims %>%
          dplyr::select(
            kids_driving,
            car_year,
            claim_freq,
            claim_amt,
            household_income
          ))

# ============================================================
# 5. vehicle year validation 
# ============================================================

# examine distribution of vehicle model years to identify 
# any unusually old vehicles that may require further 
# investigation 

claims %>% 
  filter(car_year < 1950) %>%
  dplyr::select(ID, car_year, car_make, car_model)

# developer note: 1909 seems suspicious but it corresponds
# with a legit  car model, and there is more than one listed 
# in the dataset, therefore I will leave the observation
# within the dataset. 

# ============================================================
# 6. character data standardization
# ============================================================

# correct a character-encoding issues in the vehicle manufacturer
# name to ensure consistent labeling 

claims <- claims %>%
  mutate(
    car_make = recode(
      car_make,
      "CitroÃ«n" = "Citroën"
    )
  )

# ============================================================
# 7. claim frequency and amount validation 
# ============================================================

# compare claim amounts for policies with and without reported 
# claims to determine whether claim_amt represents severity only
# or contains values for policies with zero claim frequency 

claims %>%
  group_by(claim_freq) %>%
  summarise(
    policies = n(),
    mean_claim_amt = mean(claim_amt),
    median_claim_amt = median(claim_amt),
    min_claim_amt = min(claim_amt),
    max_claim_amt = max(claim_amt)
  )

# calculate proportion of policies with at least one claim

claims %>%
  summarise(
    total_policies = n(),
    policies_with_claims = sum(claim_freq > 0),
    claim_probability = mean(claim_freq > 0)
  )

# ============================================================
# 8. derived variables 
# ============================================================

# calculate policy holder age using the current year. 
# this provides an interpretable demographic risk factor for 
# subsequent exploratory analysis and frequent modeling.

claims <- claims %>%
  mutate(
    age = as.numeric(
      difftime(Sys.Date(), birthdate, units = "days")
    ) / 365.25
  )

# round policy holder age to the nearest whole year for easier
# intrepretation

claims <- claims %>%
  mutate(
    age = floor(age)
  )

# calculate vehicle age using the current year.
# this may capture differences in vehicle characteristics 
# associated with insurance claim frequency. 

claims <- claims  %>%
  mutate(
    vehicle_age = as.numeric(format(Sys.Date(), "%Y")) - car_year
  )

# ============================================================
# 9. data quality check
# ============================================================

# check structure of the cleaned dataset to confirm data types
str(claims)

# recheck for missing values in all variables
colSums(is.na(claims))

# check for duplicate policy records using ID 
sum(duplicated(claims$ID))

# verify claims are non-negative whole numbers
sort(unique(claims$claim_freq))

# verify ages fall within reasonable range
summary(claims$age)

# verify range of vehicle ages. Noting historical vehicles identified
# during data validation 
summary(claims$vehicle_age)

# ============================================================
# 10. duplicate ID investigation 
# ============================================================

# identify IDs that appear more than once 

claims %>% 
  filter(duplicated(ID) | duplicated(ID, fromLast = TRUE)) %>%
  arrange(ID)

# compare complete records to determine if it is a duplicate 
# entry of two distinct entries

claims %>%
  filter(ID == "56-5402470") %>%
  print(width = Inf)

# ============================================================
# 11. duplicate ID resolution
# ============================================================

# remove the two observations associated with the duplicated ID.
# the records contain different information despite having the 
# ID. since it is not possible to determine which observation 
# represents the valid record, both observations will be omitted 
# from the dataset 

claims <- claims %>% 
  filter(ID != "56-5402470")

# confirm all IDs are unique

sum(duplicated(claims$ID))

# ============================================================
# 12. save cleaned data set
# ============================================================

# save cleaned data set
saveRDS(
  claims,
  "output/claims_clean.rds"
)
