# ============================================================
# Insurance Claims Frequency & Severity Analysis
# Author: Hannah Dao
# Purpose: Data cleaning and preparation
# ============================================================

# Load packages.

library(tidyverse)
library(MASS)
library(broom)
library(scales)
library(patchwork)
library(readxl)
library(lubridate)


# ============================================================
# 1. Import Data
# ============================================================

# Import raw insurance policy data from excel file (from kaggle).
claims <- read_excel("data/Car Insurance Policies .Ixlsx")

# ============================================================
# 2. Data Type Conversion
# ============================================================

# Convert birthdate from string format to date format.
claims <- claims %>%
  mutate(
    birthdate = as.Date(birthdate, format = "%m/%d/%Y")
  )

# ============================================================
# 3. Categorical Value Preparation 
# ============================================================

# Standardize spelling of marital status categories.
claims <- claims %>%
  mutate(
    marital_status = recode(
      marital_status,
      "Seperated" = "Separated"
    ), # Convert categorical predictors to factors for statistical modeling.
    marital_status = factor(marital_status),
    car_use = factor(car_use),
    gender = factor(gender),
    parent = factor(parent),
    education = factor(education),
    coverage_zone = factor(coverage_zone)
  )

# ============================================================
# 4. Numeric Variable Validation 
# ============================================================

# Review summary statistics for numerical variables to identify potential outliers, 
# invalid values, or data-entry issues.

summary(claims %>%
          dplyr::select(
            kids_driving,
            car_year,
            claim_freq,
            claim_amt,
            household_income
          ))

# ============================================================
# 5. Vehicle Year Validation 
# ============================================================

# Further investigation for unusually old vehicles.

claims %>% 
  filter(car_year < 1950) %>%
  dplyr::select(ID, car_year, car_make, car_model)

# Developer note: 1909 seems suspicious but it corresponds with a real car model, 
# and there is more than one listed in the dataset, therefore, the observations 
# will not be omitted from the dataset. 

# ============================================================
# 6. Character Data Standardization
# ============================================================

# Correct a character-encoding issue in a vehicle manufacturer name. 

claims <- claims %>%
  mutate(
    car_make = recode(
      car_make,
      "CitroÃ«n" = "Citroën"
    )
  )

# ============================================================
# 7. Claim Frequency and Amount Validation 
# ============================================================

# Compare claim amounts for policies with and without reported claims to determine 
# whether claim_amt represents severity only or contains values for policies with 
# zero claim frequency.

claims %>%
  group_by(claim_freq) %>%
  summarise(
    policies = n(),
    mean_claim_amt = mean(claim_amt),
    median_claim_amt = median(claim_amt),
    min_claim_amt = min(claim_amt),
    max_claim_amt = max(claim_amt)
  )

# Calculate proportion of policies with at least one claim.

claims %>%
  summarise(
    total_policies = n(),
    policies_with_claims = sum(claim_freq > 0),
    claim_probability = mean(claim_freq > 0)
  )

# ============================================================
# 8. Derived Variables 
# ============================================================

# Derive select variables to improve interpretability in subsequent analysis.

# Calculate policy holder age using the current year.

claims <- claims %>%
  mutate(
    age = as.numeric(
      difftime(Sys.Date(), birthdate, units = "days")
    ) / 365.25
  )

# Round policy holder age to the nearest whole year.

claims <- claims %>%
  mutate(
    age = floor(age)
  )

# Calculate vehicle age using the current year.

claims <- claims  %>%
  mutate(
    vehicle_age = as.numeric(format(Sys.Date(), "%Y")) - car_year
  )

# ============================================================
# 9. Data Quality Check
# ============================================================

# Check the structure of the cleaned dataset to confirm data types.
str(claims)

# Recheck for missing values in all variables.
colSums(is.na(claims))

# Check for duplicate policy records using ID. 
sum(duplicated(claims$ID))

# Verify claims are non-negative whole numbers.
sort(unique(claims$claim_freq))

# Verify ages fall within reasonable range.
summary(claims$age)

# Verify range of vehicle ages.
summary(claims$vehicle_age)

# ============================================================
# 10. Duplicate ID Investigation 
# ============================================================

# Identify IDs that appear more than once.

claims %>% 
  filter(duplicated(ID) | duplicated(ID, fromLast = TRUE)) %>%
  arrange(ID)

# Compare complete records to determine if it is a duplicate 
# entry of two distinct entries.

claims %>%
  filter(ID == "56-5402470") %>%
  print(width = Inf)

# ============================================================
# 11. Duplicate ID Resolution
# ============================================================

# Remove the two observations associated with the duplicated ID.The two records 
# contain different information despite having the same ID. Since it is not possible 
# to determine which observation represents the valid record, both observations 
# will be omitted from the dataset.

# Remove observations with the same ID. 

claims <- claims %>% 
  filter(ID != "56-5402470")

# Confirm all IDs are unique.

sum(duplicated(claims$ID))

# ============================================================
# 12. Save Cleaned Data Set
# ============================================================

# Save cleaned data set.
saveRDS(
  claims,
  "output/claims_clean.rds"
)
