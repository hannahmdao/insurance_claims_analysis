# ============================================================
# Insurance Claims Frequency & Severity Analysis
# Author: Hannah Dao
# Purpose: Exploratory data analysis
# ============================================================

# load packages
library(tidyverse)
library(scales)

# ============================================================
# 1.claim frequency distribution
# ============================================================

# examine number of claims reported per policy.
# this provides an initial view of the distribution of the 
# frequency response variable used in subsequent models

# create a bar chart showing the distribution of claim frequency
# this visualization will highlight the concentration of policies
# with zero claims and the relatively small number of policies with 
# multiple claims

claims %>%
  count(claim_freq) %>%
  mutate(
    proportion = n / sum(n)
  ) %>%
  ggplot(aes(x = factor(claim_freq), y = n)) +
  geom_col() +
  geom_text(
    aes(label = percent(proportion, accuracy = 0.1)),
    vjust = -0.5
  ) +
  labs(
    title = "Distribution of Claim Frequency",
    x = "Number of Claims",
    y = "Number of Policies"
  ) +
  theme_minimal()

## save plot 
ggsave(
  "figures/claim_frequency_distribution.png",
  width = 8,
  height = 6,
  dpi = 300
)

# ============================================================
# 2.claim frequency by car use 
# ============================================================

# calculate the average number of claims per policy for each 
# type of vehicle use 

# create a bar chart comparing average claim frequency by car 
# use. This provides a visual comparison of claim frequency 
# between private and commercial use.

claims %>%
  group_by(car_use) %>%
  summarise(
    policies = n(),
    mean_claim_freq = mean(claim_freq),
    claim_probability = mean(claim_freq > 0)
  ) %>%
  ggplot(aes(x = car_use, y = mean_claim_freq)) +
  geom_col() +
  geom_text(
    aes(label = round(mean_claim_freq, 3)),
    vjust = -0.5
  ) +
  labs(
    title = "Average Claim Frequency by Car Use",
    x = "Car Use",
    y = "Average Number of Claims per Policy"
  ) +
  theme_minimal()

# ============================================================
# 3.claim frequency by coverage zone
# ============================================================

# calculate the average number of claims per policy for each 
# coverage zone to identify potential geographic differences

claims %>% 
  group_by(coverage_zone) %>% 
  summarise(
    policies = n(),
    mean_claim_freq = mean(claim_freq)
  )

# ============================================================
# 4.claim frequency by policy holder age
# ============================================================

# calculate the average number of claims per policy for each 
# policyholder age to examine whether claim frequency varies 
# across different age groups.

claims %>% 
  group_by(age) %>%
  summarise(
    policies = n(),
    mean_claim_freq = mean(claim_freq)
  ) 

# create age groups in 10-year intervals to reduce random variation
# between individual ages and provide a clearer view of the 
# relationship between age and claim frequency. (individual ages looked 
# looked too noisy and age 79 has way fewer observations than the rest
# of the age groups, skewing the graph.)

# Create a bar chart comparing average claim frequency across
# policyholder age groups.

claims %>%
  mutate(
    age_group = cut(
      age, 
      breaks = c(20, 30, 40, 50, 60, 70, 80),
      right = FALSE,
      labels = c("20-29", "30-39", "40-49", "50-59", "60-69", "70-79")
    )
  ) %>%
  group_by(age_group) %>%
  summarise(
    policies = n(),
    mean_claim_freq = mean(claim_freq)
  )  %>%
  ggplot(aes(x = age_group, y = mean_claim_freq)) +
  geom_col() +
  geom_text(
    aes(label = round(mean_claim_freq, 3)),
    vjust = -0.5
  ) +
  labs(
    title = "Average Claim Frequency by Age Group",
    x = "Policyholder Age Group",
    y = "Average Number of Claims per Policy"
  ) +
  theme_minimal()

## save plot 
ggsave(
  "figures/average_claim_frequency_by_age_group.png",
  width = 8,
  height = 6,
  dpi = 300
)

# ============================================================
# 5. claim frequency by gender 
# ============================================================

# Calculate the average number of claims per policy for each
# gender to examine whether claim frequency differs between
# the two groups in the dataset.

claims %>%
  group_by(gender) %>%
  summarise(
    policies = n(),
    mean_Claim_freq = mean(claim_freq),
    claim_probability = mean(claim_freq > 0)
  )

# ============================================================
# 6. claim frequency by marital status
# ============================================================

# Calculate the average number of claims per policy for each
# marital status to examine whether claim frequency varies
# across policyholder groups.

claims %>%
  group_by(marital_status) %>%
  summarise(
    policies= n(),
    mean_claim_freq = mean(claim_freq),
    claim_probability = mean(claim_freq > 0)
  )

# ============================================================
# 7. claim frequency by vehicle age
# ============================================================

# Group vehicles into age bands to examine whether claim
# frequency varies across different vehicle age ranges.

# Create a bar chart comparing average claim frequency across
# vehicle age groups.

claims %>% 
  mutate(
    vehicle_age_group = cut(
      vehicle_age, 
      breaks = c(0, 10, 20, 30, 40, 50, Inf),
      right = FALSE,
      labels = c(
        "0-9",
        "10-19",
        "20-29",
        "30-39",
        "40-49",
        "50+"
      )
    )
  ) %>%
  group_by(vehicle_age_group) %>%
  summarise(
    policies = n(),
    mean_claim_freq = mean(claim_freq),
    claim_probability = mean(claim_freq > 0)
  )%>%
  ggplot(aes(x = vehicle_age_group, y = mean_claim_freq)) +
  geom_col() +
  geom_text(
    aes(label = round(mean_claim_freq, 3)),
    vjust = -0.5
  ) +
  labs(
    title = "Average Claim Frequency by Vehicle Age",
    x = "Vehicle Age (Years)",
    y = "Average Number of Claims per Policy"
  ) +
  theme_minimal()

## save plot 
ggsave(
  "figures/average_claim_frequency_by_vehicle_age.png",
  width = 8,
  height = 6,
  dpi = 300
)

# ============================================================
# 8. claim frequency by education 
# ============================================================

# Calculate the average number of claims per policy for each
# education level to examine whether claim frequency varies
# across policyholder groups.

claims %>%
  group_by(education) %>%
  summarise(
    policies = n(),
    mean_claim_freq = mean(claim_freq),
    claim_probability = mean(claim_freq > 0)
  )

# ============================================================
# 9. claim frequency by number of kids driving 
# ============================================================

# Calculate the average number of claims per policy based on
# the number of children who are driving in the household.

claims %>%
  group_by(kids_driving) %>%
  summarise(
    policies = n(),
    mean_claim_freq = mean(claim_freq),
    claim_probability = mean(claim_freq > 0)
  )
# ============================================================
# 10. claim frequency by parent status 
# ============================================================

# Calculate the average number of claims per policy for
# policyholders with and without children.

claims %>%
  group_by(parent) %>%
  summarise(
    policies = n(),
    mean_claim_freq = mean(claim_freq),
    claim_probability = mean(claim_freq > 0)
  )

# ============================================================
# 11. claim frequency by parent status 
# ============================================================

# Calculate claim frequency by vehicle manufacturer.
# Manufacturers with very small policy counts are excluded
# from the comparison because their observed claim frequencies
# may be highly unstable.

manufacturer_frequency <- claims %>%
  group_by(car_make) %>%
  summarise(
    policies = n(),
    mean_claim_freq = mean(claim_freq),
    claim_probability = mean(claim_freq > 0)
  ) %>%
  filter(policies >= 100)

# Identify the 10 manufacturers with the lowest observed claim
# frequencies among those meeting the minimum policy threshold.

lowest_manufacturers <- manufacturer_frequency %>%
  arrange(mean_claim_freq) %>%
  slice_head(n = 10)

# Identify the 10 manufacturers with the highest observed claim
# frequencies among those meeting the minimum policy threshold.

highest_manufacturers <- manufacturer_frequency %>%
  arrange(desc(mean_claim_freq)) %>%
  slice_head(n = 10)

# Combine the highest and lowest frequency manufacturers for
# visualization.

manufacturer_plot <- bind_rows(
  lowest_manufacturers,
  highest_manufacturers
) %>%
  mutate(
    car_make = reorder(car_make, mean_claim_freq)
  )

# Create a bar chart comparing observed claim frequency for the
# selected manufacturers.

ggplot(
  manufacturer_plot,
  aes(x = car_make, y = mean_claim_freq)
) +
  geom_col() +
  geom_text(
    aes(label = round(mean_claim_freq, 3)),
    vjust = -0.5
  ) +
  labs(
    title = "Claim Frequency by Car Manufacturer",
    subtitle = "Highest and lowest observed frequencies among manufacturers with at least 100 policies",
    x = "Car Manufacturer",
    y = "Average Number of Claims per Policy"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

## save plot 
ggsave(
  "figures/claim_frequency_by_car_manufacturer.png",
  width = 8,
  height = 6,
  dpi = 300
)

# ============================================================
# 12. claim amount distribution 
# ============================================================

# Examine the distribution of claim amounts to understand its
# range, central tendency, and variability before determining
# whether it can be used as a claim severity measure.

summary(claims$claim_amt)

# Calculate key statistics for the claim amount variable.
claims %>%
  summarise(
    policies = n(),
    mean_claim_amt = mean(claim_amt),
    median_claim_amt = median(claim_amt),
    sd_claim_amt = sd(claim_amt),
    minimum_claim_amt = min(claim_amt),
    maximum_claim_amt = max(claim_amt)
  )

# Create a histogram showing the distribution of claim amounts.

ggplot(claims, aes(x = claim_amt)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Distribution of Claim Amount",
    x = "Claim Amount",
    y = "Number of Policies"
  ) +
  theme_minimal()
# Compare claim amounts across different claim frequency levels.
# This helps determine whether claim_amt is related to the
# number of claims reported by a policyholder.

claims %>%
  group_by(claim_freq) %>%
  summarise(
    policies = n(),
    mean_claim_amt = mean(claim_amt),
    median_claim_amt = median(claim_amt)
  )

# examine the linear association between claim frequency and claim amount

cor(
  claims$claim_freq,
  claims$claim_amt
)

# examine percentile distributions

quantile(
  claims$claim_amt,
  probs = c(0, 0.01, 0.05, 0.25, 0.50, 0.75, 0.95, 0.99, 1)
)

# the claim amount variable does not appear to behave conventionally. claim 
# amounts are positive even with zero reported claims, and claim amount has 
# almost no correlation with claim frequency. Therefore claim_amnt is excluded
# from the severity model component of this analysis 

# ============================================================
# 13. claim frequency by household income 
# ============================================================

# Divide household income into approximately equal-sized groups
# to examine whether claim frequency varies across income levels.

claims %>%
  mutate(
    income_group = ntile(household_income, 4)
  ) %>%
  group_by(income_group) %>%
  summarise(
    policies = n(),
    mean_income = mean(household_income),
    mean_claim_freq = mean(claim_freq),
    claim_probability = mean(claim_freq > 0)
  )

# ============================================================
# 14. baseline claim frequency 
# ============================================================

# Calculate the overall average number of claims per policy.
# This provides a baseline against which the frequency models
# can be compared.

claims %>%
  summarise(
    policies = n(),
    total_claims = sum(claim_freq),
    mean_claim_freq = mean(claim_freq)
  )

# ============================================================
# 15. claim frequency distribution 
# ============================================================

# compare the variance of claim frequency with its mean (for a 
# poisson distribution)

claims %>%
  summarise(
    mean_claim_freq = mean(claim_freq),
    variance_claim_freq = var(claim_freq),
    dispersion_ratio = var(claim_freq) / mean(claim_freq)
  )

# the dispersion ratio is substantially greater than 1 (2.02), 
# indicating overdispersion in the claim frequency response.
# Therefore a Negative Binomial model may be more appropriate than
# a standard Poisson model 
