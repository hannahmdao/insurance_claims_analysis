# ============================================================
# Insurance Claims Frequency & Severity Analysis
# Author: Hannah Dao
# Purpose: claim frequency modeling 
# ============================================================

# load packages

library(tidyverse)
library(MASS)
library(broom)

# Import the cleaned dataset.
claims <- readRDS("output/claims_clean.rds")

# set a random seed so that the train/test split can be reproduced
set.seed(127)

# randomly assign 80% of policies to the training set and 20% to the
# test set

train_index <- sample(
  seq_len(nrow(claims)),
  size = floor(0.8 * nrow(claims))
)

train_data <- claims[train_index, ]

test_data <- claims[-train_index, ]

# confirm number of observation in each dataset
nrow(train_data)
nrow(test_data)

# ============================================================
# 1. baseline Poisson Model 
# ============================================================

# fit a Poisson generalized linear model to claim frequency.

# each observation represents one policy in the dataset.
# policy exposure duration is not provided, therefore no exposure   
# offset is included. 

# this model examines the relationship between claim frequency
# and selected policyholder, vehicle, and policy characteristics.

poisson_model <- glm(
  claim_freq ~
    age +
    gender +
    marital_status +
    car_use +
    kids_driving +
    parent +
    education +
    vehicle_age +
    coverage_zone +
    car_make,
  family = poisson(link = "log"),
  data = claims
)

# Display the model summary.
summary(poisson_model)

# ============================================================
# 2. poisson model dispersion 
# ============================================================

# calculate the pearson dispersion statistic for the poisson model

pearson_dispersion <- sum(
  residuals(poisson_model, type = "pearson")^2
) / df.residual(poisson_model)

pearson_dispersion

# ============================================================
# 3. negative binomial model 
# 3. negative binomial model 
# ============================================================

# fit a negative binomial generalized linear model to account for
# overdispersion that is in the poisson frequency model

negative_binomial_model <- glm.nb(
  claim_freq ~
    age +
    gender +
    marital_status +
    car_use +
    kids_driving +
    parent +
    education +
    vehicle_age +
    coverage_zone +
    car_make,
  data = claims
)

# Display the model summary.
summary(negative_binomial_model)

# ============================================================
# 4. model comparison 
# ============================================================

# Compare the Akaike Information Criterion (AIC) of the Poisson
# and Negative Binomial models.

AIC(
  poisson_model,
  negative_binomial_model
)

# calculate the pearson dispersion statistic for the Negative 
# Binomial model to assess whether significant overdispersion
# remains. 

nb_pearson_dispersion <- sum(
  residuals(
    negative_binomial_model,
    type = "pearson"
  )^2
) / df.residual(negative_binomial_model)

nb_pearson_dispersion

# the claim frequency data show significant overdispersion relative 
# to the Poisson assumption. Therefore a Negative Binomial GLM was fitted 
# to account for additional variability. The Negative Binomial substantially
# improved the model fit, reducing the AIC from 80,713.20 for the Poisson 
# model to 70,868.31. The Pearson dispersion statistic is 0.876,
# indicating that substantial residual overdispersion is no longer 
# present 

# ============================================================
# 5. negative binomial model diagnostics 
# ============================================================

# generate fitted claim frequencies from the Negative Binomial model 
# to represent the expected number of claims per policy based on
# characteristics in the model 

claims <- claims %>%
  mutate(
    predicted_freq = predict(
      negative_binomial_model,
      type = "response"
    )
  )

# Compare the observed and predicted average claim frequency.
# This provides a basic check of whether the model reproduces the
# overall level of claim frequency in the dataset.

claims %>% 
  summarise(
    observed_mean = mean(claim_freq),
    predicted_mean = mean(predicted_freq)
  )

# Compare observed and predicted claim frequency by the number
# of claims reported by each policy.

frequency_comparison <- claims %>%
  group_by(claim_freq) %>%
  summarise(
    policies = n(),
    predicted_mean = mean(predicted_freq),
    .groups = "drop"
  )

frequency_comparison

# ============================================================
# 6. frequency model calibration 
# ============================================================

# divide policies into ten groups based on their predicted claim 
# frequency to compare across predicted levels of risk.

claims <- claims %>%
  mutate(
    prediction_decile = ntile(predicted_freq, 10)
  )

# Compare average observed and predicted claim frequency within
# each prediction decile.

calibration_table <- claims %>%
  group_by(prediction_decile) %>%
  summarise(
    policies = n(),
    observed_freq = mean(claim_freq),
    predicted_freq = mean(predicted_freq),
    .groups = "drop"
  )

calibration_table

# Create a calibration plot comparing observed and predicted
# claim frequency across prediction deciles.

ggplot(
  calibration_table,
  aes(x = prediction_decile)
) +
  geom_line(
    aes(y = observed_freq, linetype = "Observed")
  ) +
  geom_point(
    aes(y = observed_freq)
  ) +
  geom_line(
    aes(y = predicted_freq, linetype = "Predicted")
  ) +
  geom_point(
    aes(y = predicted_freq)
  ) +
  scale_linetype_manual(
    values = c("Observed" = "solid", "Predicted" = "dashed")
  ) +
  scale_x_continuous(
    breaks = 1:10
  ) +
  labs(
    title = "Observed vs. Predicted Claim Frequency",
    subtitle = "Average claim frequency by Negative Binomial prediction decile",
    x = "Prediction Decile",
    y = "Average Claims per Policy",
    linetype = NULL
  ) +
  theme_minimal()

## save plot 
ggsave(
  "figures/negative_binomial_calibration.png",
  width = 8,
  height = 6,
  dpi = 300
)

# the calibration results show that observed claim frequency generally increases
# across prediction deciles, consistent with the model's 
# predicted risk ranking. observed and predicted frequencies are also reasonably 
# close, indicating reasonable in sample calibration. 
 
# ============================================================
# 7. out-of-sample validation 
# ============================================================

# refit negative binomial model using only training data. 

nb_train <- glm.nb(
  claim_freq~
    age+
    gender+
    marital_status + 
    car_use + 
    kids_driving + 
    parent + 
    education + 
    vehicle_age+ 
    coverage_zone+
    car_make,
  data = train_data
)

# generate predicted claim frequencies for test set 

test_data <- test_data %>% 
  mutate(
    predicted_freq = predict(
      nb_train,
      newdata = test_data,
      type = "response"
    )
  )

# compare average observed and predicted data claim frequency in the 
# test dataset

test_data %>%
  summarise(
    observed_mean = mean(claim_freq),
    predicted_mean = mean(claim_freq)
  )

# compare observed and predicted claim frequency across prediction deciles in 
# the test dataset. 

test_calibration <- test_data %>%
  mutate(
    prediction_decile = ntile(predicted_freq, 10)
  ) %>%
  group_by(prediction_decile) %>%
  summarise(
    policies = n(),
    observed_freq = mean(claim_freq),
    predicted_freq = mean(predicted_freq),
    .groups = "drop"
  )

test_calibration


# the negative binomial model maintained increasing predicted claim frequency
# across test-set risk deciles. However, observed claim frequences were 
# substantially more variable across deciles, indicating an out-of-sample 
# calibration is less stable than the in-calibration.

# calculate predictikon error metric on the test dataset. 
# measure typical magnitude of prediction error 
# measure average absolute difference between observed and predicted claim 
# frequency with MAE

test_performance <- test_data %>%
  summarise(
    rmse = sqrt(mean((claim_freq - predicted_freq)^2)),
    mae = mean(abs(claim_freq - predicted_freq))
  )

test_performance

# ============================================================
# 8. negative binomial risk relativities 
# ============================================================

# extract and convert negative binomial coefficients from log scale to 
# multiplicative risk relativities

nb_coefficients <- tidy(
  negative_binomial_model
)

# Select and round the key columns for easier interpretation.

nb_relativities <- nb_coefficients %>%
  mutate(
    relativity = exp(estimate),
    conf_low = exp(estimate - 1.96 * std.error),
    conf_high = exp(estimate + 1.96 * std.error)
  ) %>%
  dplyr::select(
    term,
    relativity,
    conf_low,
    conf_high,
    p.value
  ) %>%
  mutate(
    across(
      c(relativity, conf_low, conf_high),
      ~ round(.x, 3)
    )
  )

# identify model terms with the largest and smallest estimated claim 
# frequency relativities 

highest_relativities <- nb_relativities %>%
  filter(term != "(Intercept)") %>%
  arrange(desc(relativity)) %>%
  slice_head(n = 10)

lowest_relativities <- nb_relativities %>%
  filter(term != "(Intercept)") %>%
  arrange(relativity) %>%
  slice_head(n = 10)

# examine policy counts by car manufacturer

claims %>%
  count(car_make, sort = TRUE) %>%
  slice_tail(n = 20)

# ============================================================
# 9. manufacturer category consolidation  
# ============================================================

# identify and combine manufacturers with fewer than 100 policies into 
# an "Other" category to reduce instability caused by small manufacturer
# groups.

manufacturer_counts <- claims %>%
  count(car_make)

claims <- claims %>%
  left_join(
    manufacturer_counts,
    by = "car_make"
  ) %>%
  mutate(
    car_make_grouped = if_else(
      n < 100,
      "Other",
      car_make
    ),
    car_make_grouped = factor(car_make_grouped)
  ) %>%
  dplyr::select(-n)

# review remaining 

claims %>%
  count(car_make_grouped, sort = TRUE) %>%
  slice_tail(n = 20)

# refit Negative Binomial model using consolidated manufacturer variable 
negative_binomial_grouped <- glm.nb(
  claim_freq ~
    age +
    gender +
    marital_status +
    car_use +
    kids_driving +
    parent +
    education +
    vehicle_age +
    coverage_zone +
    car_make_grouped,
  data = claims
)

# compare original and consolidated Negative Binomial models

AIC(
  negative_binomial_model,
  negative_binomial_grouped
)

# ============================================================
# 10. final model train/test validation 
# ============================================================

# recreate train/test split using consolidated manufacturer variable 

set.seed(127)

train_index_grouped <- sample(
  seq_len(nrow(claims)),
  size = floor(0.80 * nrow(claims))
)

train_grouped <- claims[train_index_grouped, ]
test_grouped <- claims[-train_index_grouped, ]

# Set Ford as the reference manufacturer in both datasets.
train_grouped <- train_grouped %>%
  mutate(
    car_make_grouped = relevel(
      car_make_grouped,
      ref = "Ford"
    )
  )

test_grouped <- test_grouped %>%
  mutate(
    car_make_grouped = relevel(
      car_make_grouped,
      ref = "Ford"
    )
  )

# fit final Negative Binomial model using training data.

negative_binomial_final <- glm.nb(
  claim_freq ~
    age +
    gender +
    marital_status +
    car_use +
    kids_driving +
    parent +
    education +
    vehicle_age +
    coverage_zone +
    car_make_grouped,
  data = train_grouped
)

# generate predictions for test data

test_grouped <- test_grouped %>%
  mutate(
    predicted_freq = predict(
      negative_binomial_final,
      newdata = test_grouped,
      type = "response"
    )
  )

# calculate out of sample error

final_test_performance <- test_grouped %>%
  summarise(
    observed_mean = mean(claim_freq),
    predicted_mean = mean(predicted_freq),
    rmse = sqrt(mean((claim_freq - predicted_freq)^2)),
    mae = mean(abs(claim_freq - predicted_freq))
  )

# calculate Pearson dispersion statistic for final Negative Binomial model

final_pearson_dispersion <- sum(
  residuals(
    negative_binomial_final,
    type = "pearson"
  )^2
) / df.residual(negative_binomial_final)

final_pearson_dispersion


# ============================================================
# 11. final model risk relativities 
# ============================================================

# extract and convert coefficients from the final Negative Binomial 
# model and convert from log scale into multiplicative claim frequency
# relativities

# 95% confidence interval 

final_coefficients <- broom::tidy(
  negative_binomial_final
)

final_relativities <- final_coefficients %>%
  mutate(
    relativity = exp(estimate),
    conf_low = exp(estimate - 1.96 * std.error),
    conf_high = exp(estimate + 1.96 * std.error)
  ) %>%
  dplyr::select(
    term,
    relativity,
    conf_low,
    conf_high,
    p.value
  ) %>%
  mutate(
    across(
      c(relativity, conf_low, conf_high),
      ~ round(.x, 3)
    )
  )

final_relativities

# display final model with confidence intervals that exclude a 
# relativity of 1

final_relativities %>%
  filter(
    term != "(Intercept)",
    (conf_low > 1 | conf_high < 1)
  )

# ============================================================
# 12. final frequency model results
# ============================================================

# create summary of the final Negative Binomial model for use in 
# project documentation 

frequency_model_results <- tibble(
  metric = c(
    "Poisson AIC",
    "Final Negative Binomial AIC",
    "Final Pearson dispersion",
    "Test observed mean frequency",
    "Test predicted mean frequency",
    "Test RMSE",
    "Test MAE"
  ),
  value = c(
    AIC(poisson_model),
    AIC(negative_binomial_grouped),
    final_pearson_dispersion,
    final_test_performance$observed_mean,
    final_test_performance$predicted_mean,
    final_test_performance$rmse,
    final_test_performance$mae
  )
)

frequency_model_results

# save frequency model results 
write_csv(
  frequency_model_results,
  "output/frequency_model_results.csv"
)

# save risk relativities 
write_csv(
  final_relativities,
  "output/frequency_model_relativities.csv"
)

# ============================================================
# 13. frequency model conclusions 
# ============================================================

# The claim frequency data show substantial overdispersion relative 
# to the Poisson distribution. The Negative Binomial model provides a 
# substantially better fit and reduces the residual dispersion to a level 
# that is close to 1. 

# Manufacturers with fewer than 100 policies were consolidated into an "Other"
# category to reduce instability from sparse manufacturer groups. The resulting 
# model had a lower AIC and used fewer parameters.

# On the held-out test set, the final model produced predicted claim 
# frequency for Geo and lower claim frequency for Lexus and Saab.
# These results represent associations within the dataset and should not
# be interpreted as causal effects or directly applied as an insurance 
# pricing factor

