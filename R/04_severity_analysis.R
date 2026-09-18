# ============================================================
# Insurance Claims Frequency & Severity Analysis
# Author: Hannah Dao
# Purpose: claim severity data validation
# ============================================================

# Load packages.

library(tidyverse)

# Import finalized cleaned dataset.
claims <- readRDS("output/claims_clean.rds")

# ============================================================
# 1. Severity Variable Validation 
# ============================================================

# Examine claim amounts for policies with and without reported claims. 

severity_validation <- claims %>%
  mutate(
    has_claim = claim_freq > 0
  ) %>%
  group_by(has_claim) %>%
  summarise(
    policies = n(),
    mean_claim_amt = mean(claim_amt),
    median_claim_amt = median(claim_amt),
    sd_claim_amt = sd(claim_amt),
    .groups = "drop"
  )

severity_validation

# Calculate the correlation between claim frequency and claim amount. 

severity_correlation <- cor(
  claims$claim_freq, 
  claims$claim_amt
)

severity_correlation

# ============================================================
# 2. Severity Analysis Conclusion 
# ============================================================

# The claim amount variable does not seem to represent conventional 
# insurance claim severity. policies with no reported claims have 
# positive claim amounts, and the average claim amount is very close 
# for policies with and without claims. the correlation between claim
# frequency and amount is nearly 0.

# Therefore, claim_amt is not used as the response variable.

# This limitation is documented rather than addressed by imposing an
# inapropriate Gamma severity model on available data.


# ============================================================
# 2. Save Severity Validation Results
# ============================================================

# Save severity validation results for use in documentation and final 
# analysis summary. 

write_csv(
  severity_validation,
  "output/severity_validation.csv"
)

write_csv(
  tibble(
    metric = "Correlation between claim frequency and claim amount",
    value = severity_correlation
  ),
  "output/severity_correlation.csv"
)
