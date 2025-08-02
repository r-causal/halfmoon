## code to prepare `nhefs_weights` dataset goes here
library(tidyverse)
library(broom)
library(causaldata)
library(nnet)
library(propensity)

# Binary exposure propensity model (qsmk)
propensity_model <- glm(
  qsmk ~
    sex +
      race +
      age +
      I(age^2) +
      education +
      smokeintensity +
      I(smokeintensity^2) +
      smokeyrs +
      I(smokeyrs^2) +
      exercise +
      active +
      wt71 +
      I(wt71^2),
  family = binomial(),
  data = nhefs_complete
)

# Categorical exposure propensity model (alcoholfreq)
# First convert alcoholfreq to factor with meaningful labels
nhefs_complete_cat <- nhefs_complete %>%
  mutate(
    alcoholfreq_cat = factor(
      alcoholfreq,
      levels = 0:5,
      labels = c("none", "monthly", "weekly", "daily", "2plus_daily", "unknown")
    )
  )

# Drop unknown category if it has very few observations
if (sum(nhefs_complete_cat$alcoholfreq_cat == "unknown") < 10) {
  nhefs_complete_cat <- nhefs_complete_cat %>%
    filter(alcoholfreq_cat != "unknown") %>%
    mutate(alcoholfreq_cat = droplevels(alcoholfreq_cat))
}

# Multinomial propensity model for alcohol frequency
# Using similar covariates as the smoking model
propensity_model_cat <- multinom(
  alcoholfreq_cat ~
    sex +
      race +
      age +
      I(age^2) +
      education +
      smokeintensity +
      I(smokeintensity^2) +
      smokeyrs +
      I(smokeyrs^2) +
      exercise +
      active +
      wt71 +
      I(wt71^2),
  data = nhefs_complete_cat,
  trace = FALSE  # Suppress iteration output
)

# Get propensity scores for each category as a matrix
ps_cat <- fitted(propensity_model_cat)

# Get indices of kept rows (excluding unknown category)
kept_indices <- which(nhefs_complete$alcoholfreq != 5)

# Combine binary and categorical exposure data
nhefs_weights <- propensity_model %>%
  augment(type.predict = "response", data = nhefs_complete) %>%
  mutate(
    # Binary exposure weights
    wts = 1 / ifelse(qsmk == 0, 1 - .fitted, .fitted),
    w_ate = (qsmk / .fitted) +
      ((1 - qsmk) / (1 - .fitted)),
    w_att = ((.fitted * qsmk) / .fitted) +
      ((.fitted * (1 - qsmk)) / (1 - .fitted)),
    w_atc = (((1 - .fitted) * qsmk) / .fitted) +
      (((1 - .fitted) * (1 - qsmk)) / (1 - .fitted)),
    w_atm = pmin(.fitted, 1 - .fitted) /
      (qsmk * .fitted + (1 - qsmk) * (1 - .fitted)),
    w_ato = (1 - .fitted) * qsmk + .fitted * (1 - qsmk)
  ) %>%
  # Filter to match the categorical data (remove unknown alcohol freq)
  slice(kept_indices) %>%
  # Add categorical exposure
  mutate(
    alcoholfreq = nhefs_complete_cat$alcoholfreq,
    alcoholfreq_cat = nhefs_complete_cat$alcoholfreq_cat,
    # Calculate categorical exposure weights directly
    w_cat_ate = wt_ate(ps_cat, nhefs_complete_cat$alcoholfreq_cat),
    w_cat_att_none = wt_att(ps_cat, nhefs_complete_cat$alcoholfreq_cat, focal = "none"),
    w_cat_att_monthly = wt_att(ps_cat, nhefs_complete_cat$alcoholfreq_cat, focal = "monthly"),
    w_cat_att_weekly = wt_att(ps_cat, nhefs_complete_cat$alcoholfreq_cat, focal = "weekly"),
    w_cat_att_daily = wt_att(ps_cat, nhefs_complete_cat$alcoholfreq_cat, focal = "daily"),
    w_cat_att_2plus = wt_att(ps_cat, nhefs_complete_cat$alcoholfreq_cat, focal = "2plus_daily"),
    w_cat_atu_none = wt_atu(ps_cat, nhefs_complete_cat$alcoholfreq_cat, focal = "none"),
    w_cat_ato = wt_ato(ps_cat, nhefs_complete_cat$alcoholfreq_cat),
    w_cat_atm = wt_atm(ps_cat, nhefs_complete_cat$alcoholfreq_cat)
  ) %>%
  select(
    # Binary exposure
    qsmk,
    # Categorical exposure
    alcoholfreq,
    alcoholfreq_cat,
    # Covariates
    race,
    age,
    sex,
    education,
    smokeintensity,
    smokeyrs,
    exercise,
    active,
    wt71,
    # Outcomes
    wt82_71,
    death,
    # Binary exposure weights
    wts,
    starts_with("w_a"),
    # Categorical exposure weights
    starts_with("w_cat_"),
    # Propensity score for binary exposure
    .fitted
  ) %>%
  mutate(qsmk = factor(qsmk))

usethis::use_data(nhefs_weights, overwrite = TRUE)