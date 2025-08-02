## code to prepare `nhefs_weights` dataset goes here
library(tidyverse)
library(broom)
library(causaldata)
library(nnet)
library(propensity)

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

# Create categorical exposure variable for alcohol frequency
# Based on nhefs_codebook:
# 0: Almost every day, 1: 2-3 times/week, 2: 1-4 times/month,
# 3: < 12 times/year, 4: No alcohol last year, 5: Unknown
# Keep all rows, including unknown (alcoholfreq = 5)
nhefs_with_cat <- nhefs_complete %>%
  mutate(
    alcoholfreq_cat = case_when(
      alcoholfreq == 0 ~ "daily",
      alcoholfreq == 1 ~ "2_3_per_week",
      alcoholfreq == 2 ~ "1_4_per_month",
      alcoholfreq == 3 ~ "lt_12_per_year",
      alcoholfreq == 4 ~ "none",
      alcoholfreq == 5 ~ "unknown"
    ),
    alcoholfreq_cat = factor(
      alcoholfreq_cat,
      levels = c(
        "none",
        "lt_12_per_year",
        "1_4_per_month",
        "2_3_per_week",
        "daily",
        "unknown"
      )
    )
  )

# Create a mapping to preserve original row order
nhefs_with_cat$orig_row <- seq_len(nrow(nhefs_with_cat))

# For multinomial model, we'll exclude unknown category
nhefs_for_model <- nhefs_with_cat %>%
  filter(alcoholfreq != 5) %>%
  mutate(
    # Drop unused level to avoid issues
    alcoholfreq_cat = droplevels(alcoholfreq_cat)
  )

# Fit multinomial propensity score model
cat_ps_model <- multinom(
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
  data = nhefs_for_model,
  trace = FALSE
)

# Get propensity scores as data frame
cat_ps <- as.data.frame(predict(
  cat_ps_model,
  type = "probs",
  newdata = nhefs_for_model
))

# Calculate weights and join back to data
nhefs_with_cat_weights <- nhefs_for_model %>%
  mutate(
    # Convert psw objects to numeric using as.numeric
    w_cat_ate = as.numeric(wt_ate(cat_ps, alcoholfreq_cat)),
    w_cat_att_none = as.numeric(wt_att(
      cat_ps,
      alcoholfreq_cat,
      focal = "none"
    )),
    w_cat_att_lt12 = as.numeric(wt_att(
      cat_ps,
      alcoholfreq_cat,
      focal = "lt_12_per_year"
    )),
    w_cat_att_1_4mo = as.numeric(wt_att(
      cat_ps,
      alcoholfreq_cat,
      focal = "1_4_per_month"
    )),
    w_cat_att_2_3wk = as.numeric(wt_att(
      cat_ps,
      alcoholfreq_cat,
      focal = "2_3_per_week"
    )),
    w_cat_att_daily = as.numeric(wt_att(
      cat_ps,
      alcoholfreq_cat,
      focal = "daily"
    )),
    w_cat_atu_none = as.numeric(wt_atu(
      cat_ps,
      alcoholfreq_cat,
      focal = "none"
    )),
    w_cat_ato = as.numeric(wt_ato(cat_ps, alcoholfreq_cat)),
    w_cat_atm = as.numeric(wt_atm(cat_ps, alcoholfreq_cat))
  )

# Now combine with binary exposure weights, keeping all rows
nhefs_weights <- propensity_model %>%
  augment(type.predict = "response", data = nhefs_with_cat) %>%
  mutate(
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
  # Join categorical weights, they will be NA for unknown alcohol frequency
  left_join(
    nhefs_with_cat_weights %>%
      select(orig_row, starts_with("w_cat_")),
    by = "orig_row"
  ) %>%
  select(
    qsmk,
    alcoholfreq,
    alcoholfreq_cat,
    race,
    age,
    sex,
    education,
    smokeintensity,
    smokeyrs,
    exercise,
    active,
    wt71,
    wt82_71,
    death,
    wts,
    starts_with("w_"),
    .fitted
  ) %>%
  mutate(
    qsmk = factor(qsmk),
    # Set alcoholfreq_cat to NA for unknown observations
    # These 5 observations have alcoholfreq = 5 (unknown) and all categorical
    # weights are NA since we can't calculate propensity scores for unknown values.
    # Setting the exposure to NA prevents issues with empty factor levels when
    # users filter out NA weights.
    alcoholfreq_cat = if_else(
      alcoholfreq == 5,
      NA_character_,
      as.character(alcoholfreq_cat)
    ),
    alcoholfreq_cat = factor(
      alcoholfreq_cat,
      levels = c(
        "none",
        "lt_12_per_year",
        "1_4_per_month",
        "2_3_per_week",
        "daily"
      )
    )
  )

usethis::use_data(nhefs_weights, overwrite = TRUE)
