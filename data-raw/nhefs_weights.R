## code to prepare `nhefs_weights` dataset goes here
## code to prepare `nhefs_weights` dataset goes here
library(tidyverse)
library(broom)
library(causaldata)
propensity_model <- glm(
  qsmk ~ sex +
    race + age + I(age^2) + education +
    smokeintensity + I(smokeintensity^2) +
    smokeyrs + I(smokeyrs^2) + exercise + active +
    wt71 + I(wt71^2),
  family = binomial(),
  data = nhefs_complete
)

nhefs_weights <- propensity_model %>%
  augment(type.predict = "response", data = nhefs_complete) %>%
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
    w_ato = (1 - .fitted) * qsmk +
      .fitted * (1 - qsmk)
  ) %>%
  select(
    qsmk,
    race,
    age,
    education,
    smokeintensity,
    smokeyrs,
    exercise,
    active,
    wt71,
    starts_with("w_"),
    .fitted
  ) %>%
  mutate(qsmk = factor(qsmk))

usethis::use_data(nhefs_weights, overwrite = TRUE)
