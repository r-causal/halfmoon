# NHEFS with various propensity score weights

A dataset containing various propensity score weights for
`causaldata::nhefs_complete`, including weights for both binary (smoking
cessation) and categorical (alcohol frequency) exposures.

## Usage

``` r
nhefs_weights
```

## Format

A data frame with 1566 rows and 30 variables:

- qsmk:

  Quit smoking (binary exposure)

- alcoholfreq:

  Alcohol frequency (numeric 0-5)

- alcoholfreq_cat:

  Alcohol frequency as categorical (factor with levels: none,
  lt_12_per_year, 1_4_per_month, 2_3_per_week, daily); NA for unknown
  alcohol frequency

- race:

  Race

- age:

  Age

- sex:

  Sex

- education:

  Education level

- smokeintensity:

  Smoking intensity

- smokeyrs:

  Number of smoke-years

- exercise:

  Exercise level

- active:

  Daily activity level

- wt71:

  Participant weight in 1971 (baseline)

- wt82_71:

  Weight change from 1971 to 1982

- death:

  Death indicator

- wts:

  Simple inverse probability weight for binary exposure

- w_ate:

  ATE weight for binary exposure

- w_att:

  ATT weight for binary exposure

- w_atc:

  ATC weight for binary exposure

- w_atm:

  ATM weight for binary exposure

- w_ato:

  ATO weight for binary exposure

- w_cat_ate:

  ATE weight for categorical exposure (NA for unknown alcohol frequency)

- w_cat_att_none:

  ATT weight with "none" as focal category (NA for unknown alcohol
  frequency)

- w_cat_att_lt12:

  ATT weight with "lt_12_per_year" as focal category (NA for unknown
  alcohol frequency)

- w_cat_att_1_4mo:

  ATT weight with "1_4_per_month" as focal category (NA for unknown
  alcohol frequency)

- w_cat_att_2_3wk:

  ATT weight with "2_3_per_week" as focal category (NA for unknown
  alcohol frequency)

- w_cat_att_daily:

  ATT weight with "daily" as focal category (NA for unknown alcohol
  frequency)

- w_cat_atu_none:

  ATU weight with "none" as focal category (NA for unknown alcohol
  frequency)

- w_cat_ato:

  ATO weight for categorical exposure (NA for unknown alcohol frequency)

- w_cat_atm:

  ATM weight for categorical exposure (NA for unknown alcohol frequency)

- .fitted:

  Propensity score for binary exposure
