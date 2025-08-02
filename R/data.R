#' NHEFS with various propensity score weights
#'
#' A dataset containing various propensity score weights for
#' `causaldata::nhefs_complete`, including weights for both binary (smoking
#' cessation) and categorical (alcohol frequency) exposures.
#'
#' @format A data frame with 1566 rows and 30 variables: \describe{
#'   \item{qsmk}{Quit smoking (binary exposure)}
#'   \item{alcoholfreq}{Alcohol frequency (numeric 0-5)}
#'   \item{alcoholfreq_cat}{Alcohol frequency as categorical (factor with levels: none, lt_12_per_year, 1_4_per_month, 2_3_per_week, daily, unknown)}
#'   \item{race}{Race} \item{age}{Age} \item{sex}{Sex}
#'   \item{education}{Education level} \item{smokeintensity}{Smoking intensity}
#'   \item{smokeyrs}{Number of smoke-years} \item{exercise}{Exercise level}
#'   \item{active}{Daily activity level} \item{wt71}{Participant weight in 1971 (baseline)} 
#'   \item{wt82_71}{Weight change from 1971 to 1982} \item{death}{Death indicator}
#'   \item{wts}{Simple inverse probability weight for binary exposure}
#'   \item{w_ate}{ATE weight for binary exposure} \item{w_att}{ATT weight for binary exposure}
#'   \item{w_atc}{ATC weight for binary exposure} \item{w_atm}{ATM weight for binary exposure} 
#'   \item{w_ato}{ATO weight for binary exposure}
#'   \item{w_cat_ate}{ATE weight for categorical exposure (NA for unknown alcohol frequency)}
#'   \item{w_cat_att_none}{ATT weight with "none" as focal category (NA for unknown alcohol frequency)}
#'   \item{w_cat_att_lt12}{ATT weight with "lt_12_per_year" as focal category (NA for unknown alcohol frequency)}
#'   \item{w_cat_att_1_4mo}{ATT weight with "1_4_per_month" as focal category (NA for unknown alcohol frequency)}
#'   \item{w_cat_att_2_3wk}{ATT weight with "2_3_per_week" as focal category (NA for unknown alcohol frequency)}
#'   \item{w_cat_att_daily}{ATT weight with "daily" as focal category (NA for unknown alcohol frequency)}
#'   \item{w_cat_atu_none}{ATU weight with "none" as focal category (NA for unknown alcohol frequency)}
#'   \item{w_cat_ato}{ATO weight for categorical exposure (NA for unknown alcohol frequency)}
#'   \item{w_cat_atm}{ATM weight for categorical exposure (NA for unknown alcohol frequency)}
#'   \item{.fitted}{Propensity score for binary exposure}
#'   }
"nhefs_weights"
