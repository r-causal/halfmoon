#' NHEFS with various propensity score weights
#'
#' A dataset containing various propensity score weights for
#' `causaldata::nhefs_complete`.
#'
#' @format A data frame with 1566 rows and 14 variables: \describe{
#'   \item{qsmk}{Quit smoking} \item{race}{Race} \item{age}{Age} \item{sex}{Sex}
#'   \item{education}{Education level} \item{smokeintensity}{Smoking intensity}
#'   \item{smokeyrs}{Number of smoke-years} \item{exercise}{Exercise level}
#'   \item{active}{Daily activity level} \item{wt71}{Participant weight in 1971
#'   (baseline)} \item{w_ate}{ATE weight} \item{w_att}{ATT weight}
#'   \item{w_atc}{ATC weight} \item{w_atm}{ATM weight} \item{w_ato}{ATO weight}
#'    \item{.fitted}{Propensity score}
#'   }
"nhefs_weights"
