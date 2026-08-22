# `nhefs_weights` carries `psw` columns from propensity. Importing from
# propensity loads its namespace together with halfmoon's, which registers the
# S3 methods those columns rely on without the user attaching propensity.
#' @importFrom propensity is_psw
NULL
