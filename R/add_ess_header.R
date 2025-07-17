#' Add ESS Table Header
#'
#' This function replaces the counts in the default header of
#' `gtsummary::tbl_svysummary()` tables to counts representing the
#' Effective Sample Size (ESS). See [`ess()`] for details.
#'
#' @param x (`tbl_svysummary`)\cr
#'   Object of class `'tbl_svysummary'` typically created with `gtsummary::tbl_svysummary()`.
#' @param header (`string`)\cr
#'   String specifying updated header.
#'   Review `gtsummary::modify_header()` for details on use.
#'
#' @returns a 'gtsummary' table
#' @export
#' @importFrom rlang .env .data
#'
#' @examplesIf rlang::is_installed(c("survey", "gtsummary", "cards", "cardx", "dplyr"))
#' svy <- survey::svydesign(~1, data = nhefs_weights, weights = ~ w_ate)
#'
#' gtsummary::tbl_svysummary(svy, include = c(age, sex, smokeyrs)) |>
#'   add_ess_header()
#' hdr <- paste0(
#'   "**{level}**  \n",
#'   "N = {n_unweighted}; ESS = {format(n, digits = 1, nsmall = 1)}"
#' )
#' gtsummary::tbl_svysummary(svy, by = qsmk, include = c(age, sex, smokeyrs)) |>
#'   add_ess_header(header = hdr)
add_ess_header <- function(x, header = "**{level}**  \nESS = {format(n, digits = 1, nsmall = 1)}") {
  # check inputs ---------------------------------------------------------------
  rlang::check_installed(c("cards", "dplyr"))
  if (!inherits(x, "tbl_svysummary")) {
    cli::cli_abort("Argument {.arg x} must be class {.cls tbl_svysummary} and typically created with {.fun gtsummary::tbl_svysummary}.")
  }
  if (!rlang::is_string(header)) {
    cli::cli_abort("Argument {.arg header} must be a string.")
  }
  updated_call <- append(x$call_list, list(add_ess_header = match.call()))

  # calculate ARD with ESS counts ----------------------------------------------
  ard_ess <- ard_survey_ess(data = x$inputs$data, by = x$inputs$by)
  if ("add_overall" %in% names(x$call_list)) {
    ard_ess_overall <- ard_survey_ess(data = x$inputs$data)
  }

  # replace statistics in `x$table_styling$table_header` with ESS --------------
  # no `tbl_svysummary(by)` specified
  if (rlang::is_empty(x$inputs$by)) {
    x$table_styling$header$modify_stat_level <- "Overall"
    x$table_styling$header$modify_stat_N <- ard_ess$stat[[1]]
    x$table_styling$header$modify_stat_n <- ard_ess$stat[[1]]
    x$table_styling$header$modify_stat_p <- 1
  }
  # with a `tbl_svysummary(by)` value but no overall column
  else if (!"add_overall" %in% names(x$call_list)) {
    x$table_styling$header <-
      dplyr::rows_update(
        x$table_styling$header,
        ard_ess |>
          dplyr::mutate(
            column = paste0("stat_", seq_len(nrow(.env$ard_ess))),
            modify_stat_level = purrr::map_chr(.data$group1_level, as.character),
            modify_stat_n = unlist(.data$stat),
            modify_stat_N = sum(.data$modify_stat_n),
            modify_stat_p = .data$modify_stat_n / .data$modify_stat_N
          ) |>
          dplyr::select("column", tidyselect::starts_with("modify_stat_")),
        by = "column"
      ) |>
      dplyr::mutate(
        modify_stat_N = sum(.data$modify_stat_n, na.rm = TRUE)
      )
  }
  # with both a `tbl_svysummary(by)` value and an overall column
  else {
    x$table_styling$header <-
      dplyr::rows_update(
        x$table_styling$header,
        dplyr::bind_rows(ard_ess_overall, ard_ess) |>
          dplyr::mutate(
            column = paste0("stat_", dplyr::row_number() - 1L),
            modify_stat_level = purrr::map_chr(.data$group1_level, \(x) as.character(x %||% "Overall")),
            modify_stat_n = unlist(.data$stat)
          ) |>
          dplyr::select("column", tidyselect::starts_with("modify_stat_")),
        by = "column"
      ) |>
      dplyr::mutate(
        modify_stat_N = unlist(.env$ard_ess_overall$stat),
        modify_stat_p = .data$modify_stat_n / .data$modify_stat_N
      )
  }

  # update the header and return table -----------------------------------------
  x <- gtsummary::modify_header(x, gtsummary::all_stat_cols() ~ header) # replace header
  x$cards$add_ess_header <- ard_ess # add ESS ARD to results
  x$call_list <- updated_call # update the call list

  # add abbreviation -----------------------------------------------------------
  if (grepl(pattern = "ESS", x = header, fixed = TRUE)) {
    x <- gtsummary::modify_abbreviation(x, "ESS = Effective Sample Size")
  }

  x
}

# this is an ARD function in the style of the cardx::ard_survey_*() functions
ard_survey_ess <- function(data, by = NULL) {
  # calculate ESS --------------------------------------------------------------
  cards::ard_continuous(
    data =
      data$variables |>
      dplyr::mutate(...cards_survey_design_weights_column... = stats::weights(data)),
    variables = "...cards_survey_design_weights_column...",
    by = {{ by }},
    statistic = everything() ~ list(ess = \(x) ess(x))
  ) |>
    dplyr::mutate(
      variable = "..ess..",
      context = "survey_ess",
      stat_label = "Effective Sample Size"
    )
}
