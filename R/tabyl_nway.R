#' Generate an n-way frequency table from a data frame
#'
#' @description
#' This function counts the frequency of combinations of four or more variables
#' in a data frame. It returns a tidy `data.frame`
#' of counts, suitable for further manipulation or formatting.
#'
#' Unlike `tabyl()` for 1–3 variables, this function does not return percentages
#' or nested list structures. Instead, it produces a flat frequency table.
#'
#' If `show_na = FALSE`, rows containing any `NA` in the specified variables are dropped.
#' If `show_missing_levels = TRUE`, the function ensures all combinations of factor levels
#' (even those not present in the data) appear in the result with a count of 0.
#'
#' @param data A `data.frame` containing the variables to tabulate.
#' @param ... One or more unquoted column names to count combinations of.
#' @param show_na Logical, default `TRUE`. Whether to include rows with `NA` values
#'   in any of the specified variables.
#' @param show_missing_levels Logical, default `TRUE`. Whether to include zero-count combinations
#'   of factor levels.
#'
#' @return A `data.frame` with frequency counts of the specified variable combinations,
#'   assigned the class `"tabyl_nway"`.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' data <- tibble::tibble(
#'   group = c("A", "A", "B", NA, "B"),
#'   category = c("X", "Y", "X", "Y", NA),
#'   type = c("T1", "T2", "T1", "T1", "T2"),
#'   region = c("East", "West", "East", "West", "East")
#' )
#'
#' # Count combinations of 4 variables, including NAs
#' tabyl_nway(data, group, category, type, region)
#'
#' # Exclude rows with NA in any of the variables
#' tabyl_nway(data, group, category, type, region, show_na = FALSE)



tabyl_nway <- function(data, ..., show_na = TRUE, show_missing_levels = TRUE) {
  vars <- rlang::ensyms(...)
  
  # Drop rows with NA if show_na = FALSE
  if (!show_na) {
    data <- tidyr::drop_na(data, !!!vars)
  }
  
  # Ensure selected variables are available and optionally convert to factor
  var_names <- purrr::map_chr(vars, rlang::as_string)
  
  if (show_missing_levels) {
    data <- dplyr::mutate(data, dplyr::across(dplyr::all_of(var_names), ~{
      if (is.factor(.x)) {
        .x
      } else {
        factor(.x)
      }
    }))
  }
  
  # Count combinations
  df <- data %>%
    dplyr::count(!!!vars, name = "n")
  
  # Fill in all missing combinations with 0
  if (show_missing_levels) {
    df <- tidyr::complete(df, !!!rlang::syms(var_names), fill = list(n = 0))
  }
  
  df <- dplyr::arrange(df, !!!rlang::syms(var_names))
  class(df) <- c("tabyl_nway", "data.frame")
  return(df)
}

