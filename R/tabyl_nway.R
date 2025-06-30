#' Generate an n-way frequency table from a data frame
#'
#' @description
#' This function counts the frequency of combinations of one or more variables
#' (typically 4 or more) in a data frame. It returns a tidy `data.frame`
#' of counts, suitable for further manipulation or formatting.
#'
#' Note that unlike the base `table()` function or janitor's `tabyl()` for 1-3 variables,
#' this function does not return percentages or split into nested lists.
#'
#' The `show_na` parameter controls whether rows with `NA` in the specified variables
#' are included in the count. If `show_na = FALSE`, such rows are dropped before counting.
#'
#' The `show_missing_levels` parameter is currently not implemented in this function.
#' It is reserved for future extension to handle factor levels with zero counts.
#'
#' @param data A `data.frame` containing the variables to tabulate.
#' @param ... One or more unquoted column names to count combinations of.
#' @param show_na Logical, default `TRUE`. Whether to include rows with `NA` values
#'   in the specified variables.
#' @param show_missing_levels Logical, default `TRUE`. Currently not implemented.
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
  
  if (!show_na) {
    data <- tidyr::drop_na(data, !!!vars)
  }
  
  df <- data %>%
    dplyr::count(!!!vars) %>%
    dplyr::ungroup()
  
  class(df) <- c("tabyl_nway", "data.frame")
  return(df)
}
