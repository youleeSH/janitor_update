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
