#' Clean factor or character levels by collapsing rare values
#'
#' Replaces infrequent levels in a factor or character column with a common label (e.g., "Other").
#' Optionally handles case sensitivity and ensures levels are ordered by frequency.
#'
#' @param df A data.frame containing the variable to clean.
#' @param col A column name (unquoted) of type character or factor.
#' @param min_freq Minimum frequency to keep a level. Levels below this will be collapsed (default: 10).
#' @param other_label Label assigned to collapsed levels (default: "Other").
#' @param ignore_case Logical indicating whether to ignore case when comparing levels (default: TRUE).
#'
#' @return A data.frame with the specified column cleaned and converted to a factor with frequency-ordered levels.
#' @examples
#' clean_levels(df, country, min_freq = 5)
clean_levels <- function(df, col, min_freq = 10, other_label = "Other", ignore_case = TRUE) {
  col_name <- deparse(substitute(col))
  vec <- df[[col_name]]
  
  if (!is.character(vec) && !is.factor(vec)) stop("`col` must be a factor or character variable.")
  vec <- as.character(vec)
  
  if (ignore_case) {
    vec <- tolower(vec)
  }
  
  freq_table <- table(vec, useNA = "no")
  keep_levels <- names(freq_table[freq_table >= min_freq])
  vec_cleaned <- ifelse(vec %in% keep_levels, vec, other_label)
  
  df[[col_name]] <- factor(vec_cleaned, levels = names(sort(table(vec_cleaned), decreasing = TRUE)))
  return(df)
}


#' Summarize the top levels of all categorical variables in a data frame
#'
#' Produces a tabyl-style frequency summary for each factor or character column.
#'
#' @param df A data.frame.
#' @param top_n Number of most frequent levels to show per variable (default: 5).
#' @param show_na Logical indicating whether to include NA counts (default: TRUE).
#'
#' @return A named list of tabyl summaries for each categorical variable.
#' @examples
#' summarize_factors(df, top_n = 3)
summarize_factors <- function(df, top_n = 5, show_na = TRUE) {
  cat_vars <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
  
  summaries <- lapply(cat_vars, function(var) {
    tab <- janitor::tabyl(df[[var]], show_na = show_na)
    names(tab)[1] <- "vars"
    if ("valid_percent" %in% names(tab)) {
      names(tab)[names(tab) == "valid_percent"] <- "valid_percent (No NA)"
    }
    tab <- head(tab[order(-tab$n), ], top_n)
    list(variable = var, summary = tab)
  })
  
  names(summaries) <- cat_vars
  return(summaries)
}



#' Identify and summarize duplicate rows in a data frame
#'
#' Detects duplicate rows based on one or more specified columns and optionally returns them in grouped form.
#'
#' @param df A data.frame to check for duplicates.
#' @param by Optional character vector of column names to check for duplication. If NULL, all columns are used.
#' @param return One of "summary", "data", or "grouped". Controls the output type (default: "summary").
#' @param grouped Logical. If TRUE and return = "grouped", returns grouped rows with same duplicate key.
#'
#' @return A printed summary message, or a data.frame of duplicates, optionally grouped.
#' @examples
#' identify_duplicates(df, by = "id", return = "grouped")
identify_duplicates <- function(df, by = NULL, return = "summary", grouped = FALSE) {
  if (!is.data.frame(df)) stop("df must be a data.frame.")
  
  if (is.null(by)) {
    dup_logical <- duplicated(df)
  } else {
    dup_logical <- duplicated(df[, by, drop = FALSE])
  }
  
  dup_count <- sum(dup_logical)
  total <- nrow(df)
  
  if (return == "summary") {
    cat(paste0("총 중복 행: ", dup_count, " (", round(100 * dup_count / total, 2), "%)\n"))
    
  } else if (return == "data") {
    return(df[dup_logical, , drop = FALSE])
    
  } else if (return == "grouped") {
    if (is.null(by)) stop("You must specify `by` when using return = 'grouped'.")
    
    grouped_df <- df %>%
      group_by(across(all_of(by))) %>%
      filter(n() > 1) %>%
      arrange(across(all_of(by))) %>%
      ungroup()
    
    return(grouped_df)
  } else {
    stop("Invalid value for `return`: must be one of 'summary', 'data', or 'grouped'")
  }
}



#' Summarize all numeric columns in a data frame
#'
#' Provides mean, standard deviation, min, max, and NA percentage for each numeric column.
#'
#' @param df A data.frame.
#' @param round_digits Number of digits to round statistics to (default: 2).
#'
#' @return A data.frame summarizing numeric variables.
#' @examples
#' summarize_numeric(df)
summarize_numeric <- function(df, round_digits = 2) {
  numeric_vars <- names(df)[sapply(df, is.numeric)]
  if (length(numeric_vars) == 0) stop("No numeric columns found.")
  
  summary_df <- data.frame(
    variable = character(),
    mean = numeric(),
    sd = numeric(),
    min = numeric(),
    max = numeric(),
    na_pct = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (var in numeric_vars) {
    x <- df[[var]]
    summary_df <- rbind(summary_df, data.frame(
      variable = var,
      mean = round(mean(x, na.rm = TRUE), round_digits),
      sd = round(sd(x, na.rm = TRUE), round_digits),
      min = round(min(x, na.rm = TRUE), round_digits),
      max = round(max(x, na.rm = TRUE), round_digits),
      na_pct = round(100 * mean(is.na(x)), round_digits)
    ))
  }
  
  return(summary_df)
}
