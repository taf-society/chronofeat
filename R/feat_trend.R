#' Add Trend Features
#'
#' Generate polynomial trend features for time series data. Trends are computed
#' as sequential indices within each group, with optional polynomial transformations.
#'
#' @param df A data frame containing time series data
#' @param date Symbol or character naming the date column (used for sorting)
#' @param groups Character vector naming grouping columns
#' @param degrees Integer vector of polynomial degrees (e.g., c(1, 2) for linear and quadratic)
#'
#' @return Data frame with trend features added
#'
#' @details
#' Trend features are sequential indices (1, 2, 3, ...) within each group.
#' Polynomial degrees allow capturing non-linear trends:
#' - degree 1: linear trend (1, 2, 3, ...)
#' - degree 2: quadratic trend (1, 4, 9, ...)
#' - degree 3: cubic trend (1, 8, 27, ...)
#'
#' @export
#' @importFrom dplyr arrange across all_of group_by mutate ungroup row_number %>%
#' @examples
#' \dontrun{
#' # Add linear trend
#' df_trend <- feat_trend(df, date = date, groups = "store_id", degrees = 1)
#'
#' # Add linear and quadratic trends
#' df_trend <- feat_trend(df, date = date, groups = "store_id", degrees = c(1, 2))
#' }
feat_trend <- function(df, date, groups = NULL, degrees = 1L) {
  date_col   <- if (is.character(date)) date else rlang::as_name(rlang::ensym(date))
  groups_chr <- groups

  result <- df %>% arrange(across(all_of(c(groups_chr, date_col))))

  if (!is.null(groups_chr)) {
    result <- result %>% group_by(across(all_of(groups_chr)))
  }

  # Create base trend index
  result <- result %>% mutate(trend_idx = row_number())

  # Generate polynomial degrees
  for (d in degrees) {
    col_name <- paste0("trend", d)
    if (d == 1L) {
      result[[col_name]] <- result$trend_idx
    } else {
      result[[col_name]] <- result$trend_idx ^ d
    }
  }

  # Remove temporary index
  result$trend_idx <- NULL

  if (!is.null(groups_chr)) {
    result <- result %>% ungroup()
  }

  result
}