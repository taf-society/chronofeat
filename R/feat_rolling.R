#' Add Rolling Window Statistics
#'
#' Compute rolling window statistics (sum, standard deviation, min, max) and
#' rolling trend slopes for the target variable. All statistics are computed
#' within groups if specified.
#'
#' @param df A data frame containing time series data
#' @param date Symbol or character naming the date column
#' @param target Symbol or character naming the target variable column
#' @param groups Character vector naming grouping columns
#' @param windows Integer vector of window sizes for rolling statistics (e.g., c(7, 28, 90))
#' @param stats Character vector specifying which statistics to compute:
#'   "sum", "sd", "min", "max"
#' @param trend_windows Integer vector of window sizes for rolling linear trend slopes
#'
#' @return Data frame with rolling statistic features added
#'
#' @details
#' Rolling statistics use right-aligned windows (including current observation).
#' Trend slopes are computed by fitting y ~ x within each window.
#' Missing values are handled with na.rm = TRUE for statistics.
#'
#' @export
#' @importFrom rlang as_name ensym
#' @importFrom dplyr arrange across all_of group_by ungroup %>%
#' @importFrom slider slide_dbl
#' @importFrom stats lm coef sd
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' # Add 7-day and 28-day rolling statistics
#' df_roll <- feat_rolling_dt(df, date = date, target = sales,
#'                             groups = "store_id",
#'                             windows = c(7, 28),
#'                             stats = c("sum", "sd", "min", "max"))
#'
#' # Add rolling trend slopes
#' df_roll <- feat_rolling_dt(df, date = date, target = sales,
#'                             groups = "store_id",
#'                             windows = c(7, 28),
#'                             stats = "sum",
#'                             trend_windows = c(7, 14))
#' }
feat_rolling_dt <- function(df, date, target, groups = NULL,
                            windows = c(7, 28),
                            stats = c("sum","sd","min","max"),
                            trend_windows = NULL) {
  date_col   <- if (is.character(date)) date else rlang::as_name(rlang::ensym(date))
  target_col <- if (is.character(target)) target else rlang::as_name(rlang::ensym(target))
  groups_chr <- groups

  result <- df %>% arrange(across(all_of(c(groups_chr, date_col))))

  # ensure numeric target for rolling ops
  result <- coerce_numeric_col(result, target_col)

  if (!is.null(groups_chr)) {
    result <- result %>% group_by(across(all_of(groups_chr)))

    for (w in windows) {
      if ("sum" %in% stats) {
        nm <- paste0(target_col, "_rollsum_", w)
        result <- result %>%
          mutate(!!nm := slide_dbl(.data[[target_col]],
                                    function(x) if (any(!is.na(x))) sum(x, na.rm = TRUE) else NA_real_,
                                    .before = w - 1, .complete = TRUE))
      }
      if ("sd"  %in% stats) {
        nm <- paste0(target_col, "_rollsd_", w)
        result <- result %>%
          mutate(!!nm := slide_dbl(.data[[target_col]],
                                    function(x) if (sum(!is.na(x)) >= 2) sd(x, na.rm = TRUE) else NA_real_,
                                    .before = w - 1, .complete = TRUE))
      }
      if ("min" %in% stats) {
        nm <- paste0(target_col, "_rollmin_", w)
        result <- result %>%
          mutate(!!nm := slide_dbl(.data[[target_col]],
                                    function(x) if (any(!is.na(x))) min(x, na.rm = TRUE) else NA_real_,
                                    .before = w - 1, .complete = TRUE))
      }
      if ("max" %in% stats) {
        nm <- paste0(target_col, "_rollmax_", w)
        result <- result %>%
          mutate(!!nm := slide_dbl(.data[[target_col]],
                                    function(x) if (any(!is.na(x))) max(x, na.rm = TRUE) else NA_real_,
                                    .before = w - 1, .complete = TRUE))
      }
    }

    if (!is.null(trend_windows) && length(trend_windows)) {
      for (w in trend_windows) {
        nm <- paste0(target_col, "_rollslope_", w)
        result <- result %>%
          mutate(!!nm := slide_dbl(.data[[target_col]],
                                    function(y) {
                                      if (all(is.na(y)) || length(y) < 2L) return(NA_real_)
                                      x <- seq_along(y)
                                      as.numeric(coef(stats::lm(y ~ x))[2])
                                    },
                                    .before = w - 1, .complete = TRUE))
      }
    }

    result <- result %>% ungroup()
  } else {
    for (w in windows) {
      if ("sum" %in% stats) {
        nm <- paste0(target_col, "_rollsum_", w)
        result[[nm]] <- slide_dbl(result[[target_col]],
                                   function(x) if (any(!is.na(x))) sum(x, na.rm = TRUE) else NA_real_,
                                   .before = w - 1, .complete = TRUE)
      }
      if ("sd"  %in% stats) {
        nm <- paste0(target_col, "_rollsd_", w)
        result[[nm]] <- slide_dbl(result[[target_col]],
                                   function(x) if (sum(!is.na(x)) >= 2) sd(x, na.rm = TRUE) else NA_real_,
                                   .before = w - 1, .complete = TRUE)
      }
      if ("min" %in% stats) {
        nm <- paste0(target_col, "_rollmin_", w)
        result[[nm]] <- slide_dbl(result[[target_col]],
                                   function(x) if (any(!is.na(x))) min(x, na.rm = TRUE) else NA_real_,
                                   .before = w - 1, .complete = TRUE)
      }
      if ("max" %in% stats) {
        nm <- paste0(target_col, "_rollmax_", w)
        result[[nm]] <- slide_dbl(result[[target_col]],
                                   function(x) if (any(!is.na(x))) max(x, na.rm = TRUE) else NA_real_,
                                   .before = w - 1, .complete = TRUE)
      }
    }

    if (!is.null(trend_windows) && length(trend_windows)) {
      for (w in trend_windows) {
        nm <- paste0(target_col, "_rollslope_", w)
        result[[nm]] <- slide_dbl(result[[target_col]],
                                  function(y) {
                                    if (all(is.na(y)) || length(y) < 2L) return(NA_real_)
                                    x <- seq_along(y)
                                    as.numeric(coef(stats::lm(y ~ x))[2])
                                  },
                                  .before = w - 1, .complete = TRUE)
      }
    }
  }

  result
}
