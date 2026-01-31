#' Create Lag and Moving Average Features
#'
#' Add lagged and moving average features for target and exogenous variables.
#' This function creates features suitable for time series modeling by computing
#' lags and rolling averages within groups.
#'
#' @param df A data frame containing time series data
#' @param date Character string naming the date column
#' @param target Character string naming the target variable column
#' @param p Integer, number of target lags to create (e.g., p=12 creates lag_1 through lag_12)
#' @param q Integer vector, moving average window sizes for target (e.g., q=c(7,28))
#' @param groups Character vector naming grouping columns (e.g., c("store", "item"))
#' @param xreg Character vector naming exogenous variable columns to transform
#' @param xreg_lags Named list of lag specifications for exogenous variables
#'   (e.g., list(price = c(1,7), promotion = c(0,1)))
#' @param xreg_ma Named list of MA window specifications for exogenous variables
#'   (e.g., list(price = c(7,28)))
#'
#' @return Data frame with original columns plus new lag and MA features
#'
#' @details
#' Lags and MAs are computed within each group separately. The data is automatically
#' sorted by groups and date before computation. Use lag 0 in xreg_lags to include
#' the current value of an exogenous variable.
#'
#' @export
#' @importFrom dplyr arrange across all_of group_by mutate ungroup lag %>%
#' @importFrom slider slide_dbl
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' # Create 7 lags and 7-day MA for sales
#' df_feat <- feat_lag_ma_dt(df, date = "date", target = "sales",
#'                            p = 7, q = 7, groups = "store_id")
#'
#' # Add exogenous variable features
#' df_feat <- feat_lag_ma_dt(df, date = "date", target = "sales",
#'                            p = 3, groups = "store_id",
#'                            xreg = "price",
#'                            xreg_lags = list(price = c(0, 1, 7)))
#' }
feat_lag_ma_dt <- function(df, date, target,
                           p = NULL, q = NULL,
                           groups = NULL,
                           xreg = NULL, xreg_lags = NULL, xreg_ma = NULL) {
  date_col   <- date
  target_col <- target
  groups_chr <- groups

  result <- df %>% arrange(across(all_of(c(groups_chr, date_col))))

  # ensure numeric for rolling on target
  if (!is.null(q) && length(q)) result <- coerce_numeric_col(result, target_col)

  # target lags - p can be a vector of specific lag indices
  # e.g., p = c(1, 4, 6, 12) creates lag_1, lag_4, lag_6, lag_12
  if (!is.null(p) && length(p) > 0 && all(p > 0)) {
    lag_indices <- as.integer(p)
    if (!is.null(groups_chr)) {
      result <- result %>%
        group_by(across(all_of(groups_chr))) %>%
        mutate(across(all_of(target_col),
                     list_to_named_list(lag_indices, function(n) {
                       ~lag(.x, n = n)
                     }, function(n) paste0(target_col, "_lag_", n)),
                     .names = "{.fn}")) %>%
        ungroup()
    } else {
      for (n in lag_indices) {
        nm <- paste0(target_col, "_lag_", n)
        result[[nm]] <- lag(result[[target_col]], n = n)
      }
    }
  }

  # target MAs
  if (!is.null(q) && length(q)) {
    if (!is.null(groups_chr)) {
      result <- result %>% group_by(across(all_of(groups_chr)))
      for (w in q) {
        nm <- paste0(target_col, "_ma_", w)
        result <- result %>%
          mutate(!!nm := slide_dbl(.data[[target_col]], mean, .before = w - 1, .complete = TRUE, na.rm = TRUE))
      }
      result <- result %>% ungroup()
    } else {
      for (w in q) {
        nm <- paste0(target_col, "_ma_", w)
        result[[nm]] <- slide_dbl(result[[target_col]], mean, .before = w - 1, .complete = TRUE, na.rm = TRUE)
      }
    }
  }

  # xregs
  if (!is.null(xreg) && length(xreg)) {
    for (xr in xreg) {
      # if we'll compute rolling on this xreg, ensure numeric
      if (!is.null(xreg_ma) && !is.null(xreg_ma[[xr]]) && length(xreg_ma[[xr]])) {
        result <- coerce_numeric_col(result, xr)
      }
      # lags
      L <- xreg_lags[[xr]] %||% integer()
      L <- setdiff(L, 0L)
      if (length(L)) {
        if (!is.null(groups_chr)) {
          result <- result %>%
            group_by(across(all_of(groups_chr))) %>%
            mutate(across(all_of(xr),
                         list_to_named_list(L, function(n) {
                           ~lag(.x, n = n)
                         }, function(n) paste0(xr, "_lag_", n)),
                         .names = "{.fn}")) %>%
            ungroup()
        } else {
          for (n in L) {
            nm <- paste0(xr, "_lag_", n)
            result[[nm]] <- lag(result[[xr]], n = n)
          }
        }
      }
      # moving averages
      ws <- xreg_ma[[xr]] %||% integer()
      if (length(ws)) {
        if (!is.null(groups_chr)) {
          result <- result %>% group_by(across(all_of(groups_chr)))
          for (w in ws) {
            nm <- paste0(xr, "_ma_", w)
            result <- result %>%
              mutate(!!nm := slide_dbl(.data[[xr]], mean, .before = w - 1, .complete = TRUE, na.rm = TRUE))
          }
          result <- result %>% ungroup()
        } else {
          for (w in ws) {
            nm <- paste0(xr, "_ma_", w)
            result[[nm]] <- slide_dbl(result[[xr]], mean, .before = w - 1, .complete = TRUE, na.rm = TRUE)
          }
        }
      }
    }
  }
  result
}
