#' Add Calendar Features
#'
#' Add calendar-based features such as day of week, month, week of year, etc.
#' These features are commonly useful for capturing seasonal patterns in time series.
#'
#' @param df A data frame containing time series data
#' @param date Symbol or character naming the date column (must be Date class)
#' @param dow Logical, add day of week as ordered factor (Monday-Sunday)
#' @param woy Logical, add week of year (1-53)
#' @param month Logical, add month as factor (01-12)
#' @param eom Logical, add end-of-month indicator (0 or 1)
#' @param dom Logical, add day of month (1-31)
#'
#' @return Data frame with calendar features added
#'
#' @export
#' @importFrom rlang as_name ensym
#' @importFrom dplyr mutate
#' @examples
#' \dontrun{
#' # Add default calendar features (dow, month, eom)
#' df_cal <- feat_calendar_dt(df, date = date)
#'
#' # Add all calendar features
#' df_cal <- feat_calendar_dt(df, date = date,
#'                             dow = TRUE, woy = TRUE, month = TRUE,
#'                             eom = TRUE, dom = TRUE)
#' }
feat_calendar_dt <- function(df, date,
                             dow=TRUE, woy=FALSE, month=TRUE,
                             eom=TRUE, dom=FALSE) {
  date_col <- rlang::as_name(rlang::ensym(date))
  d <- df[[date_col]]
  result <- df
  if (inherits(d, "Date")) {
    if (dow)   result <- result %>% mutate(dow = factor(weekdays(.data[[date_col]]),
                                    levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")))
    if (month) result <- result %>% mutate(month = factor(format(.data[[date_col]], "%m"), levels=sprintf("%02d",1:12)))
    if (dom)   result <- result %>% mutate(dom = as.integer(format(.data[[date_col]], "%d")))
    if (eom)   result <- result %>% mutate(eom = .is_eom(.data[[date_col]]))
    if (woy)   result <- result %>% mutate(woy = as.integer(strftime(.data[[date_col]], "%V")))
  } else {
    warning("`feat_calendar_dt`: date is not a Date; calendar features skipped.")
  }
  result
}