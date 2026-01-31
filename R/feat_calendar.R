#' Add Calendar Features
#'
#' Add calendar-based features such as day of week, month, week of year, etc.
#' These features are commonly useful for capturing seasonal patterns in time series.
#'
#' @param df A data frame containing time series data
#' @param date Symbol or character naming the date column (Date or POSIXct class)
#' @param dow Logical, add day of week as ordered factor (1=Monday, 7=Sunday)
#' @param woy Logical, add week of year (1-53)
#' @param month Logical, add month as factor (01-12)
#' @param eom Logical, add end-of-month indicator (0 or 1)
#' @param dom Logical, add day of month (1-31)
#' @param hod Logical, add hour of day (0-23) - requires POSIXct
#' @param moh Logical, add minute of hour (0-59) - requires POSIXct
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
#'
#' # Add sub-daily features for POSIXct data
#' df_cal <- feat_calendar_dt(df, date = datetime, hod = TRUE, moh = TRUE)
#' }
feat_calendar_dt <- function(df, date,
                             dow=TRUE, woy=FALSE, month=TRUE,
                             eom=TRUE, dom=FALSE, hod=FALSE, moh=FALSE) {
  date_col <- rlang::as_name(rlang::ensym(date))
  d <- df[[date_col]]
  result <- df
  is_date <- inherits(d, "Date")
  is_datetime <- inherits(d, "POSIXct")

  if (is_date || is_datetime) {
    # Use strftime("%u") for locale-independent weekday numbers (1=Monday, 7=Sunday)
    if (dow) {
      result <- result %>% mutate(
        dow = factor(strftime(.data[[date_col]], "%u"), levels = sprintf("%d", 1:7))
      )
    }
    if (month) result <- result %>% mutate(month = factor(format(.data[[date_col]], "%m"), levels = sprintf("%02d", 1:12)))
    if (dom)   result <- result %>% mutate(dom = as.integer(format(.data[[date_col]], "%d")))
    if (eom)   result <- result %>% mutate(eom = .is_eom(.data[[date_col]]))
    if (woy)   result <- result %>% mutate(woy = as.integer(strftime(.data[[date_col]], "%V")))

    # Sub-daily features (require POSIXct)
    if (hod) {
      if (!is_datetime) {
        warning("hod requires POSIXct datetime; Date column will return 0 for all rows")
        result <- result %>% mutate(hod = 0L)
      } else {
        result <- result %>% mutate(hod = as.integer(format(.data[[date_col]], "%H")))
      }
    }
    if (moh) {
      if (!is_datetime) {
        warning("moh requires POSIXct datetime; Date column will return 0 for all rows")
        result <- result %>% mutate(moh = 0L)
      } else {
        result <- result %>% mutate(moh = as.integer(format(.data[[date_col]], "%M")))
      }
    }
  } else {
    warning("`feat_calendar_dt`: date column is neither Date nor POSIXct; calendar features skipped.")
  }
  result
}
