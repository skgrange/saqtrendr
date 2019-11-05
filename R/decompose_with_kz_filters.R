#' Function to decompose a time series with Kolmogorov-Zurbenko (K-Z) filters.
#' 
#' @param df Data frame containing time series observations. 
#' 
#' @return Tibble. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
decompose_with_kz_filters <- function(df) {
  
  if (!"date" %in% names(df)) {
    stop("`date` must be present in data frame.", call. = FALSE)
  }
  
  # Check for daily time series
  if (threadr::detect_date_interval(df$date) != 86400) {
    stop("Time series must be of daily resolution.", call. = FALSE)
  }
  
  if (!"value" %in% names(df)) {
    stop("Variable to be decomposed must be named `value`.", call. = FALSE)
  }
  
  # To time series object for kza
  ts <- data_frame_to_time_series(df, variable = "value", period = "day")
  
  # Build tibble
  df_kz <- tibble(
    date = df$date,
    raw = df$value,
    baseline = as.numeric(kza::kz(ts, m = 15, k = 5)),
    trend = as.numeric(kza::kz(ts, m = 365, k = 3))
  ) %>% 
    mutate(remainder = raw - baseline,
           trend_and_remainder = trend + remainder)
  
  return(df_kz)
  
}
