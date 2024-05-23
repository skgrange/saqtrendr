#' Function to decompose a time series with Kolmogorov-Zurbenko (K-Z) filters.
#' 
#' @param df A tibble containing time series observations.
#' 
#' @return Tibble. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
decompose_with_kz_filters <- function(df) {
  
  if (!"date" %in% names(df)) {
    cli::cli_abort("`date` must be present in input tibble.")
  }
  
  # Check for daily time series
  if (threadr::detect_date_interval(df$date) != 86400L) {
    cli::cli_abort("Time series must be of daily resolution.")
  }
  
  if (!"value" %in% names(df)) {
    cli::cli_abort("Variable to be decomposed must be named `value`.")
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
