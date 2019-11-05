#' Function to apply the adaptive Kolmogorov-Zurbenko (KZA) filter to a time 
#' series, usually used for discontinuity identification. 
#' 
#' @param df Data frame containing daily time series observations.
#' 
#' @param window Window for K-Z filters. 
#' 
#' @param k Number of iterations. 
#' 
#' @param impute_tails Should the leading and trailing values in the time series
#' be imputed after the K-Z filters have been applied? 
#' 
#' @return Tibble. 
#' 
#' @author Sturat K. Grange
#' 
#' @export
apply_kza_filter <- function(df, window = 365, k = 3, impute_tails = FALSE) {
  
  # Check inputs
  if (!"date" %in% names(df)) {
    stop("`date` must be present in input.", call. = FALSE)
  }
  
  # Check for daily time series
  if (threadr::detect_date_interval(df$date) != 86400) {
    stop("Time series must be of daily resolution.", call. = FALSE)
  }
  
  if (!"value" %in% names(df)) {
    stop("Variable to be decomposed must be named `value`.", call. = FALSE)
  }
  
  # Apply low pass filter to the data
  df <- mutate(df, value_low_pass = kza::kz(value, m = window, k = k))
  
  # Use adaptive filter to find signal
  list_signal <- kza::kza(
    df$value, 
    m = window, 
    k = k, 
    y = df$value_low_pass, 
    min_size = 10, 
    impute_tails = impute_tails
  )
  
  # Build tibble for return
  df_kz <- tibble(
    date = df$date,
    value = df$value,
    value_kz = list_signal$kz,
    value_kza = list_signal$kza
  )
  
  return(df_kz)
  
}
