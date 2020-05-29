#' Function to forecast observations in a time series. 
#' 
#' @param df Input tibble containing time series data. \code{df} requires a 
#' \code{date} and \code{value} variables. 
#' 
#' @param method Modelling method to use. Either \code{exponential} or 
#' \code{arima} for exponential smoothing or autoregressive integrated moving
#' average (ARIMA). 
#' 
#' @param n Number of steps to forecast to. 
#' 
#' @param ci Confidence level for prediction intervals.
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @export
forecast_time_series <- function(df, method = c("arima", "exponential"), n = 10, 
                                 ci = 80) {
  
  if (!"date" %in% names(df)) {
    stop("`date` must be present in data frame.", call. = FALSE)
  }
  
  if (!lubridate::is.POSIXct(df$date)) {
    stop("`date` must be a parsed date (POSIXct).", call. = FALSE) 
  }
  
  if (!"value" %in% names(df)) {
    stop("Variable to be decomposed must be named `value`.", call. = FALSE)
  }
  
  stopifnot(length(ci) == 1)
  
  # Get minimum data
  date_start <- min(df$date)
  
  # To time series
  ts <- threadr::data_frame_to_timeseries(df, threadr::time_zone(df$date))$value
  
  # Build model
  if (method[1] == "exponential") {
    list_fit <- forecast::ets(ts)
  } else if (method[1] == "arima") {
    list_fit <- forecast::auto.arima(ts)
  }
  
  # Predict the future
  list_forecast <- forecast::forecast(list_fit, h = n, level = ci)
  
  # To tibble
  df_forecast <- tidy_forecast_list(list_forecast, date = date_start)
  
  return(df_forecast)
  
  
}


tidy_forecast_list <- function(l, date) {
  
  # List to table and clean
  df <- l %>% 
    sweep::sw_sweep() %>% 
    rename(variable = key) %>% 
    mutate(index = index - 1,
           date = !!date + index,
           variable = if_else(variable == "actual", "value", "forecast")) %>% 
    select(-index)
  
  # Clean upper and lower names
  names(df) <- if_else(stringr::str_detect(names(df), "^lo"), "lower", names(df))
  names(df) <- if_else(stringr::str_detect(names(df), "^hi"), "upper", names(df))
  
  # Select what we want
  df <- df %>% 
    select(date,
           variable,
           value,
           lower,
           upper)
  
  return(df)
  
}
