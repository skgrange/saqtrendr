#' Function to detect anomalies with the seasonal hybrid extreme studentised
#' deviate (SHESD) model. 
#' 
#' @param df Input time series stored in a data frame or tibble with \code{date}
#' and \code{value} variables. 
#' 
#' @param max_fraction Maximum fraction (from all observations in \code{df}) 
#' anomalies that SHESD will detect. 
#' 
#' @param direction Directionality of the anomalies to be detected. One of 
#' \code{pos}, \code{neg}, or \code{both}.
#' 
#' @param long_term Should a more efficient algorithm which employs piecewise 
#' approximation be applied to the time series? 
#' 
#' @param only_last Detect anomalies only within the last day or hour in the 
#' time series. One of \code{NULL}, \code{day}, or \code{hr}. 
#' 
#' @param invalidate Should observations in \code{df} be invalidated if detected
#' as anomalies? 
#' 
#' @author Stuart K. Grange
#' 
#' @seealso \url{https://github.com/hrbrmstr/AnomalyDetection}
#' 
#' @return Tibble. 
#' 
#' @export
detect_anomalies_with_shesd <- function(df, max_fraction = 0.02, 
                                        direction = "both", long_term = FALSE,
                                        only_last = NULL, invalidate = FALSE) {
  
  # Check dates
  if (!"date" %in% names(df)) {
    stop("`date` must be present in data frame.", call. = FALSE)
  }
  
  if (!lubridate::is.POSIXct(df$date)) {
    stop("`date` must be a parsed date (POSIXct).", call. = FALSE) 
  }
  
  # Check value
  stopifnot("value" %in% names(df) && is.numeric(df$value))
  
  # Do the detection
  df_anomalies <- df %>% 
    select(date, 
           value) %>% 
    AnomalyDetection::ad_ts(
      max_anoms = max_fraction, 
      direction = direction,
      longterm = long_term,
      only_last = only_last,
      verbose = FALSE,
      na.rm = FALSE
    ) %>% 
    rename(date = timestamp)
  
  # Code for anomaly
  df <- mutate(df, anomaly = date %in% df_anomalies$date)
  
  # Invalidate observations
  if (invalidate) {
    df <- mutate(df, value = if_else(anomaly, NA_real_, value))
  }
  
  return(df)
  
}
