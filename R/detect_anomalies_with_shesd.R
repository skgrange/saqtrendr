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
  
  # date NA check
  
  # Check value
  stopifnot("value" %in% names(df) && is.numeric(df$value))
  
  # Check number of observations
  if (nrow(df) <= 2) {
    warning("Too few observations for anomaly detection to be performed...", call. = FALSE)
    return(df)
  }
  
  # Do the detection
  df_anomalies <- anomaly_detection_worker(
    df,
    max_fraction = max_fraction,
    direction = direction,
    long_term = long_term,
    only_last = only_last
  )
  
  # If zero row return, try again with long term argument
  if (nrow(df_anomalies) == 0 && !long_term) {
    
    warning(
      "Anomaly detection failed, attempting to use `long_term` algorithm...",
      call. = FALSE
    )
    
    df_anomalies <- anomaly_detection_worker(
        df,
        max_fraction = max_fraction,
        direction = direction,
        long_term = TRUE,
        only_last = only_last
      )
    
  }
  
  # Failure of algorithm
  if (nrow(df_anomalies) == 0) {
    warning(
      "Anomaly detection failed...",
      call. = FALSE
    )
    return(df)
  }

  # Code for anomaly, timestamp variable name here
  df <- mutate(df, anomaly = date %in% df_anomalies$timestamp)
  
  # Invalidate observations
  if (invalidate) {
    df <- mutate(df, value = if_else(anomaly, NA_real_, value))
  }
  
  return(df)
  
}


anomaly_detection_worker <- function(df, max_fraction, direction, long_term,
                                     only_last) {
  
  tryCatch({
    df %>% 
      select(date, 
             value) %>% 
      filter(!is.na(value)) %>% 
      AnomalyDetection::ad_ts(
        max_anoms = max_fraction, 
        direction = direction,
        longterm = long_term,
        only_last = only_last,
        verbose = FALSE,
        na.rm = FALSE
      )
  }, error = function(e) {
    tibble()
  })
  
}
