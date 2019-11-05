#' Function to classically decompose a time series with \code{stlplus}. 
#' 
#' @param df Data frame containing time series observations. 
#' 
#' @param window Span (in lags) of the loess window for seasonal extraction. 
#' This should be an odd number. 
#' 
#' @param na_preserve Should observations which were missing with the values
#' also be propagated into the the deseasonalised components? 
#' 
#' @return Tibble. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
decompose_with_stlplus <- function(df, window = 35, na_preserve = TRUE) {
  
  # Checks needed: missing values, monthly resolution
  
  if (!"date" %in% names(df)) {
    stop("`date` must be present in data frame.", call. = FALSE)
  }
  
  if (!lubridate::is.POSIXct(df$date)) {
    stop("`date` must be a parsed date (POSIXct).", call. = FALSE) 
  }
  
  if (!"value" %in% names(df)) {
    stop("Variable to be decomposed must be named `value`.", call. = FALSE)
  }
  
  if (nrow(df) <= 3) {
    warning("Too few observations to decompose...", call. = FALSE)
    return(tibble())
  } else {
    
    df <- tryCatch({
      
      df %>% 
        data_frame_to_time_series() %>% 
        stlplus::stlplus(s.window = window, s.degree = 0, robust = TRUE) %>% 
        stl_to_data_frame(na_preserve = na_preserve) 
      
    }, error = function(e) {
      warning("Decomposition failed...", call. = FALSE)
      tibble()
    })
    
  }
  
  return(df)
  
}
