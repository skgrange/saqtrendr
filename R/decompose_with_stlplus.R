#' Function to classically decompose a monthly time series with \code{stlplus}. 
#' 
#' @param df Data frame containing time series observations. The resolution 
#' must be monthly and this will be tested for. 
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
  
  if (!"date" %in% names(df)) {
    cli::cli_abort("`date` must be present in input.")
  }
  
  if (!lubridate::is.POSIXct(df$date)) {
    cli::cli_abort("`date` must be a parsed date (POSIXct).")
  }
  
  if (!threadr::detect_date_interval(df$date, text_return = TRUE) == "month") {
    cli::cli_abort("Time series must be at monthly resolution.")
  }
  
  if (!"value" %in% names(df)) {
    cli::cli_abort("Variable to be decomposed must be named `value`.")
  }
  
  if (nrow(df) <= 3) {
    cli::cli_warn("Too few observations to decompose...")
    return(tibble())
  } else {
    
    df <- tryCatch({
      df %>% 
        data_frame_to_time_series() %>% 
        stlplus::stlplus(s.window = window, s.degree = 0, robust = TRUE) %>% 
        stl_to_data_frame(na_preserve = na_preserve)
    }, error = function(e) {
      cli::cli_warn("Decomposition failed...")
      tibble()
    })
    
  }
  
  return(df)
  
}
