#' Functions to classically decompose a monthly time series with \code{stlplus}. 
#' 
#' The \code{decompose_with_stlplus_extra} variant has different defaults and
#' also computes a \code{trend_smooth} variable. 
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
#' @param span For the \code{trend_smooth} variable, what loess span should be
#' used? 
#' 
#' @param as_long Should the return be in "long" format. 
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
      # Warning suppression is for when there are missing values, rather common
      suppressWarnings(
        df |> 
          data_frame_to_time_series() |> 
          stlplus::stlplus(s.window = window, s.degree = 0, robust = TRUE) |> 
          stl_to_data_frame(na_preserve = na_preserve) 
      )
    }, error = function(e) {
      cli::cli_warn("Decomposition failed...")
      tibble()
    })
    
  }
  
  return(df)
  
}


#' @rdname decompose_with_stlplus
#' @export
decompose_with_stlplus_extra <- function(df, window = 35, na_preserve = FALSE, 
                                         span = 0.75, as_long = FALSE) {
  
  # Decompose the time series and return many variables
  df_decompose <- df |> 
    decompose_with_stlplus(window = window, na_preserve = na_preserve) |> 
    mutate(missing = is.na(raw), 
           trend_and_seasonal = trend + seasonal) |> 
    relocate(missing,
             .before = raw)
  
  # Train model on trend component, not trend + remainder
  model <- stats::loess(
    trend ~ as.numeric(date), data = df_decompose, span = span
  )
  
  # Add prediction to input, no non-missing observations in prediction
  df_decompose <- mutate(
    df_decompose, trend_smooth = unname(stats::predict(model, df))
  )
  
  # Make long data
  if (as_long) {
    df_decompose <- df_decompose |> 
      tidyr::pivot_longer(-date:-missing, names_to = "value_type") |> 
      arrange(value_type,
              date)
  }
  
  return(df_decompose)
  
}
