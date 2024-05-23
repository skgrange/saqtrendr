#' Function to convert a \code{stlplus} object to a data frame/tibble. 
#' 
#' @param stl An \code{stl} object. 
#' 
#' @param date_round Should dates be rounded? 
#' 
#' @param na_preserve Should observations which were missing with the values
#' also be propagated into the the deseasonalised components? 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @export
stl_to_data_frame <- function(stl, date_round = TRUE, na_preserve = FALSE) {
  
  if (inherits(stl, "stlplus")) {
    
    # Get dates and transform into POSIXct
    date <- stl$time
    date_offset <- (date - floor(date)) * 31536000
    date <- lubridate::ymd(floor(date), truncated = 2, tz = "UTC") + date_offset
    if (date_round) date <- lubridate::round_date(date, "month")
    
    # To tibble
    df <- stl$data %>% 
      mutate(date = !!date,
             trend_and_remainder = trend + remainder) %>% 
      as_tibble() %>% 
      relocate(date) %>% 
      select(-sub.labels,
             -weights)
    
    if (na_preserve) {
      df <- dplyr::mutate_at(df, dplyr::vars(-1:-2), ~if_else(is.na(raw), NA_real_, .))
    }
    
  } else {
    cli::cli_abort("Input not recognised.")
  }
  
  return(df)
  
}
