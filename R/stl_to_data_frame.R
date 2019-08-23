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
  
  if (class(stl) == "stlplus") {
    
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
      select(date, 
             everything()) %>% 
      select(-sub.labels,
             -weights)
    
    if (na_preserve) {
      df <- dplyr::mutate_if(df, is.numeric, ~if_else(is.na(raw), NA_real_, .))
    }
    
  } else {
    stop("Input not recognised.", call. = FALSE)
  }
  
  return(df)
  
}
