#' Function to convert a data frame to a time series (\code{ts}) object. 
#' 
#' @param df Data frame containing a date variable. 
#' 
#' @param variable Variable in \code{df} to create a time series object with. 
#' 
#' @param period Period/interval to make time series. Currently, only month is
#' supported. 
#' 
#' @author Stuart K. Grange
#' 
#' @return A \code{ts} object.
#' 
data_frame_to_time_series <- function(df, variable = "value", period = "month") {
  
  # Get start date
  date_start <- min(df$date)
  
  if (period == "month") {
    frequency <- 12L
  } else {
    stop("`frequency` not supported.", call. = FALSE)
  }
  
  # Create the time series iobject
  ts <- ts(
    data = pull(df, !!variable),
    start = c(lubridate::year(date_start), lubridate::month(date_start)),
    frequency = frequency,
    names = "value"
  )
  
  return(ts)
  
}
