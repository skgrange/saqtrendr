#' Function to conduct a robust trend test. 
#' 
#' \code{saq_trend_test} can conduct the trend tests in parallel if a 
#' \strong{futures} backend has been registered. 
#' 
#' @param df Input data frame containing a time series. 
#' 
#' @param by Which variables should be used as grouping variables? 
#' 
#' @param window Span (in lags) of the loess window for seasonal extraction. 
#' This should be an odd number.
#' 
#' @param na_preserve Should observations which were missing with the values 
#' also be propagated into the the deseasonalised components? 
#' 
#' @param alpha Confidence interval of the slope, default is 0.05 for 95 % 
#' confidence intervals. 
#' 
#' @param auto_correlation Should auto correlation be considered in the 
#' estimates?
#' 
#' @param progress Should a progress bar be displayed for the trend test 
#' calculations? 
#' 
#' @author Stuart K. Grange
#' 
#' @return Named list. 
#' 
#' @export
saq_trend_test <- function(df, by = as.character(), window = 35, 
                           na_preserve = TRUE, alpha = 0.05, 
                           auto_correlation = FALSE, progress = FALSE) {
  
  # Could also return the nested tibble? 
  
  # Do the transformations and calculations in a nested tibble
  df_nest <- df %>% 
    dplyr::group_by_at(by) %>% 
    dplyr::group_nest(.key = "observations") %>% 
    mutate(
      decomposed = furrr::future_map(
        observations, 
        ~saqtrendr::decompose_with_stlplus(
          ., 
          window = window, 
          na_preserve = na_preserve
        )
      ),
      trend_test = furrr::future_map(
        decomposed,
        ~smonitor::theil_sen_trend_test(
          ., 
          variable = "trend_and_remainder",
          deseason = FALSE,
          alpha = alpha,
          auto_correlation = auto_correlation
        ),
        .progress = progress
      )
    )
  
  # Get pieces
  df_decomposed <- df_nest %>% 
    select(-observations,
           -trend_test) %>% 
    tidyr::unnest(decomposed)
  
  # And the trend tests
  df_trend_tests <- df_nest %>% 
    select(-observations,
           -decomposed) %>% 
    tidyr::unnest(trend_test)
  
  # Build list for return
  list_trends <- list(
    decomposed = df_decomposed,
    trend_tests = df_trend_tests
  )
  
  return(list_trends)
  
}
