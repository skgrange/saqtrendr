#' Function to build a trend plot from the output of \code{\link{saq_trend_test}}.
#' 
#' @param df Input data frame, generally containing deseasonalised observations
#' from \code{\link{saq_trend_test}}. 
#' 
#' @param df_tests Input data frame, generally containing trend tests calculated
#' by \code{\link{saq_trend_test}}.
#' 
#' @param label Should the plots have a trend magnitude label added? 
#' 
#' @param round If \code{label} is \code{TRUE}, how many digits should the trend
#' estimated be rounded too?  
#' 
#' @param y_location If \code{label} is \code{TRUE}, where should the label be
#' positioned? In a decimal of the maximum y value. 
#' 
#' @param facet_variable What variable in \code{df} and \code{df_tests} should
#' be used for faceting? 
#' 
#' @param scales If \code{facet_variable} is \code{TRUE}, should the axes be 
#' free or fixed? 
#' 
#' @param x_angle Angle of the x-tick (date) labels. If overlapping labels are
#' encountered, setting \code{x_angle} to \code{45} may help. 
#' 
#' @param colour Either a colour of the plot's point and lines, or a variable in
#' \code{df} which the points' and lines' colours will be mapped to. 
#' 
#' @author Stuart K. Grange
#' 
#' @return \strong{ggplot2} plot.
#' 
#' @export
saq_trend_plot <- function(df, df_tests, label = TRUE, round = 3, 
                           y_location = 1, facet_variable = NA, scales = "fixed",
                           colour = "#FCA50A", x_angle = NA) {
  
  # If a list is passed
  if (class(df) == "list" && 
      missing(df_tests) && 
      names(df) %in% c("decomposed", "trend_tests")) {
    
    # The order matters here due to df being overwritten
    df_tests <- df$trend_tests
    df <- df$decomposed
    
  }
  
  if (stringr::str_detect(scales, "free") && !is.na(facet_variable[1])) {
    
    # site_name_country
    df_value_label_y <- df %>% 
      dplyr::group_by_at(facet_variable) %>% 
      summarise(value_label_y = max(trend_and_remainder, na.rm = TRUE)) %>% 
      ungroup()
    
  } else {
    
    value_label_y <- df %>% 
      pull(trend_and_remainder) %>% 
      max(na.rm = TRUE) %>% 
      `*` (y_location)
    
  }
  
  df_labels <- df_tests %>% 
    mutate(date_start = min(date_start),
           date_end = max(date_end),
           date_centre = threadr::date_centre(date_start, date_end),
           significant = if_else(p_value <= 0.05, "*", ""),
           label = stringr::str_c(
             round(slope, round), 
             " [",
             round(slope_lower, round),
             ", ",
             round(slope_upper, round),
             "] ", 
             significant,
             " n = ", n
           ),
           label = stringr::str_trim(label))
  
  if (stringr::str_detect(scales, "free") && !is.na(facet_variable[1])) {
    df_labels <- left_join(df_labels, df_value_label_y, by = facet_variable)
  } else {
    df_labels <- mutate(df_labels, value_label_y = !!value_label_y)
  }
  
  # Build the basic components of plot, some repitition here...
  if (colour %in% names(df)) {
    
    # Requires the use of aes_string
    plot <- ggplot(
      data = df, 
      ggplot2::aes_string("date", "trend_and_remainder", colour = colour)
    ) + 
      geom_line() + 
      geom_point(size = 1.5, pch = 1, na.rm = TRUE)
    
  } else {
    
    # A bit simpler
    plot <- ggplot(data = df, aes(date, trend_and_remainder)) +
      geom_line(colour = colour, na.rm = TRUE) +
      geom_point(size = 1.5, pch = 1, colour = colour, na.rm = TRUE)
    
  }
  
  # Add the extras
  plot <- plot + 
    geom_abline(
      data = df_tests,
      aes(slope = slope / threadr::seconds_in_a_year(), intercept = intercept)
    ) +
    geom_abline(
      data = df_tests,
      aes(
        slope = slope_lower / threadr::seconds_in_a_year(), 
        intercept = intercept_lower
      ),
      linetype = "dashed"
    ) +
    geom_abline(
      data = df_tests,
      aes(
        slope = slope_upper / threadr::seconds_in_a_year(), 
        intercept = intercept_upper
      ),
      linetype = "dashed"
    ) +
    # coord_cartesian(ylim = y_lim) +
    ylim(0, NA) + 
    threadr::theme_less_minimal() + 
    xlab("Date") +
    ylab("Deseasonalised trend")
  
  if (label) {
    
    # Could use geom_label too? 
    plot <- plot +
      geom_text(
        data = df_labels,
        aes(label = label, x = date_centre, y = value_label_y),
        size = 3,
        colour = "black"
      )
    
  }
  
  # Facet
  if (!is.na(facet_variable[1])) {
    plot <- plot + facet_wrap(facet_variable, scales = scales)
  }
  
  # x-axis labels
  if (!is.na(x_angle)) {
    plot <- plot + theme(axis.text.x = element_text(angle = x_angle, hjust = 1))
  }
  
  # Print plot in function
  print(plot)
  
  return(invisible(plot))
  
}
