#' visualize airport delays
#'
#' @description Solution to 1.1.5
#'
#' @return ggplot2
#' 
#' @import nycflights13
#' @import tidyr
#' @import ggplot2
#' @import dplyr
#' @export 




visualize_airport_delays <- function(){
  df = nycflights13::flights %>% select(arr_delay, dest) %>% group_by(dest) %>% 
    summarise(mean_arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
    left_join(nycflights13::airports %>% select(faa, name, lat, lon), by = c("dest" = "faa"))  %>% drop_na() 
  
  
  my_plot = ggplot(data = df, aes(x = lon, y = lat, color = mean_arr_delay)) + geom_point() + 
      labs(title = "Mean Arrival Delay of Flights", x= "Longitude", y = "Latitude", color = "Mean")
  return(my_plot)
}

