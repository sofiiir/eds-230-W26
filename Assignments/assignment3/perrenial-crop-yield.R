#' Perrenial Crop Yield
#'
#' Calculate perrenial crop yeilds by accounting for their temperature and precipitation specifications.
#' @param clim_data dataframe with climate data
#' @param crop crop of interest (select from wine grapes, almonds, table grapes, oranges, walnuts, avocados)
#' @param min_temp minimum temperature (C)
#' @param max_temp maximum temperature (C)
#' @param precip precipication (mm)
#' @author Sofia
#' @return perrenial crop yield

crop_yield <- function(clim_data, crop, month, year, min_temp, max_temp, precip) {
  if (crop == "almond") {
   almond_t2 <- clim_data |> 
                filter(month == 2) |> 
                summarise(min_temp = min(tmin_c))
   
   almond_p1 <- clim_data |> 
                filter(month == 1) |> 
                summarise(precip = sum(precip))
   
   map( ~ yield <- (-0.015 * almond_t2$min_temp) - (-0.0046 * (almond_t2$min_temp^2)) - (0.07 * almond_p1$precip) + (0.0043 * (almond_p1$precip^2)))
      
  } else {
    if (etype == "direct") {
      solar$total <- solar$Kdown_direct
    } else {
      solar$total <- solar$Kdown_direct + solar$Kdown_diffuse
    }
  }
  
  }