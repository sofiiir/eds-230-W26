#' Perrenial Crop Yield
#'
#' Calculate perrenial crop yeilds by accounting for their temperature and precipitation specifications.
#' @param clim_data dataframe with climate data
#' @param month month of the temp and precip values (available in the clim_data)
#' @param year year of the temp and precip values (available in the clim_data)
#' @param min_temp minimum temperature (C) (available in the clim_data)
#' @param max_temp maximum temperature (C) (available in the clim_data)
#' @param precip precipitation (mm) (available in the clim_data)
#' @author Sofia
#' @return perrenial crop yield

crop_yield <- function(clim_data, month, year, min_temp, max_temp, precip) {
    
    # select the min temp values in february
    almond_t2 <- clim_data |> 
      group_by(year) |> 
      filter(month == 2) |> 
      summarise(min_temp = min(tmin_c)) |> 
      ungroup()
    
    # set year as factor 
    almond_t2$year <- as.factor(almond_t2$year)
    
    # add the precip values for january
    almond_p1 <- clim_data |> 
      group_by(year) |> 
      filter(month == 1) |> 
      summarise(precip = sum(precip)) |> 
      ungroup()
    
    # set year as factor 
    almond_p1$year <- as.factor(almond_p1$year)
    
    # join the two dataframes
    df <- full_join(almond_t2, almond_p1, by = join_by(year))
    
    # make a function to calculate yield based on min_temp and total precip
    yield_fun <- function(min_temp, precip){
      yield  = ((-0.015 * min_temp) - 
                  (0.0046 * (min_temp^2)) - 
                  (0.07 * precip) + 
                  (0.0043 * (precip^2)) +
                  0.28
      )}
    
    # iterate over the min_temp and precip columns to get the yield function
    df$almond_yields <-  map2(df$min_temp, df$precip, yield_fun)
   
  
  }