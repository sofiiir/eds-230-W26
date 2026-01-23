#' Almond Yield Function
#'
#' Calculate almond yields by accounting for their temperature and precipitation specifications. The yield modelling equation was sourced from Lobell et al., 2006.
#' 
#' @param clim_data a dataframe with climate data (ideally should include month, year, minimum temperature, and precipitation)
#' @param month an integer between 1 and 12 representing the month of the temperature and precipitation values to be used in the yield equation (available in the clim_data)
#' @param year an integer signifying the year of the temperature and precipitation values (available in the clim_data)
#' @param min_temp numeric value denoting minimum temperature (C) (available in the clim_data)
#' @param precip numeric value showing precipitation (mm) (available in the clim_data)
#' 
#' @author Sofia Rodas and Henry Oliver
#' 
#' @return dataframe containing minimum temperature in february, precipitation in january, and almond yield (afor the year

almond_yield_df <- function(clim_data, month, year, min_temp, precip) {
    
    # filter for the min temp values in february
    almond_t2 <- clim_data |> 
      group_by(year) |> 
      filter(month == 2) |> 
      summarise(min_temp = min(tmin_c)) |> 
      ungroup()
    
    # set year as factor 
    almond_t2$year <- as.factor(almond_t2$year)
    
    # filter and add precip values for january
    almond_p1 <- clim_data |> 
      group_by(year) |> 
      filter(month == 1) |> 
      summarise(precip = sum(precip)) |> 
      ungroup()
    
    # set year as factor 
    almond_p1$year <- as.factor(almond_p1$year)
    
    # join the two dataframes
    df <- full_join(almond_t2, almond_p1, by = join_by(year))
    
    # make a function to calculate yield based on min_temp (feb) and total precip (jan)
    yield_fun <- function(min_temp, precip){
      yield  = ((-0.015 * min_temp) - 
                  (0.0046 * (min_temp^2)) - 
                  (0.07 * precip) + 
                  (0.0043 * (precip^2)) +
                  0.28
      )}
    
    # iterate over the min_temp and precip columns to get the yield function
    df$almond_yields <-  map2(df$min_temp, df$precip, yield_fun)
    
   
  return(df)
  }