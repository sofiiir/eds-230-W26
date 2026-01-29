
#' Almond Profit Function
#'
#' @param yield_df dataframe output from almond_yield() containing year, min_temp, precip, and almond_yields
#' @param base_price a numeric value indicating price per ton of almonds in 1989 dollars ($USD)
#' @param base_cost a numeric value indicating annual fixed cost of the almond farmers in 1989 dollars ($USD)
#' @param inflation_rate a numeric value indicating annual inflation rate, default 0.03 
#' @param discount_rate a numeric value indicating discount rate for NPV calculation, default 0.12
#'
#'@author Henry Oliver and Sofia Rodas
#' @return dataframe with columns: year, min_temp, precip, almond_yields, time, price($USD), cost($USD), revenue($USD), profit($USD), revenue_NPV ($USD), cost_NPV($USD), profit_NPV ($USD)


almond_profit <- function(yield_df, base_price, base_cost, inflation_rate = 0.03, discount_rate = 0.12) {
  
  # Convert year from factor to numeric and unnest yields
  df <- yield_df |>
    mutate(year = as.numeric(as.character(year)),
           almond_yields = unlist(almond_yields))
  
  # Calculate time offset from 1989
  df$time <- df$year - 1989
  
  # Inflate base values
  df$price <- base_price * (1 + inflation_rate)^df$time
  df$cost <- base_cost * (1 + inflation_rate)^df$time
  
  # Calculate actual economics
  df$revenue <- df$almond_yields * df$price
  df$profit <- df$revenue - df$cost
  
  # Apply NPV
  df$revenue_npv <- revenue_npv_function(df$revenue, df$time, discount_rate)
  df$cost_npv <- cost_npv_function(df$cost, df$time, discount_rate)
  df$profit_npv <- (df$revenue_npv - df$cost_npv)
  
  return(df)
}