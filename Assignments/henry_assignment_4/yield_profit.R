yield_profit <- function(combined_temp_precip, cost = 1500, discount = 0.12, price = 2100) {
  # Create combined table with tmin temp, precip, and yield
  combined_pt_yield <- combined_temp_precip %>%
    mutate(yield = almond_yield(combined_pt_yield$tmin_c, combined_pt_yield$precip))
  
  yield <- combined_pt_yield$yield
  year <- combined_pt_yield$year
  
  # generate a unique identifier or scenario number
  scen <- seq(from = 1, to = length(yield), by = 1) # Sequential number equivalent to length of yield values vector
  yearprofit <- data.frame(scen = scen, yield = yield, year = year)
  yearprofit$revenue <- yearprofit$yield * price # This column is revenue without inflation
  yearprofit <- yearprofit %>%
    mutate(revenue_npv = revenue_NPV(revenue = revenue, time = year - year[1], discount = discount), # NPV adjusted revenue
           cost_npv = cost_NPV(cost = cost, time = year - year[1], discount = discount)) # NPV adjusted cost
  
  # calculate profit
  yearprofit<- yearprofit %>%
    mutate(profit = yearprofit$revenue_npv-yearprofit$cost_npv)
  
  return(yearprofit)
  
}

yield_profit(combined_temp_precip)
