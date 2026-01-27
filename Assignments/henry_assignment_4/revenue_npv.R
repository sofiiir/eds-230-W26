#' Compute revenue when accounting for net present value
#'
#' compute net present value for revenue
#' @param revenue ($)
#' @param time in the future that cost/value occurs (years)
#' @param discount rate
#' @return value in $

revenue_npv_function <- function(revenue, time, discount = 0.12) {
  result <- revenue / (1 + discount)**time
  return(result)
}