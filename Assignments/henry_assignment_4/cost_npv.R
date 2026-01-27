#' Calculate cost when accounting for net present value
#'
#' @param cost fixed cost at time 0
#' @param time vector of time points
#' @param discount fixed discount rate, usually 0.12
#'
#' @returns
#' @export
#'
#' @examples

cost_npv_function <- function(cost, time, discount = 0.12) {
  result <- cost / (1 + discount)**time
  return(result)
}
