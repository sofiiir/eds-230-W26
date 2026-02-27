#'  Logistic forest growth derivative
#' @param time time since start
#' @param C carbon (C)
#' @param parms - as list with three values, r, K, g
#' @param r intrinsic growth rate (kg/year)
#' @param K carrying capacity (kgC)
#' @param g linear growth rate after canopy closure (kg/year)
#' @param thresh canopy closure threshold (default 50 (kgC))
#' @return derivative of population with time

forestgrowth <- function(time, C, parms, thresh = 50) {
  
  if (C < thresh){
  dC <- parms$r * C
  } else if (C >= thresh){
  dC <- parms$g * (1 - C / parms$K)
}
  return(list(dC))
}
