#'  Logistic forest growth derivative
#' @param time time since start
#' @param C Canopy closure threshold 
#' @param parms - as list with two values, r, K, g
#' @param r intrinsic growth rate
#' @param K carrying capacity (kg C)
#' @param g linear growth rate after canopy closure
#' @param thresh canopy closure threshold (default 50 (kgC))
#' @return derivative of population with time

forestgrowth <- function(Time, C, parms, thresh = 50) {
  
  if (thresh < 50){
  dC <- parms$r * C
  } else if (thresh >= 50){
  dC <- parms$g * (1 - C / parms$K)
}
  return(list(dC))
}
