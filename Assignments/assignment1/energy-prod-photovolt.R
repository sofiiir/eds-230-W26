##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                           ~~
##        Energy Produced from a Photovolatic System Function                        ----
##                                                                           ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Energy Produced from a Photovolatic System
#'
#' @param area A number indicating solar panel area (m$^2$)
#' @param an_sol_rad A number indicating annual average solar radiation (kWh m$^{-2}$)
#'
#' @returns energy_prod A number indicating energy produced (kWh)
#' @export
#'
#' @examples
#' photovoltaic_syst(area = 100, 
#'                   an_sol_rad = 1000)



photovoltaic_syst <- function(area, an_sol_rad) {
  
  r <- 0.2 # panel yield (dimensionless) representing manufacturer
  H <- 0.75 # performance ratio (dimensionless) accounting for site-specific losses
  
  energy_prod <- (area * r * H * an_sol_rad)

  return(energy_prod)
}
