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
#' @param annual_avg_sol_rad A number indicating annual average 
#'                                  solar radiation (kWh m$^{-2}$)
#'
#' @returns A number indicating energy produced (kWh)
#'
#' @examples 
#' photovoltaic_syst(area = 100, 
#'                   an_sol_rad = 1000) 



photovoltaic_syst <- function(area, an_sol_rad) {
  
  r <- 0.2 # panel yield (dimensionless), efficiency  
  PR <- 0.75 # performance ratio (dimensionless), site-specific losses
  
  energy_prod <- (area * r * annual_avg_sol_rad * PR)

  return(energy_prod)
}
