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
#' @param panel_yield A number between 0 and 1 indicating panel yield (dimensionless), representing manufacturer efficiency (typically $\sim 0.2$)
#' @param perform_ratio A number between 0 and 1 indicating performance ratio (dimensionless), accounting for site-specific losses (typically $\sim 0.75$)  
#' @param an_sol_rad A number indicating annual average solar radiation (kWh m$^{-2}$)
#'
#' @returns energy_prod A number indicating energy produced (kWh)
#' @export
#'
#' @examples
#' photovoltaic_syst(area = 100, 
#'                   panel_yield, 
#'                   perform_ratio,
#'                   an_sol_rad = 1000)

panel_yield <- 0.2
perform_ratio <- 0.75

photovoltaic_syst <- function(area, panel_yield, perform_ratio, an_sol_rad) {
  
  energy_prod <- (area * panel_yield * perform_ratio * an_sol_rad)

  return(energy_prod)
}
