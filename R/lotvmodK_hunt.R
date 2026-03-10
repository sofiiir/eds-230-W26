#' Lot. Voltera Model
#'
#' function computes the rate of change of populations in a predictor prey interaction
#' @param t  time (days)
#' @param pop datatype list initial conditions; list with two values prey=number of prey and pred=number of predictor
#' @param pars datatype list  coefficient in Lotka-Voltera pars$rprey, pars$alpha, pars$eff, par$pmort
#' @param prey_thresh numeric value of the predator threshold necessary for hunting to be initiated
#'  \emph{rhunt} is the hunting rate of prey population;
#'  \emph{rprey} is growth rate of prey population;
#'  \emph{eff} is the rate of ingestion of prey by predators
#'  \emph{alpha} is a interaction coefficient (higher values greater interaction
#'  \emph{pmort}  mortality rate of predictor population
#' @examples
#' lotvod(t = 1, pop = list(1, 2), pars = c(rhunt = 0.1,
#'                                          rprey = 0.95, 
#'                                          eff = 0.6, 
#'                                          alpha = 0.01,
#'                                          pmort = 0.4))
#'
#' pars = c(rhunt = 0.1,
#'          rprey = 0.95, 
#'          eff = 0.6, 
#'          alpha = 0.01,
#'          pmort = 0.4)
#' currpop <- c(prey = 1, pred = 1)
#  days = seq(from=1,to=20)
#' res <- ode(func = lotvmodK_hunt, y = currpop, times = days, parms = pars)
#'
#' @return  lotvmod returns a list containing the following components
#' \describe{
#' \item{dprey}{rate of change of prey populutation}
#' \item{dpred}{rate of change of preditor populutation}
#' }

lotvmodK_hunt <- function(t, pop, pars, prey_thresh = 150) {
  with(as.list(c(pars, pop)), {
    
    
    # hunt only if there are more prey than the threshold
    hunt <- ifelse (prey > prey_thresh, rhunt * prey, 0) 
    
    # change in prey equation 
    dprey <- (rprey * (1 - prey / K) * prey - alpha * prey * pred) - hunt
    
    # change in predator equation 
    dpred <- (eff * alpha * prey * pred) - (pmort * pred)
  
    
    return(list(c(dprey, dpred)))
  })
}
