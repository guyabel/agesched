#' Gage`s Extenstion to Brass` Polynomial Fertility Schedule
#'
#' Provides a scaled Gage fertility schedule,
#' \deqn{ f(x) = c (x - s) (s + w - x) ^ 2}
#' for a given set of parameter values and sequence of ages.
#'
#' @param tfr Numeric value for total fertitliy rate of the returned age schedule.
#' @param x Vector for the sequence of ages.
#' @param start_fertage Numeric value for the start of the fertility age range. Equivalent to \eqn{s} in the equation above.
#' @param width_fertage Numeric value for the width of the fertility age range. Equivalent to \eqn{w} in the equation above.
#' @param c Numeric value for a constant to control the overall fertility level. By default calcuated to approxiamate value in \code{tfr} argument.
#' @param scaled Boolean value to return age specific fertility rates that scale to sum to \code{tfr} or not. Default \code{TRUE}.
#'
#' @return Returns the f(x) values from the Gage extenstion to Brass` schedule of age specific fertility. The age range for the calculation can take any sequence of positive numbers, such as ages in single or 5-year intervals. The function is primarily intended for use in decomposing a total fertility rate into an age-specific values.
#'
#' The parameters relate to the start and width of the fertility age range (\code{start_fertage}, \code{ width_fertage}) and the oveall level (\code{tfr}, \code{c}). By default \code{s} is set to 15 years and \code{w} to 33 as fixed in Brass' original schedule.
#'
#' The adapted version in this function, to allow scaling to a known TFR, was based on IUSSP Demographic Models online section (see \url{http://papp.iussp.org/sessions/papp103_s03/PAPP103_s03_040_040.html} for more details).
#'
#' @references Gage, T. B. (2001). `Age-specific fecundity of mammalian populations: A test of three mathematical models`. \emph{Zoo Biology} 20(6):487-499.
#' @references Brass, W. (1975). \emph{Methods for Estimating Fertility and Mortality from Limited and Defective Data.} Chapel Hill NC: Carolina Population Centre.
#' @author Guy J. Abel
#' @seealso \code{\link{.}}
#' @export
#' @examples
#' #single year
#' f0 <- gage(tfr = 1.8)
#' plot(f0, type = "l")
#'
#' #five year
#' f1 <- gage(tfr = 5.8, x = seq(from = 0, to = 100, by = 5))
#' plot(f1, type = "l")
#' sum(f1)
gage <- function(tfr = NULL, x = seq(from = 0, to = 100, by = 1),
                 start_fertage = 15, width_fertage = 35, c = 12 * w^-4 * tfr,
                 scaled = TRUE){
  s <- start_fertage
  w <- width_fertage
  xx <- seq(from = s, to = s + w, by = unique(diff(x)))
  f0 <- c * (xx - s) * (s + w - xx)^2
  f1 <- rep(0, length(x))
  f1[x %in% xx] <- f0
  f2 <- tfr *f1/sum(f1)
  if(scaled == TRUE)
    return(f2)
  if(scaled == FALSE)
    return(f1)
}



