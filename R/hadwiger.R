#' Hadwiger Fertility Model Schedule.
#'
#' Provides a scaled Hadwiger fertility schedule,
#'\deqn{ f(x) = \frac{H}{T \sqrt{\pi}}  \left\( \frac{T}{x - d} \right\) ^ \frac{3}{2}
#'\exp \left\{-H^2 \left\(\frac{T}{x-d} \frac{x-d}{T} -2\right\) \right\} }
#' for a given set of parameter values and sequence of ages.
#'
#' @param tfr Numeric value for total fertitliy rate of the returned age schedule.
#' @param x Vector for the sequence of ages.
#' @param H Numeric value for the \eqn{H} parameter in the equation above.
#' @param T Numeric value for the \eqn{T} parameter in the equation above.
#' @param d Numeric value for the \eqn{d} parameter in the equation above.
#' @param start_fertage Numeric value for the start of the fertility age range.
#' @param width_fertage Numeric value for the width of the fertility age range.
#' @param scaled Boolean value to return age specific fertility rates that scale to sum to \code{tfr} or not. Default \code{TRUE}.
#'
#' @return Returns the f(x) values from the Hadwiger schedule of age specific fertility, later refined by Gilje and Yntema. The age range for the calculation can take any sequence of positive numbers, such as ages in single or 5-year intervals. The function is primarily intended for use in decomposing a total fertility rate into an age-specific values.
#'
#' The parameters relate to the shape of the schedule (\code{H}, \code{T} and \code{d}). The shape parameters have no demographic interpretion.
#'
#' The arguments for the start and width of the fertility age range (\code{start_fertage}, \code{width_fertage}) are used to select where the distribution is applied over the range of ages given in \code{x}.
#'
#' The adapted version in this function, to allow scaling to a known TFR, was based on IUSSP Demographic Models online section (see \url{http://papp.iussp.org/sessions/papp103_s03/PAPP103_s03_040_090.html} for more details).
#'
#' @references Gilje E. and Yntema L. (1970). `The shifted Hadwiger fertility function`. Working Paper No IO70/14. \emph{Oslo: Central Bureau of Statistics of Norway.}
#' @references Hadwiger H. (1940). Eine analytische Reprodutionsfunktion fur biologische Gesamtheiten. \emph{Skandinavisk Aktuarietidskrift} 23(101-113)
#' @author Guy J. Abel
#' @seealso \code{\link{.}}
#' @export
#' @examples
#' #single year
#' f0 <- hadwiger(tfr = 1.8, H = 4, T = 28, d = 2)
#' plot(f0, type = "l")
#'
#' #five year
#' f1 <- hadwiger(tfr = 5.8, x = seq(from = 0, to = 100, by = 5), H = 3, T = 26, d = 2, width_fertage = 40)
#' plot(f1, type = "l")
#' sum(f1)
hadwiger <- function(tfr = NULL, x = seq(from = 0, to = 100, by = 1),
                     H = NULL, T = NULL, d = NULL,
                     start_fertage = 15, width_fertage = 35,
                     scaled = TRUE){

  s <- start_fertage
  w <- width_fertage
  xx <- seq(from = s, to = s + w, by = unique(diff(x)))

  f0 <- H /(T*sqrt(pi)) * ((T/(xx - d)) ^ 1.5) * exp(-(H^2) * (((T^2 + (xx - d)^2)/(T*(xx-d)))-2))

  f1 <- rep(0, length(x))
  f1[x %in% xx] <- f0

  f2 <- tfr * f1/sum(f1)
  if(scaled == TRUE)
    return(f2)
  if(scaled == FALSE)
    return(f0)
}

