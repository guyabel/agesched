#' Romaniuk Fertility Model Schedule.
#'
#' Provides a scaled Romaniuk fertility schedule,
#'\deqn{ f(x) = f(x) = \left( 1 + \frac{x}{m-\alpha} \right) ^\left\{ \frac{(M-\alpha) [\beta-2(A-\alpha)]}{\beta(A-M)} \right\}\left( 1 - \frac{x}{\delta - M + \alpha} \right)^\left\{ \frac{(\delta - M+\alpha)[\delta-2(A-\alpha)]}{\delta(A-M)} \right\}  }
#' for a given set of parameter values and sequence of ages.
#'
#' @param tfr Total fertility rate of the returned age schedule. Equivalent to \eqn{\T} in the equation above.
#' @param x Vector for the sequence of ages.
#' @param mean_cb Numeric value for the mean age of mothers at child birth. Equivalent to \eqn{a} in the equation above.
#' @param mode_cb Numeric value for the modal age of mothers at child birth. Equivalent to \eqn{m} in the equation above.
#' @param start_fertage Numeric value for the start of the fertility age range. Equivalent to \eqn{\alpha} in the equation above.
#' @param width_fertage Numeric value for the width of the fertility age range. Equivalent to \eqn{\delta} in the equation above.
#' @param scaled Boolean value to return age specific fertility rates that scale to sum to \code{tfr} or not. Default \code{TRUE}.
#'
#' @return Returns the f(x) values from the Romaniuk schedule of age specific fertility. The age range for the calculation can take any sequence of positive numbers, such as ages in single or 5-year intervals. The function is primarily intended for use in decomposing a total fertility rate into an age-specific values.
#'
#' The parameters relate to the mean and modal central measures (\code{mean_cb}, \code{mode_cb}) and the ages of mothers at child birth (\code{start_cb}, \code{width_cb}).
#'
#' Also known as Mitra`s Pearsonian Type I function or Beta funtion. The adapted version in this function, to allow scaling to a known TFR, was based on IUSSP Demographic Models online section (see \url{http://papp.iussp.org/sessions/papp103_s03/PAPP103_s03_040_070.html} for more details).
#'
#'
#' @references Romaniuk, A. (1973). "A three parameter mode for birth projections", \emph{Population Studies} 27(3):467-478. doi 10.2307/2173766
#' @references Mitra, S. and Romaniuk, A. (1973). `Pearsonian Type I Curve and its Fertility Projection Potentials`. \emph{Demography} 10(3): 351-365. doi 10.2307/2060844
#' @author Guy J. Abel
#' @seealso \code{\link{.}}
#' @export
#' @examples
#' #single year
#' f0 <- romaniuk(tfr = 3, mean_cb = 27, mode_cb = 26)
#' plot(f0, type = "l")
#'
#' #five year
#' f1 <- romaniuk(tfr = 5.8, x = seq(0, 100, 5), mean_cb = 28, mode_cb = 23)
#' plot(f1, type = "l")
#' sum(f1) * 5
romaniuk <- function(tfr = NULL, x = seq(from = 0, to = 100, by = 1),
                     mean_cb = NULL, mode_cb = NULL,
                     start_fertage = 15, width_fertage = 35,
                     scaled = TRUE){
  if(mean_cb <= mode_cb)
    stop("mean age of child birth must be higher than modal age")
  #start_fertage = 15; width_fertage = 35; mean_cb = 30; mode_cb = 25; tfr = 2.1; x = seq(from = 0, to = 100, by = 5)
  alpha <- start_fertage
  delta <- width_fertage
  m1 <- ((mode_cb - alpha) * (delta - 2 * (mean_cb - alpha))) / (delta * (mean_cb - mode_cb))
  m2 <- ((delta - mode_cb + alpha)* (delta - 2 * (mean_cb - alpha))) / (delta * (mean_cb - mode_cb))

  a <- unique(diff(x))
  xx0 <- seq(from = alpha, to = alpha + delta, by = a)
  xx <- xx0 - mode_cb
  if(a>1)
    xx <- xx + a/2

  f0 <- (1 + xx / (mode_cb - alpha)) ^ m1 * (1 - xx / (delta - mode_cb + alpha)) ^ m2
  f0[is.nan(f0)] <- 0
  f0[is.infinite(f0)] <- 0

  f1 <- rep(0, length(x))
  f1[x %in% xx0] <- f0

  f2 <- tfr * f1/sum(f1) * 1/a
  if(scaled == TRUE)
    return(f2)
  if(scaled == FALSE)
    return(f0)
}
