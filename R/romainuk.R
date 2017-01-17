#' Romaniuk Three Parameter Fertility Model Schedule.
#'
#' Provides a scaled three parameter Romaniuk schedule,
#'\deqn{ f(x) = T \left\( 1 + \frac{x}{m-\alpha}            \right\)^\left\{ \frac{(m-\alpha)         [\beta-2(a-\alpha)]}{\beta(a-m)} \right\}
#'                \left\( 1 - \frac{x}{\delta - M + \alpha} \right\)^\left\{ \frac{(\delta - M+\alpha)[\beta-2(a-\alpha)]}{\beta(a-m)} \right\}  }
#' for a given set of parameter values and sequence of ages.
#'
#' @param tfr Total fertility rate of the returned age schedule.
#' @param x Sequence of ages.
#' @param mean_cb Mean age of mothers at child birth.
#' @param mode_cb Modal age of mothers at child birth.
#' @param alpha Lowest age group.
#' @param delta Number of age groups.
#'
#' @return Returns the f(x) values from the Romaniuk schedule of age specific fertility. The age range for the calculation can take any sequence of positive numbers, such as ages in single or 5-year intervals. The function is primarily intended for use in decomposing a total fertility rate into an age-specific values.
#'
#' The parameters relate to the mean and modal central measures (\code{mean_cb}, \code{mode_cb}) and the ages of mothers (\code{alpha}, \code{delta}), which by default are obtained from the ages provided in vector \code{x}.
#'
#' Also known as Mitra’s Pearsonian Type I function or Beta funtion. The adapted version in this function, to allow scaling to a known TFR, was based on IUSSP Demographic Models online section (see \url{http://papp.iussp.org/sessions/papp103_s03/PAPP103_s03_040_070.html} for more details).
#'
#'
#' @references Romaniuk, A. (1973). "A three parameter mode for birth projections", \emph{Population Studies} 27(3):467-478. doi 10.2307/2173766
#' @references Mitra, S. and Romaniuk, A. (1973). `Pearsonian Type I Curve and its Fertility Projection Potentials`. \emph{Demography} 10(3): 351-365. doi 10.2307/2060844
#' @author Guy J. Abel
#' @seealso \code{\link{.}}
#' @examples
#' #single year
#' f0 <- romaniuk(tfr = 3, mean_cb = 27, mode_cb = 26)
#' plot(f0, type = "l")
#'
#' #five year
#' f1 <- romaniuk(tfr = 5.8, x = seq(15, 50, 5), mean_cb = 28, mode_cb = 23)
#' plot(f1, type = "l")
#' sum(f1)
romaniuk <- function(tfr = NULL, x = seq(15, 50, 1),
                     mean_cb = NULL, mode_cb = NULL,
                     alpha = min(x), delta = length(x) - 1){
  if(mean_cb <= mode_cb)
    stop("mean age of child birth must be higher than modal age")
  m1 <- ((mode_cb - alpha) * (delta - 2 * (mean_cb - alpha))) / (delta * (mean_cb - mode_cb))
  m2 <- ((delta - mode_cb + alpha)* (delta - 2 * (mean_cb - alpha))) / (delta * (mean_cb - mode_cb))

  xx <- x - mode_cb
  x1 <- (1 + xx / (mode_cb - alpha)) ^ m1 * (1 - xx / (delta - mode_cb + alpha)) ^ m2
  x1[is.nan(x1)] <- 0
  x2 <- tfr *x1/sum(x1)
  return(x2)
}

