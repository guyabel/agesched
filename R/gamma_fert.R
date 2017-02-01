#' Gamma Fertility Model Schedule.
#'
#' Provides a scaled gamma fertility schedule,
#'\deqn{ f(x) = \frac{1}{\Gamma(k)\theta^k} x^{(k-1)}e^{-\frac{x}{\theta}} }
#' for a given set of parameter values and sequence of ages.
#'
#' @param tfr Numeric value for total fertitliy rate of the returned age schedule.
#' @param x Vector for the sequence of ages.
#' @param mean_cb Numeric value for the mean age of mothers at child birth. Equivalent to \eqn{k\theta}.
#' @param shape Numeric poisitive value for schedule shape. Used direcly in \code{\link{dgamma}}.
#' @param start_fertage Numeric value for the start of the fertility age range. Equivalent to \eqn{\alpha} in the equation above.
#' @param width_fertage Numeric value for the width of the fertility age range. Equivalent to \eqn{\delta} in the equation above.
#' @param ... Additional arguments passed to \code{\link{dgamma}}
#'
#' @return Returns the f(x) values from a gamma schedule of age specific fertility. The age range for the calculation can take any sequence of positive numbers, such as ages in single or 5-year intervals. The function is primarily intended for use in decomposing a total fertility rate into an age-specific values.
#'
#' The parameters relate the mean age of child bearing (\code{mean_cb}) and the shape of the schedule (\code{shape}). The shape and mean age parameters are used to determine the scale parameter of the gamma distribution.
#'
#' The arguments for the start and width of the fertility age range (\code{start_fertage}, \code{width_fertage}) are used to select where the gamma distribution is applied over the range of ages given in \code{x}.
#'
#' @author Guy J. Abel
#' @seealso \code{\link{gage}} \code{\link{romainuk}} \code{\link{hadwiger}}
#' @export
#' @examples
#' #single year
#' f0 <- gamma_fert(tfr = 4, mean_cb = 25, shape = 5)
#' plot(f0, type = "l")
#'
#' #five year
#' f1 <- gamma_fert(tfr = 5.8, x = seq(from = 0, to = 100, by = 5), shape = 3)
#' plot(f1, type = "l")
#' sum(f1)
gamma_fert <- function(tfr = NULL, x = seq(from = 0, to = 100, by = 1),
                       mean_cb = NULL, shape = NULL,
                       start_fertage = 15, width_fertage = 35, ...){
  s <- start_fertage
  w <- width_fertage
  a <- unique(diff(x))
  xx <- seq(from = s, to = s + w, by = a)

  k <- shape
  theta <- (mean_cb-s) / k
  theta <- theta / a
  f0 <- dgamma(1:length(xx), shape = k, scale = theta, ...)

  f1 <- rep(0, length(x))
  f1[x %in% xx] <- f0

  f2 <- tfr * f1/sum(f1)
  return(f2)
}
