#' Romaniuk’s Three-Parameter Fertility Model Schedule.
#'
#' Returns a fertility age schedule using the paramaterisation of
#' Romainuk (1973) scaled to have a total fertitliy rate
#' (T) matching the \code{tfr} argument;
#'\deqn{ f(x) = T \left\( 1 + \frac{x}{M-\alpha}            \right\)^\left\{ \frac{(M-\alpha)         [\beta-2(A-\alpha)]}{\beta(A-M)} \right\}
#'                \left\( 1 - \frac{x}{\delta - M + \alpha} \right\)^\left\{ \frac{(\delta - M+\alpha)[\beta-2(A-\alpha)]}{\beta(A-M)} \right\}  }
#'
#' @param tfr Numeric value for total fertitliy rate of the returned age
#'   age schedule
#' @param A Numeric value for average age parameter.
#' @param M Numeric value for modal age parameter.
#' @param alpha Numeric value for start of fertile age range.
#' @param beta Numeric value for duration of fertile age range.
#' @param ag Numeric value for age groupings. \code{1} year age groups used by default.
#' @return A vector for the age specific fertility rates generated from the
#'   Romaniuk age schedule. See
#'   \url{http://papp.iussp.org/sessions/papp103_s03/PAPP103_s03_040_070.html} for more details.
#' @references Romaniuk, Anatole (1973). "A three parameter mode for birth projections",
#'  \emph{Population Studies} 27(3):467-478. doi 10.2307/2173766
#'
#' @examples
#' #single year
#' f0 <- romaniuk(tfr = 1.8, A = 28.2, M = 26, alpha = 15, delta = 33, a = 1)
#' plot(f0)
#'
#' #five year
#' f1 <- romaniuk(tfr = 5.8, A = 27, M = 23, alpha = 15, delta = 35, a = 5)
#' plot(f1)
#' @author Guy J. Abel
#' @seealso \code{\link{.}}

romaniuk <- function(tfr = 2.1, A = NULL, M = NULL, alpha = NULL, delta = NULL, ag = 1){
  m1 <- ((M - alpha) * (delta - 2 * (A - alpha))) / (delta * (A - M))
  m2 <- ((delta - M + alpha)* (delta - 2 * (A - alpha))) / (delta * (A - M))

  x <- seq(from = alpha, to = alpha + delta, by = ag) - M
  x1 <- (1 + x / (M - alpha)) ^ m1 * (1 - x / (delta - M + alpha)) ^ m2
  x1[is.nan(x1)] <- 0
  x2 <- tfr *x1/sum(x1)
  return(x2)
}
