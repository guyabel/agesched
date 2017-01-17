#' Rogers-Castro Eleven Parameter Migration Schedule.
#'
#' Provides a scaled eleven parameter Rogers-Castro schedule,
#' \deqn{ m(x) = a_{1} \exp[-\alpha_{1}x] +
#'   a_{2} \exp [ \alpha_{2}(x-\mu_{2})- \exp [ \lambda_{2}(x-\mu_{2}) ] ] +
#'   a_{3} \exp [ \alpha_{3}(x-\mu_{3})- \exp [ \lambda_{3}(x-\mu_{3}) ] ] +
#'   c}
#' for a given set of parameter values and sequence of ages.
#'
#' @param tot_mig Total migration of the returned age schedule.
#' @param x Sequence of ages.
#' @param a1 Numeric value for height pre-labour force curve.
#' @param alpha1 Numeric value for descent of pre-labour force curve.
#' @param a2 Numeric value for height of labour force curve.
#' @param mu2 Numeric value for peak location of labour force curve.
#' @param lambda2 Numeric value for ascent of labour force curve.
#' @param alpha2 Numeric value for descent of labour force curve.
#' @param a3 Numeric value for height of post-labour force curve.
#' @param mu3 Numeric value for peak location of post-labour force curve.
#' @param lambda3 Numeric value for ascent of post-labour force curve.
#' @param alpha3 Numeric value for descent of post-labour force curve.
#' @param c Numeric value for basic height of all migration curves.
#' @param scaled Boolean value to return age specific migration that scale to sum to \code{tot_mig} or not. Default \code{TRUE}.
#'
#' @return Returns the m(x) values from the Rogers-Castro schedule of age specific migration. The age range for the calculation can take any sequence of positive numbers, such as ages in single or 5-year intervals. The function is primarily intended for use in decomposing an aggregate migration total or rate into an age-specific values. Set \code{scaled = FALSE} if you wish to simply generate an migration age schedule that is not scaled to the aggregate level given by \code{tot_mig}.
#'
#' The parameters relate to the heights (\code{a1}, \code{a2}, \code{a3}, \code{c}), locations (\code{mu2}, \code{mu3}) and slopes (\code{alpha1}, \code{alpha2}, \code{lambda2}, \code{alpha3}, \code{lambda3}).
#'
#' The default values of the parameters match the fundamental parameters of a simplified basic standard schedule migration schedule given in Rogers and Castro (1981). See examples below for non-zero parameters for the post-labour force slope.
#'
#'
#' @references Rogers, A. and Castro. L. J. (1981). Model Migration Schedules. \emph{IIASA Research Report 81} RR-81-30
#' @references Rogers, A., Little, J. and Raymer J. (2010). \emph{The Indirect Estimation of Migration.} Springer Series on Demographic Methods and Population Analysis 26
#' @author Guy J. Abel
#' @seealso \code{\link{rogerscastro7}}, \code{\link{rogerscastro9}}, \code{\link{rogerscastro13}}
#' @examples
#' #single year
#' f0 <- rogerscastro11(tot_mig = 100, x = seq(0, 100, 1),
#'                      a1 = 0.02, alpha1 = 0.1,
#'                      a2 = 0.06, mu2 = 20, alpha2 = 0.1, lambda2 = 0.4,
#'                      a3 = 0.01, alpha3 = 0.1, mu3 = 70, lambda3 = 0.1,
#'                      c = 0.03)
#' plot(f0, type = "l")
#'
#' #five year
#' f1 <- rogerscastro11(tot_mig = 0.05, x = seq(0, 100, 5),
#'                      a1 = 0.02, alpha1 = 0.1,
#'                      a2 = 0.06, mu2 = 25, alpha2 = 0.1, lambda2 = 0.4,
#'                      a3 = 0.01, alpha3 = 0.2, mu3 = 75, lambda3 = 0.1,
#'                      c = 0.05)
#' plot(f1, type = "l")
#' sum(f1)
rogerscastro11 <- function(tot_mig = 1, x = seq(from = 0, to = 100, by = 1),
                           a1 = 0.02, alpha1 = 0.1,
                           a2 = 0.06, mu2 = 20, alpha2 = 0.1, lambda2 = 0.4,
                           a3 = 0, alpha3 = 0, mu3 = 0, lambda3 = 0,
                           c = 0.03,
                           scaled = TRUE){
  m0 <- a1 * exp(-alpha1 * x) +
    a2 * exp(-alpha2 * (x - mu2) - exp(-lambda2*(x - mu2))) +
    a3 * exp(-alpha3 * (x - mu3) - exp(-lambda3*(x - mu3))) +
    c
  m1 <- m0/sum(m0)
  m2 <- m1*tot_mig
  if(scaled == TRUE)
    return(m2)
  if(scaled == FALSE)
    return(m0)
}
