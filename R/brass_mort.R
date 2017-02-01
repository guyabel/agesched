#' Brass's Relational Model of Mortility
#'
#' Brass' relational model of mortility makes use of a standard model mortility schedule, based on $L_x^s$ that is linearised using
#' \deqn{Y_s(x) = \frac{1}{2} \ln \left( \frac{a l_0 - L_x^s}{L_x^s} \right) }
#' where \eqn{l_0} is the population of the cohort at age zero.
#'
#' The modified transformed schedule is formed by adjusting the demographer's logit above using
#' \deqn{Y(x) = \alpha + \beta Y_s(x)}
#' where \eqn{\beta} changes the slope and \eqn{\alpha} the intercept of the linearised $L_x$
#'
#' The modified transformed schedule is converted back into $L_x$ by applying the anti-function used to linearise the standard schedule.
#'
#' @param model Vector of a `model` age specific person years lived schedule $L_x$. Will be used to derive the standardised transformed age schedule, \eqn{\Y_s(x)} above.
#' @param x Vector for the sequence of ages.
#' @param alpha Numeric value for intercept adjustment to the standardised transformed age schedule
#' @param beta Numeric value for slope adjustment to the standardised transformed age schedule
#'
#' @return Returns vector of $L_x$ from the relational model schedule. The function is primarily intended for creating synthetic survivorships for projection models.
#'
#' The \code{alpha} and \code{beta} parameters relate a modified schedule to the schedule provided by the \code{model} argument.
#'
#' @author Guy J. Abel
#'
#' @export
#'
#' @examples
#' #single year
#' df0 <- subset(austria, Year == 2014)
#' f0 <- df0$Lx_f
#' f1 <- brass_mort(model = f0, x = df0$Age, alpha = 0.8, beta = 1)
#' plot(f0, type = "l")
#' lines(f1, col = "red")
#' e0 <- sum(Lx)/l0
#' e0
brass_mort <- function(model = NULL, x = seq(from = 0, to = 100, by = 1),
                       alpha = 0, beta = 1, l0 = 100000){
  # model = df0$Lx_f; x = df0$x
  a <- unique(diff(x))
  Lx_s <- model
  Yx_s <- 0.5 * log( (a*l0 - Lx_s)/Lx_s )
  Yx <- alpha + beta * Yx_s
  Lx <- a*l0 / (1+exp(2*Yx))
  # T0 <- sum(Lx)
  # e0 <- T0/l0
  return(Lx)
}



# brass_mort <- function(model = NULL, x = seq(from = 0, to = 100, by = 1),
#                        alpha = 0, beta = 1, l0 = 100000){
#   # model <- c(100000, sc$lx[-1]*100000)
#   # x = seq(from = 0, to = 75, by = 5),
#   a <- unique(diff(x))
#   lt <- data_frame(x = x, lx_s = model) %>%
#     mutate(lxp_s = model/model[1],
#            Yx_s = 0.5*log( (1-lxp_s)/lxp_s),
#            Yx = alpha + beta*Yx_s,
#            lxp = 1 / (1+exp(2*Yx)),
#            lx = lxp*model[1],
#            dx = c(abs(diff(lx)), lx[n()]),
#            Lx = a*(lx - dx) + a * 0.5 * dx,
#            sx = lead(Lx)/Lx,
#            sx = ifelse(test = row_number()==n()-1,
#                        yes = lead(Lx) / (lead(Lx) + Lx),
#                        no = sx),
#            Tx = sum(Lx) - cumsum(Lx) + Lx,
#            ex = Tx/lx)
#   return(lt)
# }


