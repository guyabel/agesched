#' Brass's Relational Model of Mortility
#'
#' Brass' relational Gompertz model of mortility makes use of a model mortility schedule that is linearised using
#' \deqn{Y_s(x) = \frac{1}{2} \ln \left( \frac{l^{*}_x}{1-l^{*}_x} \right) }
#' where \eqn{l^{*}_x} is the proportions surviving at exact age x, i.e. the $l_x$ from the life table scaled to sum to a radix of 1.
#'
#' The modified transformed schedule is formed by adjusting the demographer's logit above using
#' \deqn{Y(x) = \alpha + \beta Y_s(x)}
#' where \eqn{\beta} changes the slope and \eqn{\alpha} the intercept of the linearised $l_x$
#'
#' The modified transformed schedule is converted back into $l_x$ by applying the anti-function used to linearise the standard schedule and multiplying by original radix of $l_x$
#'
#' @param model Vector of a `model` age specific mortility rates. Will be used to derive the standardised transformed age schedule, \eqn{\Y_s(x)} above.
#' @param x Vector for the sequence of ages.
#' @param alpha Numeric value for intercept adjustment to the standardised transformed age schedule
#' @param beta Numeric value for slope adjustment to the standardised transformed age schedule
#'
#' @return Returns a life table with values from a relational model schedule of age specific mortility. The age range for the calculation can take any sequence of positive numbers, such as ages in single or 5-year intervals. The function is primarily intended for creating synthetic mortility rate into an age-specific values.
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
#' df0$lx_f <- df0$Lx_f + df0$Dx_f/2
#' f0 <- df0$lx_f
#'
#' f1 <- brass_mort(model = f0, x = seq(from = 0, to = 110, by = 1), alpha = 0, beta = 1)
#' plot(f0, type = "l")
#' lines(f1$lx, col = "red")
#' f1$ex[1]
brass_mort <- function(model = NULL, x = seq(from = 0, to = 100, by = 1),
                       alpha = 0, beta = 1, l0 = 100000){
  # model <- c(100000, sc$lx[-1]*100000)
  # x = seq(from = 0, to = 75, by = 5),
  a <- unique(diff(x))
  lt <- data_frame(x = x, lx_s = model) %>%
    mutate(lxp_s = model/model[1],
           Yx_s = 0.5*log( (1-lxp_s)/lxp_s),
           Yx = alpha + beta*Yx_s,
           lxp = 1 / (1+exp(2*Yx)),
           lx = lxp*model[1],
           dx = c(abs(diff(lx)), lx[n()]),
           Lx = a*(lx - dx) + a * 0.5 * dx,
           sx = lead(Lx)/Lx,
           sx = ifelse(test = row_number()==n()-1,
                       yes = lead(Lx) / (lead(Lx) + Lx),
                       no = sx),
           Tx = sum(Lx) - cumsum(Lx) + Lx,
           ex = Tx/lx)
  return(lt)
}

