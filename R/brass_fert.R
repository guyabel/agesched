#' Brass's Relational Gompertz Model of Fertility
#'
#' @param tfr Numeric value for total fertitliy rate of the returned age schedule.
#' @param x Vector for the sequence of ages.
#' @param model Vector of a `model` age specific fertility rates.
#' @param alpha
#' @param beta
#' @param start_fertage Numeric value for the start of the fertility age range.
#' @param width_fertage Numeric value for the width of the fertility age range.
#' @param model_age String value to indicate if model is for fertility age range only or all ages.
#'
#' @return
#' @export
#'
#' @examples
brass_fert <- function(tfr = NULL, x = seq(from = 0, to = 100, by = 1),
                       model = NULL, alpha = 0, beta = 1,
                       start_fertage = 15, width_fertage = 35, model_ages = "auto"){
  if(!(model_ages %in% c("auto", "fertage", "all")))
    stop("model_ages must be set to either `auto`, `all` or `fertage`")
  s <- start_fertage
  w <- width_fertage
  a <- unique(diff(x))
  xx <- seq(from = s, by = a, length.out = w/a)

  # f0 <- model
  f1 <- f0/sum(f0)
  F0 <- cumsum(f1)
  F1 <- -log(-log(F0))
  F2 <- alpha + beta*F1
  F3 <- exp(-exp(-F2))
  f2 <- c(0,diff(F3)*tfr)

  f3 <- NULL
  if(model_ages == "auto"){
    if(length(model)>50 & a == 1)
      f3 <- f2
    if(length(model)>9 & a == 5)
      f3 <- f2
  }
  if(is.null(f1) | model_ages == "fertage"){
    f3 <- rep(0, length(x))
    f3[x %in% xx] <- f2
  }
  return(f3)
}
