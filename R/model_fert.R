#' Model Fertility Model Schedule.
#'
#' Scales a `model`` fertility schedule to match a given TFR.
#'
#' @param tfr Numeric value for total fertitliy rate of the returned age schedule.
#' @param x Vector for the sequence of ages.
#' @param model Vector of a `model` age specific fertility rates.
#' @param start_fertage Numeric value for the start of the fertility age range.
#' @param width_fertage Numeric value for the width of the fertility age range.
#' @param model_age String value to indicate if model is for fertility age range only or all ages.
#'
#' @return Returns the f(x) values from a model schedule of age specific fertility. The age range for the calculation can take any sequence of positive numbers, such as ages in single or 5-year intervals. The function is primarily intended for use in decomposing a total fertility rate into an age-specific values.
#'
#' The arguments for the start and width of the fertility age range (\code{start_fertage}, \code{width_fertage}) are used to select where the model distribution is applied over the range of ages given in \code{x}. Ensure that these match those of the model age schedule.
#'
#' If the model schedule covers both fertility and non-fertility age ranges, set \code{model_age} to \code{all} to ignore the values passed to \code{start_fertage}, \code{start_fertage}, \code{x}. The \code{auto} option attempts to guess, based on the length of the vetor passed to \code{model} and the age groups used in \code{x}.
#'
#' @author Guy J. Abel
#' @seealso \code{\link{gage}} \code{\link{romainuk}} \code{\link{hadwiger}}
#' @export
#' @examples
#' #single year
#' f0 <- subset(austria, Year == 2014)$Fx
#' plot(f0, type = "l")
#' sum(f0)
#' f1 <- model_fert(tfr = 2.1, model = f0)
#' sum(f1)
#' plot(f1, type = "l")
#'
#' #five year
#' f1 <- model_fert(tfr = 3, x = seq(from = 0, to = 100, by = 5), model = un1956$high)
#' plot(f1, type = "l")
model_fert <- function(tfr = NULL, x = seq(from = 0, to = 100, by = 1),
                       model = NULL,
                       start_fertage = 15, width_fertage = 35, model_ages = "auto"){
  if(!(model_ages %in% c("auto", "fertage", "all")))
    stop("model_ages must be set to either `auto`, `all` or `fertage`")
  s <- start_fertage
  w <- width_fertage
  a <- unique(diff(x))
  xx <- seq(from = s, by = a, length.out = w/a)

  f0 <- model

  f1 <- NULL
  if(model_ages == "auto"){
    if(length(model)>50 & a == 1)
      f1 <- f0
    if(length(model)>9 & a == 5)
      f1 <- f0
  }
  if(is.null(f1) | model_ages == "fertage"){
    f1 <- rep(0, length(x))
    f1[x %in% xx] <- f0
  }

  f2 <- tfr * f1/sum(f1)
  return(f2)
}
