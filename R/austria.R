#' Austrian Demographic Data
#'
#' Gender age specific popoulation, mortality and fertitly rates from 1947 to 2015
#'
#' @source \url{http://www.mortality.org/} and \url{http://www.humanfertility.org/}, downloaded 2016-04-20.
#' @format A data frame with columns:
#' \describe{
#'  \item{Year}{Year.}
#'  \item{Age}{Age group.}
#'  \item{Nx_f,Nx_m}{Female and male population sizes.}
#'  \item{Lx_f,Lx_m}{Female and male person years lived.}
#'  \item{Fx}{Age specific fertitliy rate.}
#' }
#' @examples
#' head(austria)
"austria"
