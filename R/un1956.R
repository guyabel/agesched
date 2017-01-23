#' Model Fertility Age Schedules from the United Nations (1956)
#'
#' Emperical model age schdules by five-year age groups, based on the average shape of fertility distributions in 52 countries, 15 with comparatively high fertility (although the country with the highest fertility had a TFR of just over 5 children per woman), and 37 with relatively low fertility.
#'
#' @source Taken from \url{http://papp.iussp.org/sessions/papp103_s03/PAPP103_s03_040_020.html}.
#' @format A data frame with columns:
#' \describe{
#'  \item{age_group}{Five year age group.}
#'  \item{combined}{ASFR over all countries scaled to a TFR of 0.2.}
#'  \item{high}{ASFR in high fertility countries scaled to  a TFR of 0.2.}
#'  \item{low}{ASFR in low fertility countries scaled to  a TFR of 0.2.}
#' }
#' @examples
#' un1956
"un1956"
