#' LSOA data
#'
#' A dataset containing 2011 LSOA boundaries for London.
#' Boundaries are not precise and have been simplified to remove around 90\% of points while keeping accuracy of boundaries as high as possible.
#'
#' @format A sf object with 4 variables:
#' \describe{
#' \item{Name}{LSOA (2011) Name}
#' \item{Code}{Official 2011 LSOA ID Code}
#' \item{Description}{Concatenated Name and Code}
#' \item{geometry}{geometry column simplified to remove 90\% of points while maintaining accuracy of boundaries}
#' }
#' @source \url{https://geoportal.statistics.gov.uk/datasets}
"lsoa"
