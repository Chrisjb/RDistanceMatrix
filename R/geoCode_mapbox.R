#' Geocode a location using the mapbox geocode API.
#'
#' The function geocodes a single address given as a character string. Be as specific as possible, including the country or postcode to avoid ambiguity.
#' To avoid loss of accuracy when geocoding a lat/lng string, the geoCode function does not geocode lat/lng strings but instead returns the same string as a dataframe.
#'
#' @param address character string with address to be geocoded. Be as specific as possible to avoid ambiguity.
#' @param api_key character string containing api key for the google maps geocode api. You can generate one for free in your google cloud console.
#' @param return_all logical value. Some locations may return more than one geocoded result. If you only want one row returned set return_all = FALSE.
#'
#' @return a data.frame containing the coordinates, location type and approximate address.
#'
#' @importFrom dplyr mutate rename
#' @importFrom rlang .data
#' @importFrom glue glue
#' @importFrom utils URLencode
#' @importFrom sf st_coordinates read_sf st_drop_geometry
#'
#' @examples
#' geocode_mapbox('London Bridge, London, UK', return_all = FALSE)
#'
#' #  lat        lng             type                           address
#' #  51.50788 -0.0877321 GEOMETRIC_CENTER London Bridge, London SE1 9RA, UK
#'
#' @export



geocode_mapbox <- function(address, api_key=Sys.getenv('mapbox_api_key'),return_all = TRUE) {
  # if address is in lat lng form, do not geocode.
  if(isLatLng(address)){
    return(data.frame(lat=getLat(address),lng=getLng(address),type=NA, address='User Defined Location'))
  } else if(is.na(address)){
      return(data.frame(lat=NA,lng=NA,type=NA, address=NA))
  } else {
    u <- glue::glue('https://api.mapbox.com/geocoding/v5/mapbox.places/{address}.json?access_token={api_key}')
    u_secret <- gsub(api_key,'SECRET',u)
    message(glue::glue('geocoding url: {utils::URLencode(u_secret)}'))
    place <- sf::read_sf(utils::URLencode(u)) %>%
      cbind(sf::st_coordinates(.$geometry)) %>%
      dplyr::rename(lng = .data$X, lat = .data$Y) %>%
      sf::st_drop_geometry()

    lat <- place$lat
    lng <- place$lng
    type  <- place$id

      if(return_all == TRUE){
        return(data.frame(lat, lng, type))
      } else {
        return(data.frame(lat, lng, type)[1,])
      }
    }
}

