#' Geocode a location using the mapbox geocode API.
#'
#' The function geocodes a single address given as a character string. Be as specific as possible, including the country or postcode to avoid ambiguity.
#' To avoid loss of accuracy when geocoding a lat/lng string, the geoCode function does not geocode lat/lng strings but instead returns the same string as a dataframe.
#'
#' @param address character string with address to be geocoded. Be as specific as possible to avoid ambiguity.
#' @param api_key character string containing api key for the google maps geocode api. You can generate one for free in your google cloud console.
#' @param verbose logical value. If TRUE, echos the input address
#' @param return_all logical value. Some locations may return more than one geocoded result. If you only want one row returned set return_all = FALSE.
#'
#' @return a data.frame containing the coordinates, location type and approximate address.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#' @importFrom RCurl getURL
#' @import stringr
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
    message(glue('geocoding url: {URLencode(u_secret)}'))
    place <- read_sf(URLencode(u)) %>%
      cbind(st_coordinates(.)) %>%
      rename(lng = X, lat = Y) %>%
      st_drop_geometry()

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

