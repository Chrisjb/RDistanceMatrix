#' geocode a location using the google maps geocode API.
#'
#' The function geocodes a single address given as a character string. Be as specific as possible, including the country or postcode to avoid ambiguity.
#' To avoid loss of accuracy when geocoding a lat/lng string, the geoCode function does not geocode lat/lng strings but instead returns the same string as a dataframe.
#'
#' @param address character string with address to be geocoded. Be as specific as possible to avoid ambiguity.
#' @param api_key character string containing api key for the google maps geocode api. You can generate one for free in your google cloud console.
#' @param verbose logical value. If TRUE, echos the input address
#' @param return_all logical value. Some locations may return more than one geocoded result. If you only want one row returned set return_all = F.
#'
#' @return a data.frame containing the coordinates, location type and approximate address.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom RCurl getURL
#' @import stringr
#'
#' @examples
#' geocode('London Bridge, London, UK', return_all = F)
#'
#' #  lat        lng             type                           address
#' #  51.50788 -0.0877321 GEOMETRIC_CENTER London Bridge, London SE1 9RA, UK
#'
#' @export



geocode <- function(address, api_key = Sys.getenv('google_api_key'), verbose=FALSE, return_all = TRUE) {
  # if address is in lat lng form, do not geocode.
  if(isLatLng(address)){
    return(data.frame(lat=getLat(address),lng=getLng(address),type=NA, address='User Defined Location'))
  } else if(is.na(address)){
      return(data.frame(lat=NA,lng=NA,type=NA, address=NA))
  } else {
    if(verbose) message(glue::glue("geocoding: {address}\n"))
    u <- url(address,api_key=api_key)
    u_secret <- gsub(api_key,'<API_key_here>', u)
    if(verbose) message(glue::glue("trying url: {u_secret}\n"))
    res <- httr::GET(u)
    x <- httr::content(res, simplifyVector = T)
    err <- httr::http_error(res)
    status <- httr::http_status(res)

    if(status$category=="Success") {
      lat <- x$results$geometry$location[1]
      lng <- x$results$geometry$location[2]
      type  <- x$results$geometry$location_type
      address  <- x$results$formatted_address
      if(return_all == T){
        return(data.frame(lat, lng, type, address))
      } else {
        return(data.frame(lat, lng, type, address)[1,])
      }
      Sys.sleep(0.1)
    } else {
      warning(glue::glue('API request failed with status: {status$category}. Reason given: {status$reason}\n Original url (api key masked): {u_secret} \n Returning empty data.frame...'),call. = F)
      return(data.frame(lat=NA,lng=NA,type=NA, address=NA))
    }
  }
}
