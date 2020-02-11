#' Set API Key
#'
#' Helper function to set the mapbox API key as an environment variable
#'
#' @param key a mapbox API key
#'
#' @examples
#' \dontrun{
#' set_mapbox_api('<api key here>')
#' # Check our assignment worked
#' Sys.getenv('mapbox_api_key')
#'
#' #[1] "<api key here>"
#' }
#' @export

set_mapbox_api <- function(key){
  Sys.setenv(mapbox_api_key = key)
}
