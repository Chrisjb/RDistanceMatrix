#' Set API Key
#'
#' Helper function to set the google API key as an environment variable
#'
#' @param key
#'
#'
#' @examples
#' set_google_api('<api key here>')
#' # Check our assignment worked
#' Sys.getenv('google_api_key')
#'
#' #[1] "<api key here>"
#'
#' @export

set_google_api <- function(key){
  Sys.setenv(google_api_key = key)
}
