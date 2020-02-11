#' Set API Key
#'
#' Helper function to set the NOMIS API key as an environment variable
#'
#' @param key the NOMIS API Key (unique ID)
#'
#' @examples
#' set_nomis_api('<api key here>')
#' # Check our assignment worked
#' Sys.getenv('nomis_api_key')
#'
#' #[1] "<api key here>"
#'
#' @export

set_nomis_api <- function(key){
  Sys.setenv(nomis_api_key = key)
}





