#' Get network distance
#'
#' Fetch the distance between sets of origins and destinations using Google Distance Matrix API
#' @param data a data.frame or tibble containing our origins, destinations or both.
#' @param origins a column in our data.frame containing the origin addresses - can be of form 'lat,lng' or address with postcode. Else a string or vector can be passed.
#' @param destinations a column in our data.frame containing the destination addresses - can be of form 'lat,lng' or address with postcode. Else a string or vector can be passed.
#' @param mode driving, cycling, transit or walking.
#' @param departing departure time can be set in format 'YYYY-MM-DD HH:MM:SS'
#' @param model 'best_guess' is the default model. Google also provides 'optimistic' and 'pessimistic' models. See google distance matrix API documentation.
#' @param google_api_key google cloud console API key. Ensure this is enabled on the distance matrix API and geocoding API.
#'
#' @return a data.frame containing the coordinates, location type and approximate address.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr pull mutate bind_rows
#' @importFrom glue glue
#' @importFrom rlang ensym UQ
#' @import stringr
#'
#' @examples
#' \dontrun{
#' ## Example with a dataframe of origins (lat lng) and a single destination
#' df <- tibble::tribble(
# ~ lat, ~lng,
# 51.5131, -0.09182,
# 51.5037, -0.01715,
# 51.5320, -0.12343,
# 51.4447, -0.33749
# )
#' origin_df <- dplyr::mutate(df, origin = paste0(lat,',',lng))
#'
#' get_distance(origin_df, origin, 'London Paddington')
#'
#' # lat     lng origin           driving_distance driving_time
#' # <dbl>   <dbl> <chr>                       <dbl>        <dbl>
#' # 1  51.5 -0.0918 51.5131,-0.09182             7877         26.9
#' # 2  51.5 -0.0172 51.5037,-0.01715            13839         39.3
#' # 3  51.5 -0.123  51.532,-0.12343              5231         20.0
#' # 4  51.4 -0.337  51.4447,-0.33749            17944         38.4
#'
#'
#'
#' ## Example with a single dataframe containing postcodes and lat,lng coordinates
#' pcd_df <- tibble::tribble(
#'  ~ origin,           ~destination,
#' "51.5131,-0.09182", 'EC2R 8AH',
#' "51.5037,-0.01715", 'E14 5AB',
#'" 51.5320,-0.12343", 'SE1 9SG',
#' "51.4447,-0.33749", 'SW1A 1AA'
#' )
#'
#' get_distance(pcd_df, origin, destination)
#'
#' #   origin              destination driving_distance driving_time
#' #   <chr>               <chr>                  <dbl>        <dbl>
#' # 1 "51.5131,-0.09182"  EC2R 8AH                 515         3.4
#' # 2 "51.5037,-0.01715"  E14 5AB                  867         3.33
#' # 3 " 51.5320,-0.12343" SE1 9SG                 5747        24.0
#' # 4 "51.4447,-0.33749"  SW1A 1AA               16895        40.2
#' }
#' @export

get_distance <- function(data, origins, destinations,
                          mode= c('driving','walking', 'cycling', 'transit'), departing = FALSE,
                          model = c('best_guess','pessimistic', 'optimistic'),
                          google_api_key = Sys.getenv('google_api_key'),verbose=verbose) {
  # match args
  mode <- match.arg(mode)
  model <- match.arg(model)

  # check if tidy eval needed
  origins <- tryCatch(data %>% dplyr::pull({{origins}}), error = function(e) {
                        as.character(origins)
                        })

  destinations <- tryCatch(data %>% dplyr::pull({{destinations}}), error = function(e) {
    as.character(destinations)
  })

  # check how many origins and destinations
  n_origins <- length(origins)
  n_destin <- length(destinations)


  # check parameters
  if(departing == FALSE & model != 'best_guess'){
    warning(glue::glue('traffic model {model} is not available without a departure time set'))
  }

  # if n_origins is 1, use distance_to_destinations
  if(n_origins ==1){
    dmat <- distance_to_destinations(origin = origins,dest = destinations, mode=mode, departing=departing, model=model,api_key = google_api_key, verbose=verbose)
  } else if(n_destin ==1){
    dmat <- distance_from_origins(origin = origins, dest = destinations, mode = mode, departing = departing, model = model, api_key=google_api_key,verbose=verbose)
  } else {
    dmat_list <- apply(data.frame(origins, destinations),MARGIN = 1,
                  function(x) distance_from_origins(origin = x[['origins']], dest = x[['destinations']], mode = mode, departing = departing, model = model, api_key=google_api_key,verbose=verbose)
                  )
    dmat <- dplyr::bind_rows(dmat_list)
  }


  time_col <-glue::glue('{mode}_time')
  dist_col <- glue::glue('{mode}_distance')

  if(time_col %in% names(data)){
    warning(paste0(time_col, ' already exists in the dataset, overwriting...'))
  } else if (dist_col %in% names(data)){
    warning(paste0(dist_col, ' already exists in the dataset, overwriting...'))
  }

  data %>%
    dplyr::mutate(!!ensym(dist_col) := as.numeric(dmat$dist),
                  !!ensym(time_col) := as.numeric(dmat$time)/60)
}

