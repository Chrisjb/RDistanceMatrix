#' Get Population Within Boundary
#'
#' Fetches the population within a given boundary. The boundary is given as an sf file and intersected with LSOAs.
#' The population data is fetched from NOMIS population estimates for small areas.
#'
#' @param boundary an sf polygon in England or Wales.
#' @param year the year for which to return employment data
#' @param age 'all' for no age breakdown. 'working' returns young, working age, and older groupings. 'five_year' for five year groupings or 'sya' for single year of age.
#' @param api_key can be specified manually here, or set using 'set_nomis_api' function.
#'
#'
#' @return a tibble containing employment data
#'
#' @importFrom glue glue
#' @importFrom sf st_intersection st_transform
#' @importFrom dplyr mutate select rename if_else case_when left_join
#' @importFrom janitor clean_names
#' @importFrom httr GET content http_type http_error
#'
#' @examples
#' iso <- make_isochrone(site = 'SE1 9SG', time = 30, method = 'mapbox', mode = 'driving')
#' pop <- get_population_within(iso, 'latest', age = 'all')
#' # total employment within isochrone:
#' pop %>%
#'  summarise(total_pop = sum(population_within))
#'
#' @export
#'
get_population_within <- function(boundary, year = 'latest', age = 'all', api_key = Sys.getenv('nomis_api_key')){
  if(all(sf::st_is_valid(boundary)) == FALSE){
    boundary <- lwgeom::st_make_valid(boundary)
  }
  intersect <- suppressWarnings(st_intersection(st_transform(boundary, 27700), st_transform(lsoa,27700))) %>%
    dplyr::mutate(overlap_area = as.numeric(st_area(geometry)),
                  overlap = round(overlap_area / area, 2)) %>%
    dplyr::filter(overlap >0) %>%
    sf::st_drop_geometry() %>%
    dplyr::select(Code,overlap)

  codes <- intersect$Code
  age_group <- dplyr::case_when(age == 'sya' ~'101...191',
                         age == 'all' ~ '200',
                         age == 'working' ~'201,203,209',
                         age == 'five_year' ~ '1,3...18,210')
  u <- glue::glue("https://www.nomisweb.co.uk/api/v01/dataset/NM_2010_1.data.csv?geography={paste0(codes,collapse=',')}&date={year}&gender=0&c_age={age_group}&measures=20100")
  if(api_key != ''){
    u <- paste0(u, '&uid=',api_key)
  }
  res <- httr::GET(u)
  if (httr::http_type(res) != "text/csv") {
    stop("Something went wrong. NOMIS API did not return csv data.")
  } else if( httr::http_error(res)){
    stop(glue::glue('Request failed with status: {httr::http_status(res)$reason}'))
  }
  df <- tryCatch({
    httr::content(res, col_types =cols()) %>%
      janitor::clean_names() %>%
      select(date, geography_code, geography_name, geography_type, gender_name, age = c_age_name, age_type = c_age_type, population = obs_value, record_count)
  },
  error = function(e){
    glue::glue('error: {e}')
  },
  warning = function(w){
    glue::glue('warning: {w}')
  })

  if(nrow(df) == 25000){
    warning('We have hit the 25,000 observation limit for our request. The data returned will not be complete. Please set an API key or use a smaller query.')
  }


  df %>%
    dplyr::left_join(intersect, by =c('geography_code'='Code')) %>%
    dplyr::mutate(population_within = population * overlap)
}


