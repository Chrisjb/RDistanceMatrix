#' Get Employment Within Boundary
#'
#' Fetches the employment within a given boundary. The boundary is given as an sf file and intersected with LSOAs.
#' The employment data is fetched from NOMIS Business Register and Employment Survey (BRES) data.
#'
#' @param boundary an sf polygon in England or Wales.
#' @param year the year for which to return employment data
#' @param type 'employment' or 'employees'
#' @param split If split is set to TRUE and type is set to 'employees', the employment will be separated into part time and full time workers
#' @param industry 'all' for no industry breakdown. 'broad' for broad sections, 'sections' for sections, or '2digit' for two digit SIC codes.
#' @param api_key can be specified manually here, or set using 'set_nomis_api' function.
#'
#'
#' @return a tibble containing employment data
#'
#' @importFrom glue glue
#' @importFrom sf st_intersection st_transform st_drop_geometry st_area
#' @importFrom dplyr mutate select rename if_else case_when filter left_join summarise
#' @importFrom readr cols
#' @importFrom janitor clean_names
#' @importFrom httr GET content http_type http_error
#'
#' @examples
#' \dontrun{
#' iso <- make_isochrone(site = 'SE1 9SG', time = 30, method = 'mapbox', mode = 'driving')
#' emp <- get_employment_within(iso, 'latest', industry = 'all')
#' # total employment within isochrone:
#' emp %>%
#'  dplyr::summarise(total_emp = sum(employment_within))
#'  }
#'
#' @export

get_employment_within <- function(boundary, year = 'latest', type = 'employment', split = FALSE, industry = 'all', api_key = Sys.getenv('nomis_api_key')){
  intersect <- suppressWarnings(sf::st_intersection(sf::st_transform(boundary, 27700), sf::st_transform(lsoa,27700))) %>%
    dplyr::mutate(overlap_area = as.numeric(sf::st_area(.$geometry)),
           overlap = round(.data$overlap_area / .data$area, 2)) %>%
    dplyr::filter(.data$overlap >0) %>%
    sf::st_drop_geometry() %>%
    dplyr::select(.data$Code,.data$overlap)

  codes <- intersect$Code
  nomis_id <- dplyr::if_else(year >= 2015,  'NM_189_1', '****')
  industry_group <- dplyr::case_when(industry == 'all' ~'37748736',
                         industry == 'broad' ~ '163577857...163577874',
                         industry == 'sections' ~'150994945...150994965',
                         industry == '2digit' ~ '146800641...146800643,146800645...146800673,146800675...146800679,146800681...146800683,146800685...146800687,146800689...146800693,146800695,146800696,146800698...146800706,146800708...146800715,146800717...146800722,146800724...146800728,146800730...146800739')

  emp_type <- dplyr::case_when(type == 'employment' ~'4',
                               type == 'employees' & split == FALSE ~ '1',
                               type == 'employees' & split == TRUE ~ '2,3')

  u <- glue::glue("https://www.nomisweb.co.uk/api/v01/dataset/{nomis_id}.data.csv?geography={paste0(codes,collapse=',')}&date={year}&industry={industry_group}&employment_status={emp_type}&measure=1&measures=20100")
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
    httr::content(res, col_types= readr::cols()) %>%
      janitor::clean_names() %>%
      dplyr::select(.data$date, .data$geography_code,
                    .data$geography_name, .data$geography_type,
                    .data$industry_code, .data$industry_name,
                    .data$employment_status_name, employment = .data$obs_value,
                    .data$obs_status_name, .data$obs_status)
  },
  error = function(e){
    stop(glue::glue('error: {e}'))
  },
  warning = function(w){
    warning(glue::glue('warning: {w}'))
  })

  if(nrow(df) == 25000){
    warning('We have hit the 25,000 observation limit for our request. The data returned will not be complete. Please set an API key or use a smaller query.')
  }

  if(industry != 'all'){
    out <- df %>%
      dplyr::left_join(intersect, by =c('geography_code'='Code')) %>%
      dplyr::mutate(employment_within = .data$employment * .data$overlap) %>%
      tidyr::extract(.data$industry_name, into = c('industry_id','industry_name'), '([A-Z0-9]+)[ :]+([A-Za-z, \\& \\(\\)]+)')
  } else {
    out <- df %>%
      dplyr::left_join(intersect, by =c('geography_code'='Code')) %>%
      dplyr::mutate(employment_within = .data$employment * .data$overlap)
  }

  return(out)
}


