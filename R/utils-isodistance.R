validate_distance_args <- function(google_api_key, direction, site, distance, detail, mode, departing, init_grid_size) {
  # ensure API Key is correct
    if(Sys.getenv('google_api_key') == ''){
      stop('please set google api key (see set_google_api)')
    }

  # ensure direction correctly specified
  if(!direction %in% c('in', 'out')){
    stop(paste0('direction parameter must be one of "in" or "out". "in" builds an isochrone based on travel times travelling towards the site. "out" builds the isochrone travelling away from the site.'))
  }
  # ensure site is either a data frame containing lat and lon, or a string to be geocoded
  if(is.data.frame(site)){
    if('lat' %in% names(site)) {
      message('found column "lat" in site dataframe...')
    } else {
      stop('If the site parameter is a dataframe it should contain a column named "lat".')
    }

    if('lng' %in% names(site)){
      message('found column "lng" in site dataframe...')
    } else if('lon' %in% names(site)){
      message('found column "lon" in site dataframe...')
      assign('site', rename(site, lng = lon), envir = parent.frame())
    } else {
      stop('If the site parameter is a dataframe it should contain a column named "lng".')
    }
  } else if(!is.character(site)){
    stop('Site should be a dataframe or a character string to be geocoded.')
  }

  # distance should be numeric
  if(!is.numeric(distance)){
    stop('distance should be specified as a numeric value (distance in meters)')
  }

  # detail should be correctly specified
  if(!detail %in% c('low', 'med', 'medium','high')) {
    stop('Detail should be one of low, med or high')
  }

  # mode should be valid
  if(!(mode %in% c('driving','walking','cycle','cycling','bicycle','transit'))){
    stop('mode must be one of: driving, walking, cycle, transit')
  }

  # departure time should be valid, if set
  if(departing != F & !grepl('[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2,}',departing)) {
    stop(paste0('departure time must either be set to FALSE or a time in the format YYYY-MM-DD HH:MM:SS. Note the time is local time.'))
  }


}
