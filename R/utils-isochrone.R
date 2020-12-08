approx_grid <- function(lat,lng,km) {
  dist_lat <- km/6378 * (180/pi)
  dist_lng <- km/6387 *(180/pi) / cos(lat * pi/180)
  return(data.frame(lat=dist_lat, lng = dist_lng))
}


dist_url <- function(origin, dest, mode, departing = F, model = 'best_guess',
                     api_key = api_key) {

  # enter departing in form: YYYY-MM-DD 08:00:00
  if(departing != F & !grepl('[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2,}',departing)) {
    stop(paste0('departure time must either be set to FALSE or a time in the format YYYY-MM-DD HH:MM:SS. Note the time is local UK time.'))
  } else if(departing != F) {
    # if departure time is set, set it to a time in the future (later today or tomorrow depending on whether or not the time has already passed today.
    # has the entered time already passed today?
    if(Sys.time() > as.POSIXct(departing)) {
      stop(paste0('your departure time ', departing, 'is not in the future.'))
    } else {
      departure_time <- paste0('&departure_time=',as.character(as.numeric(as.POSIXct(departing))),'&traffic_model=', model)
    }
  } else if(departing == F) {
    departure_time <- ''
  }

  # create web address
  root <- "https://maps.googleapis.com/maps/api/distancematrix/json"
  i <- paste0(root, "?origins=", origin, "&destinations=", dest, "&mode=",mode,
              departure_time, "&key=", api_key)

  return(utils::URLencode(i))


}


distance_from_origins <- function(origin, dest, mode='driving', departing = F, model='best_guess', api_key = api_key, verbose=FALSE, max_dimension = 25) {
  if(!(mode %in% c('driving','walking','cycle','cycling','bicycle','transit'))){
    stop('mode must be one of: driving, walking, cycle, transit')
  }

  dists_vec <- c()
  time_vec <- c()

  # Google Map Distance Matrix Service API only allow 25 origins per request
  lots <- length(origin) %/% max_dimension
  if(length(dest) %% max_dimension != 0) {
    lots <- lots + 1
  }

  for(i in 1:(lots)){

    # set up destination request string up to designated number of origins
    origin_txt <- gsub(' ', "+", paste0(origin[(1+max_dimension*(i-1)):(max_dimension*i)][!is.na(origin[(1+max_dimension*(i-1)):(max_dimension*i)])],collapse='|'))

    if(origin_txt == ''){
      break
    }

    u <- dist_url(origin_txt,dest,mode,departing,model, api_key=api_key)

    if(verbose == TRUE) {
      message('trying url: ', u)
    }

    u_secret <- gsub(api_key, '<api_key_here>', u)

    if(lots > 1){
      message(glue::glue('Trying URL: {i} of {lots}', ))
    }

    doc <- httr::GET(u)
    status <- httr::http_status(doc)
    err <- httr::http_error(doc)

    if(err){
      stop(glue::glue('API request failed with error: {status$reason}\n attempted url (api key masked): {u_secret}'))
    }
    x <- httr::content(doc, simplifyVector = TRUE)
    if(status$category=="Success"  & departing !=F & mode =='driving') {
      tbl <- lapply(x$rows$elements,function(x) unlist(x) %>% as.data.frame.list(stringsAsFactors = F) ) %>% dplyr::bind_rows()
      dists_vec <- c(dists_vec,tbl$distance.value)
      time_vec <- c(time_vec,tbl$duration_in_traffic.value)
      Sys.sleep(0.1)

    } else if(status$category=="Success") {
      tbl <- lapply(x$rows$elements,function(x) unlist(x) %>% as.data.frame.list(stringsAsFactors = F) ) %>% dplyr::bind_rows()
      dists_vec <- c(dists_vec,tbl$distance.value)
      time_vec <- c(time_vec,tbl$duration.value)
      Sys.sleep(0.1)

    } else{
      stop(paste0('Error in url: ', u))
    }

  }

  return(list(dist = dists_vec,
              time = time_vec))

}



distance_to_destinations <- function(origin,dest,mode,departing=F,model='best_guess',api_key = api_key, max_dimension = 25){

  dists_vec <- c()
  time_vec <- c()

  # Google Map Distance Matrix Service API only allow 25 destinations per request
  lots <- length(dest) %/% max_dimension

  if(length(dest) %% max_dimension != 0) {
    lots <- lots + 1
  }

  for(i in 1:(lots)){
    # set up destination request string up to designated number of destinations
    dest_txt <- gsub(' ',"+",paste0(dest[(1+max_dimension*(i-1)):(max_dimension*i)][!is.na(dest[(1+max_dimension*(i-1)):(max_dimension*i)])],collapse='|'))
    if(dest_txt == ''){
      break
    }

    u <- dist_url(origin,dest_txt,mode,departing,model,api_key=api_key)

    # don't include ferries in walk directions
    if(mode == 'walking') {
    u <- gsub('(.+&mode=[A-Za-z]+)(&key=.+)','\\1&avoid=ferries\\2',u)
    }

    u_secret <- gsub(api_key, '<api_key_here>', u)
    if(lots > 1){
      message(glue::glue('Trying URL: {i} of {lots}', ))
    }

    res <- httr::GET(u)
    err <- httr::http_error(res)
    status <- httr::http_status(res)
    if(err){
      stop(glue::glue('API request failed with error: {status$reason}\n attempted url (api key masked): {u_secret}'))
    }
    x <- httr::content(res, simplifyVector=TRUE)

    # if valid result and we have set departure time for driving, get time in traffic
    if(status$category=="Success"  & mode=='driving' & departing !=F) {
      dist <- x$rows$elements[[1]]$distance$value
      time <- x$rows$elements[[1]]$duration_in_traffic$value
      dists_vec <- c(dists_vec,dist)
      time_vec <- c(time_vec,time)
      Sys.sleep(0.1)
    } else if(status$category=="Success") {
      dist <- x$rows$elements[[1]]$distance$value
      time <- x$rows$elements[[1]]$duration$value
      dists_vec <- c(dists_vec,dist)
      time_vec <- c(time_vec,time)
      Sys.sleep(0.1)
    } else {
      stop(paste0('Error in url: ', u))
    }

  }

  return(list(dist = dists_vec,
              time = time_vec))
}






check_request_valid <- function(method, mapbox_api_key, google_api_key, direction, site, time, detail, mode, departing, init_grid_size, multiplier) {
  # ensure API Key is correct
  if(method %in% c('google','google_guess') ){
    assign('api_key', google_api_key, envir = parent.frame())
    if(google_api_key== ''){
      stop('please set google api key (see set_google_api)')
    }
  } else if(method == 'mapbox'){
    assign('api_key', mapbox_api_key, envir = parent.frame())
    if(mapbox_api_key == ''){
      stop('please set mapbox api key (see set_mapbox_api)')
    }
    if(direction == 'in'){
      stop('mapbox method does not support direction "in"')
    }
  } else{
    stop('method must be one of google or mapbox')
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

  # time should be numeric
  if(!is.numeric(time)){
    stop('Time should be specified as a numeric value (time in minutes)')
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
    stop(paste0('departure time must either be set to FALSE or a time in the format YYYY-MM-DD HH:MM:SS. Note the time is local UK time.'))
  } else if(departing != F) {
    # if departure time is set, set it to a time in the future (later today or tomorrow depending on whether or not the time has already passed today.

    # google maps wants it in the format in time elapsed since 01/01/1970 UTC.

    # has the entered time already passed today?
    if(Sys.time() > as.POSIXct(departing)) {
      stop(paste0('your departure time ', departing, ' is not in the future.'))
    }
  }


}
