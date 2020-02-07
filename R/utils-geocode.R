#' Utility functions used in geoCode.R
url <- function(address, return.call = "json", sensor = "false",
                api_key='AIzaSyB5N4Uij7F5XchUWJ-xvCqLBVfnmWNj9f0') {
  root <- "https://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor,"&key=",api_key, sep = "")
  return(utils::URLencode(u))
}

isLatLng <- function(address) {
  if(is.na(address)){
    return(FALSE)
  } else if(stringr::str_detect(address,'[A-Za-z]{1,}')){
    return(FALSE)
  } else {
    stringr::str_detect(address, '[-\\d ]{1,}.[\\d ]{1,}[,]{1}[-\\d ]{1,}.[\\d]{1,}')
  }


}

getLat <- function(address) {
  if(isLatLng(address)) {
    return(stringr::str_split(address, ',')[[1]][1] %>%
             stringr::str_trim('both') %>%
             as.numeric())
  }
  else{
    return(NA)
  }
}

getLng <- function(address) {
  if(isLatLng(address)) {
    return(stringr::str_split(address, ',')[[1]][2] %>%
             stringr::str_trim('both') %>%
             as.numeric())
  }
}

hfDist <- function(df,lat,lng, lat_ext, lng_ext) {
  df<- df %>%  mutate(lat1=as.numeric(!!enquo(lat)),
                      long1=as.numeric(!!enquo(lng)),
                      lat2 = as.numeric(lat_ext),
                      long2 = as.numeric(lng_ext))
  R <- 6371 # Earth mean radius [km]
  delta.long <- (df$long2*pi/180 - df$long1*pi/180)
  delta.lat <- (df$lat2*pi/180 - df$lat1*pi/180)
  a <- sin(delta.lat/2)^2 + cos(df$lat1*pi/180) * cos(df$lat2*pi/180) * sin(delta.long/2)^2
  c <- 2 * asin(pmin(1,sqrt(a)))
  d = R * c
  df_out<-df %>% mutate(distance_hav=d*1000) %>% select(-lat1,-long1,-lat2,-long2)
  return(df_out) # Distance in m
}
