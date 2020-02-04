#' Make Isodistance
#'
#' Generates an isodistance polygon for travel times to/from a specified location.
#'
#' @param site either a data.frame with the columns 'lat' and 'lng' or a character string to be geocoded.
#' @param direction either 'in' or 'out'. In generates an isochrone from multiple origins to the site. Out goes from the site to multiple destinations.
#' @param distance a numeric value in meters - the distance to the isochrone extents.
#' @param pts the number of points to include in estimating our isochrone. More points will be more accurate but will eat up more queries from the google maps API allownce. It is recommended to test on a low number ~10 or less to ensure all of the parameters are correct before attempting to increase the number of points.
#' @param mode a character string for the mode of travel. Possible values are 'driving', 'cycling', 'transit', or 'walking'
#' @param api_key the google maps API key. This can be generated from the google cloud console.
#'
#'
#' @return a list containing the data.frame, leaflet map, and sf object generated.
#'
#' @import sf
#' @importFrom lwgeom st_make_valid
#' @importFrom maptools ContourLines2SLDF
#' @importFrom utils URLencode
#' @importFrom RCurl getURL
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' distance_isochrone <- make_distance_isochrone(site = 'London Bridge Station, London, UK', 'out', distance = 1609, mode = 'walking', api_key = api_key)
#'
#'
#' @export
#'
#'
make_isodistance <- function(site, direction = 'out', distance, pts = 10, mode= 'driving', api_key = Sys.getenv('google_api_key')){
  # direction parameter takes values:
  # 'in': going from multiple origins to a single destination
  # 'out': going from single origin to multiple destinations

  # distance parameter should be in meters

  # direction parameter takes values:
  # 'in': going from multiple origins to a single destination
  # 'out': going from single origin to multiple destinations

  # distance parameter should be in meters

  # validity checks...
  # ensure direction correctly specified
  if(!direction %in% c('in', 'out')){
    stop(paste0('direction parameter must be one of "in" or "out". "in" builds an isochrone based on travel distances travelling towards the site. "out" builds the isochrone travelling away from the site.'))
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
      site <- rename(site, lng = lon)
    } else {
      stop('If the site parameter is a dataframe it should contain a column named "lng".')
    }
  } else if(is.character(site)) {
    message('Geocoding: "', site,'" if you entered precise co-ordinates, please specify site as a data frame containing the columns "lat" and "lng"')

    site <- geoCode(site, api_key = api_key)[1,]

  } else {
    stop('Site should be a dataframe or a character string to be geocoded.')
  }


  # time should be numeric
  if(!is.numeric(distance)){
    stop('Distance should be specified as a numeric value (distance in meters)')
  }

  # number of points should be between 5 and 30
  if(pts <5 | pts > 40) {
    stop('The number of points queried should be between 5 and 40.')
  } else if(pts > 20) {
    response <- readline(prompt=paste0("your query of ", pts, " points will require ", pts*pts, " total points to be requested from the google distance matrix API. It is recommended to use 10-20 points until you are sure of multiplier parameter to save queries. Continue to use ", pts," points? (Y/N)"))

    if(response != 'Y') {
      stop('Use a lower value for pts')
    }
  }

  # mode should be valid
  if(!(mode %in% c('driving','walking','cycle','cycling','bicycle','transit'))){
    stop('mode must be one of: driving, walking, cycle, transit')
  }



  # All correct.. make isochrone
  # give 10% leeway to extents
  extents <- approx_grid(site$lat, site$lng, distance/1000)

  lats <- seq(from=-extents$lat, to = extents$lat, length.out = pts) + site$lat
  lngs <- seq(from=-extents$lng, to = extents$lng, length.out = pts) + site$lng

  distanceDF <- expand.grid(lats, lngs)
  names(distanceDF) <- c('lat', 'lng')

  distanceDF <- dplyr::mutate(distanceDF, latlng=paste0(lat,',',lng))

  # set up for direction = out
  if(direction == 'out') {

    dists <- distance_to_destinations(origin = paste0(site$lat,',',site$lng), dest = distanceDF$latlng, mode = mode, api_key = api_key)

    # set up for direction = in
  } else if(direction == 'in'){

    dists <- distance_from_origins(origin = distanceDF$latlng, dest = paste0(site$lat,',',site$lng),
                                   mode = mode, api_key = api_key)

  }

  distanceDF <- dplyr::mutate(distanceDF, distance = as.numeric(dists$dist))


  # make spatial dataframe
  m <- matrix(distanceDF$distance, nrow = sqrt(nrow(distanceDF)),
              dimnames = list(unique(distanceDF$lat),
                              unique(distanceDF$lng)),
              byrow = T)

  # if missing value, make it arbitrarily high
  m[is.na(m)] <- 2*distance

  cont <- contourLines(x=unique(distanceDF$lng), y= unique(distanceDF$lat), z=m, levels = seq(from = 0, to = distance, by= distance/5 ))

  shp <- maptools::ContourLines2SLDF(cont)

  shp2 <- shp %>% sf::st_as_sf() %>% sf::st_set_crs(4326) %>% sf::st_cast('POLYGON') %>% lwgeom::st_make_valid() %>% sf::st_union()



  return(shp2)




}
