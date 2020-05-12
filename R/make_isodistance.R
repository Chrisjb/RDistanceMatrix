#' Make Isodistance
#'
#' Generates an isodistance polygon for network distances to/from a specified location.
#'
#' @param site either a data.frame with the columns 'lat' and 'lng' or a character string to be geocoded.
#' @param distance a numeric value in meters. This is the network distance from the site to the isochrone extents.
#' @param direction either 'in' or 'out'. In generates an isochrone from multiple origins to the site. Out goes from the site to multiple destinations.
#' @param detail 'low', 'medium' or 'high' level of detail in the isochrone. High will produce the most granular detail but will use more API credits.
#' @param mode a character string for the mode of travel. Possible values are 'driving', 'cycling', 'transit', or 'walking'
#' @param departing optional parameter for getting distance in traffic. Google maps may route differently to avoid heavy traffic, changing the distance at peak times. If set, takes the format "YYYY-MM-DD HH:MM:SS"
#' @param google_api_key the google maps API key. This can be generated from the google cloud console and set with set_google_api
#'
#'
#' @return an sf polygon
#'
#' @import sf
#' @importFrom maptools ContourLines2SLDF
#' @importFrom utils URLencode
#' @importFrom httr GET content http_error http_status
#'
#' @examples
#'
#' walk_radius <- make_isodistance('EC2R 8AH', distance = 2000, direction = 'out', mode = 'walking')
#'
#'
#' @export

make_isodistance <- function(site, distance, direction = c('out', 'in'),
                             detail = 'medium',
                             mode= c('driving','walking', 'cycling', 'transit'),
                             departing = FALSE,
                             google_api_key = Sys.getenv('google_api_key')){

  # validity checks...
  # handle match.arg
  mode <- match.arg(mode)
  direction <- match.arg(direction)
  args <- as.list(match.call())[-1]
  form <- formals()
  form[names(args)] <- args
  form['mode'] <- mode
  form['direction'] <- direction

  # validity checks...
  do.call(validate_distance_args, form)

  # geocode site if lat lng not already given
  if(is.character(site)) {
    message('Geocoding: "', site,'" if you entered precise co-ordinates, please specify site as a data frame containing the columns "lat" and "lng"')
      site <- geocode(site, api_key = google_api_key)[1,]
  }


  # get grid extents
  extents <- approx_grid(site$lat, site$lng, distance/1000)

  lats <- seq(from=-extents$lat, to = extents$lat, length.out = 8) + site$lat
  lngs <- seq(from=-extents$lng, to = extents$lng, length.out = 8) + site$lng

  distanceDF <- expand.grid(lats, lngs)
  names(distanceDF) <- c('lat', 'lng')

  df_init <- dplyr::mutate(distanceDF, latlng=paste0(lat,',',lng))

  # set up for direction = out
  if(direction == 'out') {
    dists_init <- distance_to_destinations(origin = paste0(site$lat,',',site$lng), dest = df_init$latlng, mode = mode,
                                           departing = departing, api_key = google_api_key)
    # set up for direction = in
  } else if(direction == 'in'){
    dists_init <- distance_from_origins(origin = df_init$latlng, dest = paste0(site$lat,',',site$lng),
                                        mode = mode, departing = departing, api_key = google_api_key)
  }

  df_init <- dplyr::mutate(df_init, distance = as.numeric(dists_init$dist))


  # make spatial dataframe
  m_init <- matrix(df_init$distance, nrow = sqrt(nrow(df_init)),
              dimnames = list(unique(df_init$lat),
                              unique(df_init$lng)),
              byrow = T)

  # if missing value, make it arbitrarily high
  m_init[is.na(m_init)] <- 2*distance

  tryCatch({
    contour_init <- contourLines(x=unique(df_init$lng), y= unique(df_init$lat), z=m_init, levels = seq(from = 0, to = distance, by= distance/5 ))
  },
  error =function(e){
    message(glue::glue("Unable to find an isochrone. It's likely the multiplier is set too high.\n Original error message: {e}"))
  })


  shp_init <- maptools::ContourLines2SLDF(contour_init)

  shp_init <- shp_init %>%
    sf::st_as_sf() %>%
    sf::st_set_crs(4326) %>%
    sf::st_cast('POLYGON') %>%
    sf::st_make_valid() %>%
    sf::st_union()

  # get bounding box of our google maps initial poly to calibrate the buffers
  bbox <- st_bbox(shp_init)
  left <- data.frame(lng = bbox[['xmin']], lat =bbox[['ymin']]) %>%
    st_as_sf(coords = c('lng', 'lat'), crs= 4326)
  right <- data.frame(lng = bbox[['xmax']], lat =bbox[['ymin']]) %>%
    st_as_sf(coords = c('lng', 'lat'), crs= 4326)
  top <- data.frame(lng = bbox[['xmin']], lat =bbox[['ymax']]) %>%
    st_as_sf(coords = c('lng', 'lat'), crs= 4326)
  bottom <- data.frame(lng = bbox[['xmin']], lat =bbox[['ymin']]) %>%
    st_as_sf(coords = c('lng', 'lat'), crs= 4326)
  width <- as.numeric(st_distance(left, right))
  height <- as.numeric(st_distance(top, bottom))

  # set buffer to 10% of max(height,width)
  buffer_dist <- max(height, width) * 0.1


  # find key buffer region where shp_range is the possible region for isochrone extents
  shp_max <- shp_init %>%
    st_transform(27700) %>%
    st_buffer(buffer_dist)  %>%
    st_transform(4326)

  shp_min <- shp_init %>%
    st_transform(27700) %>%
    st_buffer(-buffer_dist)  %>%
    st_transform(4326)

  shp_range <- suppressMessages(shp_max %>% st_difference(shp_min))


  # create grid of points over our bounding box
  grid_bbox <- st_bbox(shp_max)

  pts <- dplyr::case_when(detail == 'low' ~ max(floor(max(height, width)/2500),15),
                          detail %in% c('med','medium') ~ max(floor(max(height, width)/1000), 20),
                          detail == 'high' ~ max(floor(max(height, width)/500),30))

  tolerence <- max(height, width)/pts

  lats <- seq(from=grid_bbox$ymin, to = grid_bbox$ymax, length.out = pts)
  lngs <- seq(from=grid_bbox$xmin, to = grid_bbox$xmax, length.out = pts)
  grid <-  expand.grid(lat = lats, lng = lngs)

  grid <- grid %>%
    st_as_sf(coords = c('lng','lat'),crs= 4326, remove = FALSE)


  # intersect grid with possible area range
  st_agr(grid) ='constant'
  donut <- suppressMessages(
    grid %>%
    st_intersection(shp_range)
    )

  # create matrix from our grid of points
  pts_mat <- grid  %>%
    dplyr::mutate(latlng = paste0(lng,',',lat))

  # create points inside donut hole and set travel time to just less than the max time

  inner_pts <- suppressMessages(
    grid %>%
    st_intersection(shp_min) %>%
    dplyr::mutate(latlng = paste0(round(lat,5),',',round(lng,5)),
                  distance = distance -1)
  )

  # create points in donut ring for geocoding
  df <- donut %>%
    dplyr::mutate(latlng = paste0(lat,',',lng))

  elements <- length(df_init$latlng)+length(df$latlng)
  credits <- dplyr::if_else(departing == FALSE, elements*0.005, elements*0.01)

  if(credits >= 10) {
    warning("Large Query Warning: request will use {elements} elements (\u00A3{credits} credits).")
    response <- readline(prompt=paste0("Continue? (y/n)"))

    if(!response %in% c('y', 'Y')) {
      stop('Use a lower detail setting')
    }

  }
  # set up for direction = out
  if(direction == 'out') {
    dists <- distance_to_destinations(origin = paste0(site$lat,',',site$lng), dest = df$latlng, mode = mode,
                                      departing = departing, api_key = google_api_key)
    # set up for direction = in
  } else if(direction == 'in'){
    dists <- distance_from_origins(origin = df$latlng, dest = paste0(site$lat,',',site$lng),
                                   mode = mode, departing = departing, api_key = google_api_key)
  }

  df <- dplyr::mutate(df, distance = as.numeric(dists$dist))

  # join matrices
  distance_df <- suppressMessages(
    pts_mat %>%
    st_join(df) %>%
    dplyr::select(lng=lng.x, lat=lat.x, latlng=latlng.x, distance) %>%
    st_join(inner_pts,left = T) %>%
    dplyr::mutate(distance = dplyr::if_else(!is.na(`distance.x`), `distance.x`, `distance.y`)) %>%
    dplyr::select(lng = lng.x, lat = lat.x, latlng = latlng.x, distance)
  )

  # make distance matrix
  m <- matrix(distance_df$distance, nrow = sqrt(nrow(distance_df)),
              dimnames = list(unique(distance_df$lat),
                              unique(distance_df$lng)),
              byrow = T)


  # if missing value, make it arbitrarily high
  m[is.na(m)] <- distance*1.1

  contour <- contourLines(x=unique(distance_df$lng), y= unique(distance_df$lat),
                          z=m, levels = seq(from = 0, to = distance, by= distance/5 ))

  shp <- maptools::ContourLines2SLDF(contour)

  shp <- shp %>% sf::st_as_sf() %>% sf::st_set_crs(4326) %>%
    sf::st_cast('POLYGON') %>% sf::st_make_valid() %>%
    sf::st_union() %>%
    sf::st_sf()

  message(glue::glue('Google API elements used: {elements} (\u00A3{credits} credits). Isochrone generated to accuracy of {round(tolerence)}m'))
  if(all(sf::st_is_valid(shp))==FALSE){
    shp <- sf::st_make_valid(shp)
  }
  return(shp)
}
