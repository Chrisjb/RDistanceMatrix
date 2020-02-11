context('make_isochrone')

test_that("mapbox method returns sf", {
  skip_if_not_installed("sf")

  mapbox_drv_iso <- make_isochrone(site = 'London Bridge Station, London, UK', time = 30,
                                   method = 'mapbox', direction ='out',
                                   mode = 'driving')

  expect_true('sf' %in% class(mapbox_drv_iso))

  expect_error(make_isochrone(site = 'London Bridge Station, London, UK', time = 30,
                              method = 'mapbox', direction ='in',
                              mode = 'driving'),
               'does not support')

  expect_error(make_isochrone(site = 'London Bridge Station, London, UK', time = 30,
                              method = 'mapbox', direction ='out',
                              mode = 'transit'),
               'not available')
})


test_that("google method returns sf", {
  skip_if_not_installed("sf")

  google_drv_iso <- make_isochrone(site = 'London Bridge Station, London, UK', time = 15,
                                   method = 'google', direction ='out', detail = 'low',
                                   mode = 'driving')

  expect_true('sf' %in% class(google_drv_iso))

  google_trnst_iso <- make_isochrone(site = 'London Bridge Station, London, UK', time = 15,
                              method = 'google', direction ='in', detail = 'med', multiplier = 0.1,
                              mode = 'walking')

  expect_true('sf' %in% class(google_trnst_iso))

})

test_that('missing API key returns error', {
  expect_error(make_isochrone(site = 'London Bridge Station, London, UK', time = 30,
                              method = 'mapbox', direction ='out',
                              mode = 'transit', mapbox_api_key = ''),
               'set mapbox api key')
})
