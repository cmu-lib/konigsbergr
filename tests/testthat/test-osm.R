context("test-osm")

test_that("osm bbox returns a correct osmar object", {
  xmin <- -73.11673
  xmax <- -73.10871
  ymin <- 42.69575
  ymax <- 42.70097

  expect_message(res <- get_osm_bbox(xmin, xmax, ymin, ymax), regexp = "complete")
  expect_error(get_osm_bbox(xmax, xmin, ymax, ymin), regexp = "no nodes")
  expect_is(res, "osmar")
})
