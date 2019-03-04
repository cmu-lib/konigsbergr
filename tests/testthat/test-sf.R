context("test-sf")

test_that("graph to sf", {
  suppressMessages({
    boston_graph <- konigsberg_graph(boston)
  })

  boston_sf <- graph_to_sf(boston_graph)
  expect_is(boston_sf, "sf_graph")

  boston_points <- boston_sf$vertices
  boston_linestrings <- boston_sf$edges

  expect_is(boston_points, "sf")
  expect_true(all(st_geometry_type(boston_points) == "POINT"))
  expect_equal(nrow(boston_points), vcount(boston_graph))

  expect_is(boston_linestrings, "sf")
  expect_true(all(st_geometry_type(boston_linestrings) == "LINESTRING"))
  expect_equal(nrow(boston_linestrings), ecount(boston_graph))
})
