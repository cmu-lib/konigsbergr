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
  expect_true(all(sf::st_geometry_type(boston_points) == "POINT"))
  expect_equal(nrow(boston_points), vcount(boston_graph))

  expect_is(boston_linestrings, "sf")
  expect_true(all(sf::st_geometry_type(boston_linestrings) == "LINESTRING"))
  expect_equal(nrow(boston_linestrings), ecount(boston_graph))

  suppressMessages({
    boston_pathway <- cross_all_bridges(boston_graph)
  })

  path_lines <- pathway_to_sf(boston_graph, boston_pathway)
  expect_is(path_lines, "sf")
  expect_is(path_lines, "konigsberg_sf")

  boston_map <- view_konigsberg_path(boston_graph, boston_pathway)
  expect_is(boston_map, "mapview")
})
