context("test-graph_construction")

test_that("extracts OSM nodes", {
  knodes <- get_kongisberger_nodes(boston)
  expect_gt(nrow(knodes), 0)

  expect_is(knodes, "tbl_df")

  expect_true(has_name(knodes, "id"))
  expect_false(anyNA(knodes$id))
  expect_is(knodes$id, "numeric")

  expect_true(has_name(knodes, "lat"))
  expect_false(anyNA(knodes$lat))
  expect_is(knodes$lat, "numeric")

  expect_true(has_name(knodes, "lon"))
  expect_false(anyNA(knodes$lon))
  expect_is(knodes$lon, "numeric")

  expect_true(has_name(knodes, "label"))
  expect_is(knodes$label, "character")
})

test_that("extracts OSM ways", {
  kways <- get_kongisberger_ways(boston)

  expect_is(kways, "tbl_df")
  expect_gt(nrow(kways), 0)

  expect_true(has_name(kways, "id"))
  expect_false(anyNA(kways$id))
  expect_is(kways$id, "numeric")

  expect_true(all(osm_edge_tag_keys() %in% names(kways)))
})
