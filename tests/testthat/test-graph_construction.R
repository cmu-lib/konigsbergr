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

test_that("base graph construction", {
  expect_message(base_graph <- create_base_konigsberg_graph(boston), regexp = "complete")

  expect_is(base_graph, "konigsberg_graph")
  expect_is(base_graph, "tbl_graph")

  expect_equal(components(base_graph)$no, 1L)
  expect_true(all(edge_attr_names(base_graph) %in% c("id", "label", osm_edge_tag_keys(), "bridge_relation")))
  expect_true(all(vertex_attr_names(base_graph) %in% c("id", "lat", "lon", "label")))
  expect_true(is_directed(base_graph))
  expect_true(is_connected(base_graph))
})

test_that("filter ways", {
  suppressMessages(base_graph <- create_base_konigsberg_graph(boston))

  filtered_graph <- automobile_highways(base_graph)
  expect_true(is_connected(filtered_graph))

  edge_table <- as_tibble(filtered_graph, "edges")
  expect_false(any(edge_table[["access"]] == "no", na.rm = TRUE))
  expect_true(all(edge_table[["highway"]] %in% c("residential",
                                                   "tertiary",
                                                   "primary",
                                                   "secondary",
                                                   "motorway_link",
                                                   "unclassified",
                                                   "motorway",
                                                   "trunk",
                                                   "primary_link",
                                                   "trunk_link",
                                                   "tertiary_link",
                                                   "secondary_link")))

  filtered_graph <- pedestrian_highways(base_graph)
  expect_true(is_connected(filtered_graph))

  edge_table <- as_tibble(filtered_graph, "edges")
  expect_false(any(edge_table[["foot"]] == "no", na.rm = TRUE))
  expect_true(all(edge_table[["highway"]] %in% c("footway", "pedestrian", "path", "primary", "secondary", "tertiary", "primary_link")))
})

test_that("mark bridges", {
  suppressMessages(base_graph <- create_base_konigsberg_graph(boston))
  filtered_graph <- automobile_highways(base_graph)

  bridged_graph <- main_bridges(filtered_graph)
  expect_true("is_bridge" %in% edge_attr_names(bridged_graph))
  expect_is(edge_attr(bridged_graph, "is_bridge"), "logical")
  expect_false(anyNA(edge_attr(bridged_graph, "is_bridge")))

  bridged_graph <- all_bridges(filtered_graph)
  expect_true("is_bridge" %in% edge_attr_names(bridged_graph))
  expect_is(edge_attr(bridged_graph, "is_bridge"), "logical")
  expect_false(anyNA(edge_attr(bridged_graph, "is_bridge")))
})
