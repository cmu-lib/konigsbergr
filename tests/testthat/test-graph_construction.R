context("test-graph_construction")

test_that("extracts OSM nodes", {
  knodes <- get_kongisberger_nodes(boston)
  expect_gt(nrow(knodes), 0)

  expect_is(knodes, "tbl_df")

  expect_true(all(c("id", "lat", "lon", "label") %in% names(knodes)))

  expect_is(knodes[["id"]], "numeric")

  expect_false(anyNA(knodes[["lat"]]))
  expect_is(knodes[["lat"]], "numeric")

  expect_false(anyNA(knodes$lon))
  expect_is(knodes[["lon"]], "numeric")

  expect_is(knodes[["label"]], "character")
})

test_that("extracts OSM ways", {
  kways <- get_kongisberger_ways(boston)

  expect_is(kways, "tbl_df")
  expect_gt(nrow(kways), 0)

  expect_true(all(c("id", osm_edge_tag_keys()) %in% names(kways)))
  expect_false(anyNA(kways[["id"]]))
  expect_is(kways[["id"]], "numeric")
})

test_that("base graph construction", {
  expect_message(base_graph <- base_konigsberg_graph(boston), regexp = "complete")

  expect_is(base_graph, "base_konigsberg_graph")
  expect_is(base_graph, "tbl_graph")

  expect_equal(components(base_graph)$no, 1L)
  expect_true(all(edge_attr_names(base_graph) %in% c("id", "label", osm_edge_tag_keys(), "bridge_relation", "relation_label")))
  expect_true(all(vertex_attr_names(base_graph) %in% c("id", "lat", "lon", "label")))
  expect_true(is_directed(base_graph))
  expect_true(is_connected(base_graph))
})

test_that("filter ways", {
  suppressMessages(base_graph <- base_konigsberg_graph(boston))

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
  expect_true(all(edge_table[["highway"]] %in% c("footway", "pedestrian", "path", "primary", "secondary", "tertiary", "primary_link", "steps")))
})

test_that("mark bridges", {
  suppressMessages(base_graph <- base_konigsberg_graph(boston))
  filtered_graph <- automobile_highways(base_graph)

  bridged_graph <- main_bridges(filtered_graph)
  expect_true("is_bridge" %in% edge_attr_names(bridged_graph))
  expect_is(edge_attr(bridged_graph, "is_bridge"), "logical")
  expect_false(anyNA(edge_attr(bridged_graph, "is_bridge")))
  bridge_ids <- edge_attr(mark_bridges(bridged_graph), "bridge_id")
  expect_is(bridge_ids, "numeric")
  expect_true(any(!is.na(bridge_ids)))

  bridged_graph <- all_bridges(filtered_graph)
  expect_true("is_bridge" %in% edge_attr_names(bridged_graph))
  expect_is(edge_attr(bridged_graph, "is_bridge"), "logical")
  expect_false(anyNA(edge_attr(bridged_graph, "is_bridge")))
  bridge_ids <- edge_attr(mark_bridges(bridged_graph), "bridge_id")
  expect_is(bridge_ids, "numeric")
  expect_true(any(!is.na(bridge_ids)))
})

test_that("konigsberg_graph accepts appropriate args", {
  expect_error(konigsberg_graph(iris))
  expect_message(direct_graph <- konigsberg_graph(boston))
  expect_message(indirect_graph <- konigsberg_graph(base_konigsberg_graph(boston)))

  expect_is(direct_graph, "konigsberg_graph")
  expect_is(indirect_graph, "konigsberg_graph")
  expect_equal(ecount(direct_graph), ecount(indirect_graph))
  expect_equal(vcount(direct_graph), vcount(indirect_graph))
  expect_equal(vertex_attr_names(direct_graph), vertex_attr_names(indirect_graph))
  expect_equal(edge_attr_names(direct_graph), edge_attr_names(indirect_graph))
})

test_that("collect edge bundles", {
  suppressMessages({
    bridged_graph <- konigsberg_graph(boston)
  })

  bridge_ids <- unique(na.omit(edge_attr(bridged_graph, "bridge_id")))

  ebs <- collect_edge_bundles(bridged_graph)
  expect_length(ebs, length(bridge_ids))
  res <- lapply(ebs, function(x) expect_gt(length(x), 0))
})

test_that("select main component", {
  multi_graph <- tidygraph::play_islands(4, 10, 0.7, 0)
  single_graph <- select_main_component(multi_graph)
  expect_equal(components(single_graph)[["no"]], 1L)
})

test_that("weight by distance", {
  dnodes <- tibble::tibble(
    name = c("a", "b", "c"),
    lat = c(45.991, 45.01, 46.1),
    lon = c(31.002, 32, 31.500)
  )

  dedges <- tibble::tibble(
    from = c("a", "b", "c"),
    to = c("b", "c", "a")
  )

  dgraph <- as_tbl_graph(graph_from_data_frame(d = dedges, vertices = dnodes))
  class(dgraph) <- c(class(dgraph), "base_konigsberg_graph")

  ifrom <- 1:3
  ito <- c(2, 3, 1)
  ex_dist <- geosphere::distGeo(p1 = cbind(dnodes$lon[ifrom], dnodes$lat[ifrom]),
                                p2 = cbind(dnodes$lon[ito], dnodes$lat[ito]))

  weighted_graph <- weight_by_distance(dgraph)

  expect_true("distance" %in% edge_attr_names(weighted_graph))
  expect_equivalent(edge_attr(weighted_graph, "distance"), ex_dist)
})

test_that("can specify different filters", {
  expect_message(auto_boston <- konigsberg_graph(boston, path_filter = automobile_highways))
  expect_message(ped_boston <- konigsberg_graph(boston, path_filter = pedestrian_highways))
  expect_false(ecount(auto_boston) == ecount(ped_boston))

  expect_message(all_boston <- konigsberg_graph(boston, path_filter = automobile_highways, bridge_filter = all_bridges))
  expect_message(main_boston <- konigsberg_graph(boston, path_filter = automobile_highways, bridge_filter = main_bridges))
  expect_equal(ecount(all_boston), ecount(main_boston))
  # Need to find a better data extract where these two values will actually be different
  # expect_gt(length(unique(edge_attr(all_boston, "bridge_id"))), length(unique(edge_attr(main_boston, "bridge_id"))))
})


