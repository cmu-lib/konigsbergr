#' Calculate pathways across bridges
#'
#' These functions calculate pathways across bridges in [`konigsberg_graph`]
#' objects.
#'
#' @param graph A [`konigsberg_graph`]
#' @param starting_node An integer specifying the OSM id of the starting node.
#'   Defaults to the first vertex in `graph`.
#' @param cheat Boolean. Allow pathway to re-cross bridges?
#' @param quiet Boolean. Suppress progress messages?
#' @param ... Additional variables passed to [`greedy_search()`][pathfinder::greedy_search]
#'
#' @return A `konigsberg_path` object, which inherits from the `pathfinder_path` object from [`greedy_search()`][pathfinder::greedy_search]
#'
#' @name traverse_graph
NULL

#' @describeIn traverse_graph Cross every bridge in the graph
#' @export
cross_all_bridges <- function(graph, starting_node = NULL, cheat = TRUE, quiet = FALSE, ...) {
  stopifnot(inherits(graph, "konigsberg_graph"))
  bridge_bundles <- collect_edge_bundles(graph)
  starting_point <- calculate_starting_node(graph, starting_node)
  res <- pathfinder::greedy_search(graph, bridge_bundles, E(graph)$distance, starting_point = starting_point, cheat = cheat, quiet = quiet, ...)
  class(res) <- c(class(res), "konigsberg_path")
  res
}

#' @describeIn traverse_graph Cross only specified bridges
#' @param required_bridges The [Way](https://wiki.openstreetmap.org/wiki/Way) or
#'   [Relation](https://wiki.openstreetmap.org/wiki/Relation) ids of bridges
#'   that must be crossed.
#' @export
cross_specific_bridges <- function(graph, starting_node = NULL, required_bridges = NULL, cheat = TRUE, quiet = FALSE, ...) {
  stopifnot(inherits(graph, "konigsberg_graph"))
  bridge_bundles <- lapply(required_bridges, function(x) which(x == edge_attr(graph, "bridge_id")))
  starting_point <- calculate_starting_node(graph, starting_node)

  res <- pathfinder::greedy_search(graph,
                                   edge_bundles = bridge_bundles,
                                   distances = E(graph)$distance,
                                   starting_point = starting_point,
                                   cheat = cheat,
                                   quiet = quiet,
                                   ...)
  class(res) <- c(class(res), "konigsberg_path")
  res
}

calculate_starting_node <- function(graph, starting_node) {
  if (is.null(starting_node)) {
    starting_point <- 1
  } else {
    stopifnot(starting_node %in% vertex_attr(graph, "id"))
    starting_point <- which(vertex_attr(graph, "id") == starting_node)
  }
}
