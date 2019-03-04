#' Calculate a pathway that will cross every bridge
#'
#' @param graph A [`konigsberg_graph`]
#' @param starting_node An integer specifying the OSM id of the starting node. Defaults to the first vertex in `graph`.
#' @param ... Additional variables passed to [`pathfinder::greedy_search`]
#'
#' @return A `pathfinder_path` object
#'
#' @export
cross_all_bridges <- function(graph, starting_node = vertex_attr(graph, "id", 1), ...) {
  stopifnot(inherits(graph, "konigsberg_graph"))
  stopifnot(starting_node %in% vertex_attr(graph, "id"))
  bridge_bundles <- collect_edge_bundles(graph)

  pathfinder::greedy_search(graph,
                edge_bundles = bridge_bundles,
                distances = E(graph)$distance,
                ...)
}
