#' Calculate a pathway that will cross every bridge
#'
#' @param graph A [`konigsberg_graph`]
#' @param starting_node An integer specifying the OSM id of the starting node. Defaults to the first vertex in `graph`.
#' @param ... Additional variables passed to [`greedy_search()`][pathfinder::greedy_search]
#'
#' @return A `konigsberg_path` object, which inherits from the `pathfinder_path` object from [`greedy_search()`][pathfinder::greedy_search]
#'
#' @export
cross_all_bridges <- function(graph, starting_node = NULL, ...) {
  stopifnot(inherits(graph, "konigsberg_graph"))

  bridge_bundles <- collect_edge_bundles(graph)

  if (is.null(starting_node)) {
    starting_point <- 1
  } else {
    stopifnot(starting_node %in% vertex_attr(graph, "id"))
    starting_point <- which(vertex_attr(graph, "id") == starting_node)
  }

  res <- pathfinder::greedy_search(graph,
                edge_bundles = bridge_bundles,
                distances = E(graph)$distance,
                starting_point = starting_point,
                ...)
  class(res) <- c(class(res), "konigsberg_path")
  res
}
