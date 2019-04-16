#' Simplify a Konigsbergr Graph for Network Analysis
#'
#' This function simplifies a Konigsbergr graph by reducing the
#' graph to a simpler representation where intersections are directly connected
#' by straight edges.
#'
#' Raw Konigsbergr graphs contain a huge number of nodes in order to accurately
#' describe the geometry of curved roads. For many network analyses, however,
#' road curvature is unimportant compared to the toplogocal layout of
#' intersections. Simplifying OSM-based road graphs like this can reduce the
#' size of a graph by up to 90%, greatly speeding many analyses. The additional
#' nodes needed to accuratley encode complete spatial information can interfere
#' with some common calculations such as [closeness()][igraph::closeness], so
#' simplifying the graph like this can also return more accurate metrics.
#'
#' Beware that this simplifcation also reduces the data to a simple, undirected
#' graph, so note that the distinction between two-way and one-way roads will be
#' discarded. The sum of the total distances of the removed edges are
#' maintained, however, so the real-world distance in meters of an edge will be
#' saved in the `distance` edge attribute of the new graph.
#'
#' @seealso This function relies on the
#'   [simplify_topology()][simplygraph::simplify_topology] function from the
#'   simplygraph package.
#'
#' @param graph A Konigsbergr graph created from [konigsbergr_graph()].
#' @param preserve_bridges Boolean. Preserve full bridge geometry?
#'
#' @return A simple, undirected [`igraph`][igraph::igraph] object.
#' @export
#' @examples
#' \dontrun{
#' boston_konigsbergr <- konigsberg_graph(boston)
#'
#' boston_sf <- graph_to_sf(boston_konigsbergr)
#' plot(boston_sf$edges["geometry"])
#'
#' simple_boston <- simplify_konigsbergr(boston_konigsbergr)
#' simple_boston_sf <- graph_to_sf(simple_boston)
#' plot(simple_boston_sf$edges["geometry"])
#' }
simplify_konigsbergr <- function(graph, preserve_bridges = TRUE) {
  # Simplygraph only takes simple, undirected graphs
  ud_graph <- igraph::as.undirected(graph, mode = "collapse",
                                    edge.attr.comb = list("first"))

  bridge_nodes <- NULL
  if (preserve_bridges) {
    bridge_bundles <- collect_edge_bundles(graph)
    pathway_graph <- pathfinder::decorate_graph(graph, bridge_bundles, edge_attr(graph, "distance"))
    bridge_nodes <- which(vertex_attr(pathway_graph, "pathfinder.interface"))
  }

  # Sum road distances when merging edges;
  simplygraph::simplify_topology(
    ud_graph,
    edge_attr_comb = list(distance = sum,
                          .default.combiner = simplygraph::first),
    protected_nodes = bridge_nodes)
}
