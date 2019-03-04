#' Convert an igraph object with lon/lat attributes to an SF linestring collection
#'
#' @param graph An [`igraph::igraph`] object
#' @param v_lat Numeric. Latitude values for each vertex
#' @param v_lon Numeric. Longitude values for each vertex
#'
#' @return A named list with an `edges` and a `nodes` [`sf::sfc`]
#'
#' @export
graph_to_sf <- function(graph, v_lat = igraph::vertex_attr(graph, "lat"), v_lon = igraph::vertex_attr(graph, "lon")) {
  edges <- edges_to_sf(graph, v_lat, v_lon)
  nodes <- nodes_to_sf(graph, v_lat, v_lon)

  structure(
    list(
      edges = edges,
      vertices = nodes
    ),
    class = "sf_graph"
  )
}

#' @describeIn graph_to_sf Return only the edges as [`sf::st_linestring`]
#' @export
edges_to_sf <- function(graph, v_lat = igraph::vertex_attr(graph, "lat"), v_lon = igraph::vertex_attr(graph, "lon")) {
  edges <- igraph::as_data_frame(graph, "edges")

  froms <- tail_of(graph, es = E(graph))
  tos <- head_of(graph, es = E(graph))

  from_lat <- v_lat[froms]
  from_lon <- v_lon[froms]
  to_lat <- v_lat[tos]
  to_lon <- v_lon[tos]

  st_edges <- mapply(function(flo, fla, tlo, tla) sf::st_linestring(matrix(c(flo, fla, tlo, tla), 2, 2, byrow = TRUE)), from_lon, from_lat, to_lon, to_lat, SIMPLIFY = FALSE)
  sf::st_geometry(edges) <- sf::st_sfc(st_edges, crs = 4326)

  edges
}

#' @describeIn graph_to_sf Return only the nodes as [`sf::st_point`]
#' @export
nodes_to_sf <- function(graph, v_lat = igraph::vertex_attr(graph, "lat"), v_lon = igraph::vertex_attr(graph, "lon")) {
  nodes <- igraph::as_data_frame(graph, "vertices")

  st_nodes <- mapply(function(lon, lat) sf::st_point(c(lon, lat)), v_lon, v_lat, SIMPLIFY = FALSE)
  sf::st_geometry(nodes) <- sf::st_sfc(st_nodes, crs = 4326)

  nodes
}

#' Create an sf object from a konigsberg path
#'
#' Generates an [sf::sf] collection of line strings representing the pathway
#' taken over the map. Can be visualized using base plotting functions or a
#' library such as [mapview::mapview]
#'
#' @param graph A [`konigsberg_graph`]
#' @param pathway A `konigsberg_pathway` resulting from [`cross_all_bridges`]
#'
#' @return An object.
#'
#' @importFrom dplyr group_by mutate ungroup bind_cols
#' @importFrom rlang .data
#'
#' @export
pathway_to_sf <- function(graph, pathway) {
  stopifnot(inherits(graph, "konigsberg_graph"))
  stopifnot(inherits(pathway, "konigsberg_path"))

  edges_sf <- edges_to_sf(graph, V(graph)$lat, V(graph)$lon) %>%
    dplyr::mutate(osm_url = glue::glue("https://openstreetmap.org/way/{id}"))

  augmented_pathway <- augment(pathway) %>%
    group_by(.data$bundle_id) %>%
    mutate(total_times_bridge_crossed = max(.data$times_bundle_crossed)) %>%
    ungroup()

  bind_cols(edges_sf[augmented_pathway$edge_id,], augmented_pathway)
}

plot.konigsberg_path <- function(graph, pathway, ...) {
  path_sf <- pathway_to_sf(graph, pathway)
  mapview(path_sf)
}
