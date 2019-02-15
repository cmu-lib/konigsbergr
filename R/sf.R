#' Convert an igraph object with lon/lat attributes to an SF linestring collection
#'
#' @param [`igraph::igraph`] object
#' @param v_lat Numeric. Latitude values for each vertex
#' @param v_lon Numeric. Longitude values for each vertex
#'
#' @return A named list with an `edges` and a `nodes` [`sf::sfc`]
#'
#' @export
graph_to_sf <- function(graph, v_lat, v_lon) {
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

#' @describeIn graph_to_sf Return only the edges as [`sf::sfc_linestring`]
#' @export
edges_to_sf <- function(graph, v_lat, v_lon) {
  edges <- igraph::as_data_frame(graph, "edges")

  from_lat <- v_lat[edges$from]
  from_lon <- v_lon[edges$from]
  to_lat <- v_lat[edges$to]
  to_lon <- v_lon[edges$to]

  st_edges <- mapply(function(flo, fla, tlo, tla) sf::st_linestring(matrix(c(flo, fla, tlo, tla), 2, 2, byrow = TRUE)), from_lon, from_lat, to_lon, to_lat, SIMPLIFY = FALSE)
  sf::st_geometry(edges) <- sf::st_sfc(st_edges, crs = 4326)

  edges
}

#' @describeIn graph_to_sf Return only the nodes as [`sf::sfc_point`]
#' @export
nodes_to_sf <- function(graph, v_lat, v_lon) {
  nodes <- igraph::as_data_frame(graph, "vertices")

  st_nodes <- mapply(function(lon, lat) sf::st_point(c(lon, lat)), v_lon, v_lat, SIMPLIFY = FALSE)
  sf::st_geometry(nodes) <- sf::st_sfc(st_nodes, crs = 4326)

  nodes
}
