#' Convert an igraph object with lon/lat attributes to an SF linestring collection
#'
#' @param [`igraph::igraph`] object
#' @param v_lat Numeric. Latitude values for each vertex
#' @param v_lon Numeric. Longitude values for each vertex
#'
#' @return A named list with an `edges` and a `nodes` [`sf::sfc`]
#'
#' @export
graph_to_sf <- function(graph, v_lat,v_lon) {
  edges <- igraph::as_data_frame(graph, "edges")
  nodes <- igraph::as_data_frame(graph, "vertices")

  from_lat <- v_lat[match(edges$from, nodes$name)]
  from_lon <- v_lon[match(edges$from, nodes$name)]
  to_lat <- v_lat[match(edges$to, nodes$name)]
  to_lon <- v_lon[match(edges$to, nodes$name)]

  st_edges <- mapply(function(flo, fla, tlo, tla) sf::st_linestring(matrix(c(flo, fla, tlo, tla), 2, 2, byrow = TRUE)), from_lon, from_lat, to_lon, to_lat, SIMPLIFY = FALSE)
  sf::st_geometry(edges) <- sf::st_sfc(st_edges, crs = 4326)

  st_nodes <- mapply(function(lon, lat) sf::st_point(c(lon, lat)), v_lon, v_lat, SIMPLIFY = FALSE)
  sf::st_geometry(nodes) <- sf::st_sfc(st_nodes, crs = 4326)

  structure(
    list(
      edges = edges,
      vertices = nodes
    ),
    class = "sf_graph"
  )
}
