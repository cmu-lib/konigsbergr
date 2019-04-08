#' Convert an igraph object with lon/lat attributes to an SF linestring collection
#'
#' @param graph An [`igraph`][igraph::igraph] object
#' @param v_lat Numeric. Latitude values for each vertex
#' @param v_lon Numeric. Longitude values for each vertex
#'
#' @return A named list with an `edges` and a `nodes` [`sfc`][sf::sfc] object.
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

#' @describeIn graph_to_sf Return only the edges as an [`st_linestring`][sf::st_linestring] object
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

#' @describeIn graph_to_sf Return only the nodes as an [`st_point`][sf::st_point] object
#' @export
nodes_to_sf <- function(graph, v_lat = igraph::vertex_attr(graph, "lat"), v_lon = igraph::vertex_attr(graph, "lon")) {
  nodes <- igraph::as_data_frame(graph, "vertices")

  st_nodes <- mapply(function(lon, lat) sf::st_point(c(lon, lat)), v_lon, v_lat, SIMPLIFY = FALSE)
  sf::st_geometry(nodes) <- sf::st_sfc(st_nodes, crs = 4326)

  nodes
}

#' Create an sf object from a konigsberg path
#'
#' Generates an [`sf`][sf::sf] collection of line strings representing the pathway
#' taken over the map.
#'
#' @param graph A [`konigsberg_graph`]
#' @param pathway A `konigsberg_pathway` resulting from [`cross_all_bridges`]
#'
#' @return A list with two elements:
#'     - `pathway` An [`sf::sf`] data frame with LINESTRING features
#'     representing the pathway
#'     - `terminals` An [`sf::sf`] data frame with two POINT features
#'     representing the start and end nodes of the path.
#'
#' @importFrom dplyr group_by mutate ungroup bind_cols case_when
#' @importFrom rlang .data
#' @importFrom utils head tail
#'
#' @export
pathway_to_sf <- function(graph, pathway) {
  stopifnot(inherits(graph, "konigsberg_graph"))
  stopifnot(inherits(pathway, "konigsberg_path"))

  edges_sf <- edges_to_sf(graph) %>%
    mutate(
      osm_url = case_when(
        !is.na(.data$bridge_id) ~ glue::glue("https://openstreetmap.org/way/{bridge_id}"),
        !is.na(.data$bridge_relation) ~ glue::glue("https://openstreetmap.org/relation/{bridge_relation}"),
        TRUE ~ glue::glue("https://openstreetmap.org/way/{id}")
      )
    )

  augmented_pathway <- pathfinder::augment(pathway) %>%
    group_by(.data$bundle_id) %>%
    mutate(total_times_bridge_crossed = max(.data$times_bundle_crossed)) %>%
    ungroup()

  pathway_sf <- bind_cols(edges_sf[augmented_pathway$edge_id,], augmented_pathway)

  # Get start and end point and add to map
  start_point <- head(head(pathway$vpath, 1)[[1]], 1)
  end_point <- tail(tail(pathway$vpath, 1)[[1]], 1)

  nodes_sf <- nodes_to_sf(graph)[c(start_point, end_point),]
  nodes_sf$start <- c("Beginning", "End")

  structure(list(
    pathway = pathway_sf,
    terminals = nodes_sf),
    class = c("list", "konigsberg_sf"))
}

#' Plot bridge crossing pathway on a Leaflet map
#'
#' This transforms the resulting pathway into an sf project and plots it onto a Leaflet map.
#'
#' @inheritParams pathway_to_sf
#'
#' @return A [`leaflet`][leaflet::leaflet] object
#'
#' @import leaflet
#'
#' @export
view_konigsberg_path <- function(graph, pathway) {
  path_sf <- pathway_to_sf(graph, pathway)

  cross_pal <- colorFactor(c("#2B83BA", "#ABDDA4", "#FDAE61"),
                           path_sf$pathway$total_times_bridge_crossed)

  lf <- leaflet(path_sf$pathway, width = "100%", height = "600px") %>%
    addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
    addPolylines(opacity = 0.6, color = ~cross_pal(total_times_bridge_crossed),
                 label = ~label, popup = ~glue::glue("<a href='{osm_url}'>{osm_url}</a>")) %>%
    addCircleMarkers(data = path_sf$terminals, label = ~start, color = c("blue", "red")) %>%
    addLegend("topright", pal = cross_pal, values = ~total_times_bridge_crossed, title = "Times bridge has been crossed",
              na.label = "Non-bridge")

  lf
}
