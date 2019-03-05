# Constructing a road/bridge graph from an osmar object ----

# Extract a table of OSM nodes bearing IDs, lat, lon, and label
#' @importFrom rlang .data
#' @importFrom tibble as_tibble
#' @importFrom dplyr select filter mutate_at rename left_join
#' @importFrom tidyr spread
get_kongisberger_nodes <- function(src) {
  stopifnot(inherits(src, "osmar"))

  base_attrs <- src$nodes$attrs %>%
    select(.data$id, .data$lat, .data$lon) %>%
    as_tibble()

  node_tags <- src$nodes$tags %>%
    as_tibble() %>%
    filter(.data$k == "name") %>%
    mutate_at(vars(.data$v), as.character) %>%
    spread(.data$k, .data$v, drop = TRUE) %>%
    # To avoid collision with the "name" id used by igraph/tidygraph, use the
    # term "label" for OSM name
    rename(label = .data$name)

  base_attrs %>%
    left_join(node_tags, by = "id")
}

# Extract a table of OSM Ways bearing ids, labels, and selected tag values
#' @importFrom rlang .data
#' @importFrom tibble as_tibble
#' @importFrom dplyr filter mutate_at rename left_join select distinct vars
#' @importFrom tidyr spread
get_kongisberger_ways <- function(src) {
  stopifnot(inherits(src, "osmar"))

  way_tags <- src$ways$tags %>%
    as_tibble() %>%
    filter(.data$k %in% c(osm_edge_tag_keys(), "name")) %>%
    mutate_at(vars(.data$v), as.character) %>%
    spread(.data$k, .data$v, drop = TRUE) %>%
    # To avoid collision with the "name" id used by igraph/tidygraph, use the
    # term "label" for OSM feature name
    rename(label = .data$name)

  # Collect parent bridge relations
  relation_tags <- src$relations$tags %>%
    as_tibble() %>%
    filter(.data$k == "type", .data$v == "bridge") %>%
    left_join(src$relations$refs, by = "id") %>%
    filter(.data$type == "way") %>%
    select(id = .data$ref, bridge_relation = .data$id) %>%
    distinct(.data$id, .keep_all = TRUE)

  relation_labels <- src$relations$tags %>%
    as_tibble() %>%
    filter(.data$k == "name") %>%
    select(bridge_relation = .data$id, relation_label = .data$v)

  way_tags %>%
    left_join(relation_tags, by = "id") %>%
    left_join(relation_labels, by = "bridge_relation")
}

#' Create a road and bridge network from OSM data
#'
#' Creates a base graph object with the appropriate edge and vertex attributes from OSM.
#'
#' @param src An [`osmar::osmar`] or a [`base_konigsberg_graph`] object.
#' @param path_filter A function that filters which Ways will be traversable in the graph. See [`way_filters`].
#' @param bridge_filter A function that marks which Ways are bridges that need to be crossed. See [`bridge_filters`].
#'
#' @return A [`konigsberg_graph`] object.
#'
#' @importFrom magrittr %>%
#' @export
konigsberg_graph <- function(src, path_filter = automobile_highways, bridge_filter = all_bridges) {
  # If src already has the base konigsberg transform, move on immediately, otherwise apply it
  if (inherits(src, "base_konigsberg_graph")) {
    k_graph <- src
  } else if (inherits(src, "osmar")) {
    k_graph <- base_konigsberg_graph(src)
  } else {
    stop("src must be either an 'osmar' or a 'base_konigsberg_graph' object")
  }

  message("Filtering graph to desired paths and bridges...", appendLF = FALSE)
  marked_graph <- k_graph %>%
    path_filter() %>%
    bridge_filter() %>%
    mark_bridges() %>%
    weight_by_distance() %>%
    reciprocal_two_way_streets()
  message("complete!")

  class(marked_graph) <- c(class(marked_graph), "konigsberg_graph")
  marked_graph
}

#' Create an intermediate graph representation of OSM data
#'
#' This function is usually called by [`konigsberg_graph`]. It transforms the
#' OSM data into a graph object and attaches necessary attributes. It is
#' publicly exported, however, because it is an expensive operation on large
#' datasets. If you are trying to calculate a pathway for a large city and would
#' like to try several combinations of road and bridge filters, then you may
#' wish to run this function yourself and save the result, then pass it on to
#' [`konigsberg_graph`] to complete the annotation of the graph.
#'
#' @param src An [`osmar::osmar`] object
#'
#' @importFrom rlang .data
#' @importFrom dplyr rename mutate_at left_join select
#' @importFrom tidygraph as_tbl_graph activate
#'
#' @export
base_konigsberg_graph <- function(src) {
  stopifnot(inherits(src, "osmar"))

  message("Creating base graph...", appendLF = FALSE)
  base_graph <- osmar::as_igraph(src)
  message("complete!")

  message("Adding OSM attributes...", appendLF = FALSE)
  graph <- as_tbl_graph(base_graph, directed = TRUE) %>%
    activate(nodes) %>%
    rename(id = .data$name) %>%
    mutate_at(vars(.data$id), as.numeric) %>%
    left_join(get_kongisberger_nodes(src), by = "id") %>%
    activate(edges) %>%
    rename(id = .data$name) %>%
    select(-.data$weight) %>%
    left_join(get_kongisberger_ways(src), by = "id") %>%
    select_main_component()
  message("complete!")

  class(graph) <- c(class(graph), "base_konigsberg_graph")
  graph
}

#' Get a list of edge lists that represent multi-edge bridges
#'
#' @param graph A [`konigsberg_graph`] object
#'
#' @return A list of integer vectors representing edge indices
#'
#' @export
collect_edge_bundles <- function(graph) {
  stopifnot(inherits(graph, "konigsberg_graph"))
  all_bridge_ids <- igraph::edge_attr(graph, "bridge_id")
  unique_relation_ids <- unique(stats::na.omit(all_bridge_ids))
  lapply(unique_relation_ids, function(x) which(x == all_bridge_ids))
}

# Graph utilities ----

# Add reverse edges of non-one-way streets
#' @importFrom rlang .data
#' @importFrom dplyr filter select everything
reciprocal_two_way_streets <- function(graph) {
  stopifnot(inherits(graph, "base_konigsberg_graph"))

  reversed_edges <- graph %>%
    as_tibble("edges") %>%
    filter(is.na(.data$oneway) | .data$oneway != "yes") %>%
    select(from = .data$to, to = .data$from, everything())
  graph <- tidygraph::bind_edges(graph, reversed_edges)
}

# Remove all isolated nodes in a graph
#' @importFrom tidygraph as_tbl_graph activate
remove_unreachable_nodes <- function(graph) {
  stopifnot(inherits(graph, "tbl_graph"))

  graph %>%
    activate(nodes) %>%
    filter(!(tidygraph::node_is_isolated()))
}

#' Keep only the biggest connected component of a graph
#'
#' @param graph A [`tidygraph::tbl_graph`]
#'
#' @return A [`tidygraph::tbl_graph`] with one component
#'
#' @importFrom tidygraph as_tbl_graph activate
#' @importFrom dplyr filter select mutate
#' @importFrom rlang .data
#' @export
select_main_component <- function(graph) {
  stopifnot(inherits(graph, "tbl_graph"))

  graph %>%
    activate(nodes) %>%
    mutate(component = tidygraph::group_components()) %>%
    filter(.data$component == 1) %>%
    select(-.data$component)
}

# Weights by Haversine distance between nodes
#' @importFrom tidygraph .N activate
#' @importFrom rlang .data
weight_by_distance <- function(graph) {
  stopifnot(inherits(graph, "base_konigsberg_graph"))

  graph %>%
    activate(edges) %>%
    dplyr::mutate(distance = geosphere::distGeo(
      p1 = cbind(.N()$lon[.data$from], .N()$lat[.data$from]),
      p2 = cbind(.N()$lon[.data$to], .N()$lat[.data$to])))
}
