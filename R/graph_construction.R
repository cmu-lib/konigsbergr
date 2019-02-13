# Constructing a road/bridge graph from an osmar object ----

# Extract a table of OSM nodes bearing IDs, lat, lon, and label
get_kongisberger_nodes <- function(src) {
  stopifnot(inherits(src, "osmar"))

  base_attrs <- src$nodes$attrs %>%
    select(id, lat, lon) %>%
    as_tibble()

  node_tags <- src$nodes$tags %>%
    as_tibble() %>%
    filter(k == "name") %>%
    mutate_at(vars(v), as.character) %>%
    spread(k, v, drop = TRUE) %>%
    # To avoid collision with the "name" id used by igraph/tidygraph, use the
    # term "label" for OSM name
    rename(label = name)

  base_attrs %>%
    left_join(node_tags, by = "id")
}

# Extract a table of OSM Ways bearing ids, labels, and selected tag values
#' @import dplyr tidyr
get_kongisberger_ways <- function(src) {
  stopifnot(inherits(src, "osmar"))

  way_tags <- src$ways$tags %>%
    as_tibble() %>%
    filter(k %in% c(osm_edge_tag_keys(), "name")) %>%
    mutate_at(vars(v), as.character) %>%
    spread(k, v, drop = TRUE) %>%
    # To avoid collision with the "name" id used by igraph/tidygraph, use the
    # term "label" for OSM feature name
    rename(label = name)

  # Collect parent bridge relations
  relation_tags <- src$relations$tags %>%
    as_tibble() %>%
    filter(k == "type", v == "bridge") %>%
    left_join(src$relations$refs, by = "id") %>%
    filter(type == "way") %>%
    select(id = ref, bridge_relation = id) %>%
    distinct(id, .keep_all = TRUE)

  way_tags %>%
    left_join(relation_tags, by = "id")
}

#' Create a road and bridge network from OSM data
#'
#' Creates a base graph object with the appropriate edge and vertex attributes from OSM.
#'
#' @param src An [`osmar::osmar`] object
#' @param path_filter A function that filters which Ways will be traversable in the graph. See [`way_filters`].
#' @param bridge_filter A function that marks which Ways are bridges that need to be crossed. See [`bridge_filters`].
#'
#' @return A [`konigsberg_graph`] object.
#'
#' @importFrom magrittr %>%
#' @export
konigsberg_graph <- function(src, path_filter = automobile_highways, bridge_filter = all_bridges) {
  k_graph <- create_base_konigsberg_graph(src)

  message("Filtering graph to desired paths and bridges...", appendLF = FALSE)
  marked_graph <- k_graph %>%
    path_filter() %>%
    bridge_filter() %>%
    mark_bridges() %>%
    weight_by_distance() %>%
    reciprocal_two_way_streets()
  message("complete!")

  marked_graph
}

#' @importFrom rlang .data
create_base_konigsberg_graph <- function(src) {
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

  class(graph) <- c(class(graph), "konigsberg_graph")
  graph
}

#' Get a list of edge lists that represent multi-edge bridges
#'
#' @param A [`konigsberg_graph`] object
#'
#' @return A list of integer vectors representing edge indices
#'
#' @export
collect_edge_bundles <- function(graph) {
  stopifnot(inherits(graph, "konigsberg_graph"))
  all_bridge_ids <- igraph::edge_attr(graph, "bridge_id")
  unique_relation_ids <- unique(na.omit(all_bridge_ids))
  lapply(unique_relation_ids, function(x) which(x == all_bridge_ids))
}

# Graph utilities ----

# Add reverse edges of non-one-way streets
reciprocal_two_way_streets <- function(graph) {
  stopifnot(inherits(graph, "konigsberg_graph"))

  reversed_edges <- graph %>%
    as_tibble("edges") %>%
    filter(is.na(oneway) | oneway != "yes") %>%
    select(from = to, to = from, everything())
  graph <- bind_edges(graph, reversed_edges)
}

# Remove all isolated nodes in a graph
remove_unreachable_nodes <- function(graph) {
  stopifnot(inherits(graph, "tbl_graph"))

  graph %>%
    activate(nodes) %>%
    filter(!(node_is_isolated()))
}

# Keep only the biggest connected component of a graph
select_main_component <- function(graph) {
  stopifnot(inherits(graph, "tbl_graph"))

  graph %>%
    activate(nodes) %>%
    mutate(component = group_components()) %>%
    filter(component == 1) %>%
    select(-component)
}

# Weights by carteisan distance of from and to nodes
# the "conversion" factor is
weight_by_distance <- function(graph) {
  stopifnot(inherits(graph, "konigsberg_graph"))

  graph %>%
    activate(edges) %>%
    mutate(distance = geosphere::distGeo(
      p1 = cbind(.N()$lon[from], .N()$lat[from]),
      p2 = cbind(.N()$lon[to], .N()$lat[to])))
}
