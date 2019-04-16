# Define which OSM metadata key-value pairs are relevant

osm_edge_tag_keys <- function() c("access", "highway", "bicycle", "foot", "bridge", "oneway")

#' OSM Way filters
#'
#' A variety of presets for filtering down a full OSM dataset to the ways that
#' describe transportation. This primarily filters on the value of the
#' [`highway`](https://wiki.openstreetmap.org/wiki/Key:highway) tag's value.
#'
#' @seealso When using [get_osm_file()], note that the underlying
#'   [read_big_osm()][bigosm::read_big_osm] function automatically subsets the
#'   network to Ways that contain a `highway` key.
#'
#' @param graph A [`konigsberg_graph`] object.
#'
#' @return An integer vector of OSM Way ids.
#'
#' @importFrom rlang .data
#' @name way_filters
NULL

#' @describeIn way_filters Ways that are navigable in an automobile
#' @export
automobile_highways <- function(graph) {
  stopifnot(inherits(graph, "base_konigsberg_graph"))

  graph %>%
    tidygraph::activate(edges) %>%
    dplyr::filter(
      (.data$highway %in% c("residential",
                      "tertiary",
                      "primary",
                      "secondary",
                      "motorway_link",
                      "unclassified",
                      "motorway",
                      "trunk",
                      "primary_link",
                      "trunk_link",
                      "tertiary_link",
                      "secondary_link")),
      (is.na(.data$access) | .data$access != "no")
    ) %>%
    select_main_component()
}

#' @describeIn way_filters Ways that are navigable by foot
#' @export
pedestrian_highways <- function(graph) {
  stopifnot(inherits(graph, "base_konigsberg_graph"))

  graph %>%
    tidygraph::activate(edges) %>%
    dplyr::filter(
      (.data$highway %in% c("footway", "pedestrian", "path", "primary", "secondary", "tertiary", "primary_link")),
      (is.na(.data$foot) | .data$foot != "no")
    ) %>%
    select_main_component()
}

#' OSM Bridge filters
#'
#' Preset filtering functions to define which Ways in the OSM data will be counted as bridges to be crossed
#'
#' @inheritParams way_filters
#'
#' @aliases bridge_filter
#' @name bridge_filters
NULL

#' @describeIn bridge_filters Include all OSM bridges
#' @export
all_bridges <- function(graph) {
  stopifnot(inherits(graph, "base_konigsberg_graph"))
  stopifnot("bridge" %in% edge_attr_names(graph))

  graph %>%
    tidygraph::activate(edges) %>%
    dplyr::mutate(
      is_bridge = dplyr::case_when(
        .data$bridge == "yes" ~ TRUE,
        # When in doubt, it's not a bridge
        TRUE ~ FALSE))
}

#' @describeIn bridge_filters Only use major bridges, excluding any on/off ramps
#' @export
main_bridges <- function(graph) {
  stopifnot(inherits(graph, "base_konigsberg_graph"))
    stopifnot("highway" %in% edge_attr_names(graph))

  graph %>%
    tidygraph::activate(edges) %>%
    dplyr::mutate(
      is_bridge = dplyr::case_when(
        .data$bridge == "yes" & !(.data$highway %in% c("motorway_link", "trunk_link", "primary_link", "secondary_link", "tertiary_link")) ~ TRUE,
        # When in doubt, it's not a bridge
        TRUE ~ FALSE))
}

mark_bridges <- function(graph) {
  stopifnot(inherits(graph, "base_konigsberg_graph"))
  stopifnot("is_bridge" %in% edge_attr_names(graph))

  graph %>%
    tidygraph::activate(edges) %>%
    dplyr::mutate(
      bridge_id = dplyr::case_when(
        .data$is_bridge & !is.na(.data$bridge_relation) ~ .data$bridge_relation,
        .data$is_bridge ~ .data$id,
        TRUE ~ NA_real_
      )
    )
}
