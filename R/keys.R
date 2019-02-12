# Define which OSM metadata key-value pairs are relevant

osm_edge_tag_keys <- function() c("access", "highway", "bicycle", "foot", "bridge", "oneway")

#' OSM Way filters
#'
#' A variety of presets that will
#'
#' @param graph An [`konigsberg_graph`] object.
#'
#' @return An integer vector of OSM Way ids.
#'
#' @import dplyr
#' @name way_filters
NULL

#' @describeIn way_filters Ways that are navigable in an automobile
#' @export
automobile_highways <- function(graph) {
  stopifnot(inherits(graph, "konigsberg_graph"))

  graph %>%
    activate(edges) %>%
    filter(
      (highway %in% c("residential",
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
      (is.na(access) | access != "no")
    ) %>%
    select_main_component()
}

#' @describeIn way_filters Ways that are navigable by foot
#' @export
pedestrian_highways <- function(graph) {
  stopifnot(inherits(graph, "konigsberg_graph"))

  graph %>%
    activate(edges) %>%
    filter(
      (highway %in% c("footway", "pedestrian", "path", "primary", "secondary", "tertiary", "primary_link")),
      (is.na(foot) | foot != "no")
    ) %>%
    select_main_component()
}

#' OSM Bridge filters
#'
#' Preset filtering functions to define which Ways in the OSM data will be counted as bridges to be crossed
#'
#' @inheritParams way_filters
#'
#' @import dplyr
#' @name bridge_filters
NULL

#' @describeIn bridge_filters Include all OSM bridges
#' @export
all_bridges <- function(graph) {
  stopifnot(inherits(graph, "konigsberg_graph"))

  graph %>%
    activate(edges) %>%
    mutate(
      is_bridge = case_when(
        bridge == "yes" ~ TRUE,
        # When in doubt, it's not a bridge
        TRUE ~ FALSE))
}

#' @describeIn bridge_filters Only use major bridges, excluding any on/off ramps
#' @export
main_bridges <- function(graph) {
  stopifnot(inherits(graph, "konigsberg_graph"))

  graph %>%
    activate(edges) %>%
    mutate(
      is_bridge = case_when(
        bridge == "yes" & !(highway %in% c("motorway_link", "trunk_link", "primary_link", "secondary_link", "tertiary_link")) ~ TRUE,
        # When in doubt, it's not a bridge
        TRUE ~ FALSE))
}

mark_bridges <- function(graph) {
  stopifnot(inherits(graph, "konigsberg_graph"))
  stopifnot("is_bridge" %in% edge_attr_names(graph))

  graph %>%
    activate(edges) %>%
    mutate(
      bridge_id = case_when(
        is_bridge & !is.na(bridge_relation) ~ bridge_relation,
        is_bridge ~ id,
        TRUE ~ NA_real_
      )
    )
}
