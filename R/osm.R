# Data utilities ----

get_osm_bbox <- function(xlim, ylim) {
  content(GET(str_glue("https://overpass-api.de/api/map?bbox={xlim[1]},{ylim[1]},{xlim[2]},{ylim[2]}")), as = "text", encoding = "UTF-8")
}

read_osm_response <- function(raw_response) {
  tfile <- tempfile()
  write_lines(raw_response, path = tfile)
  get_osm(complete_file(), source = osmsource_file(tfile))
}

# Convert OSM nodes to sf so that we can determine which ones are withi the PGH
# administrative boundary
osm_nodes_to_sf <- function(osm) {
  nodes_sf <- cbind(osm$nodes$attrs$lon, osm$nodes$attrs$lat) %>%
    st_multipoint() %>%
    st_sfc(crs = 4326) %>%
    st_cast("POINT")

  res <- select(osm$nodes$attrs, id)
  st_geometry(res) <- nodes_sf
  res
}

nodes_within_boundaries <- function(nodes, boundaries) {
  res <- st_intersects(nodes, boundaries, sparse = FALSE) %>%
    apply(1, any)

  nodes$id[which(res)]
}

