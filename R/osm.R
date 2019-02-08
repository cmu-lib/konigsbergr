# Data utilities ----

get_osm_bbox <- function(xlim, ylim) {
  content(GET(str_glue("https://overpass-api.de/api/map?bbox={xlim[1]},{ylim[1]},{xlim[2]},{ylim[2]}")), as = "text", encoding = "UTF-8")
}

read_osm_response <- function(raw_response) {
  tfile <- tempfile()
  write_lines(raw_response, path = tfile)
  get_osm(complete_file(), source = osmsource_file(tfile))
}
