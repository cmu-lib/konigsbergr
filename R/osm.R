# Data utilities ----

#' Download OSM data from a specified lon/lat bounding box
#'
#' Downloads OSM data from an [Overpass API server](http://overpass-api.de) and reads it into an [`osmar`] object.
#'
#' @seealso [`osmar::osmsource_api`] which iplements the same function but has stricter controls on the size that can be downloaded.
#'
#' @param xmin, xmax, ymin, ymax Numeric. Minimum and maximum latitude and longitude
#' @param server Character. Hostname of an [Overpass API server](http://overpass-api.de) Defaults to <https://overpass-api.de> although you may specify a different hostname.
#'
#' @return An [`osmar`] object.
#' @export
get_osm_bbox <- function(xmin, xmax, ymin, ymax, server = "https://overpass-api.de") {
  tfile <- tempfile()
  message("Downloading...", appendLF = FALSE)
  res <- httr::GET(glue::glue("{server}/api/map?bbox={xmin},{ymin},{xmax},{ymax}"), httr::write_disk(tfile), httr::progress())
  message("complete!")

  message("Reading XML...", appendLF = FALSE)
  osm_res <- osmar::get_osm(osmar::complete_file(), source = osmar::osmsource_file(tfile))
  message("complete!")

  unlink(tfile)

  osm_res
}
