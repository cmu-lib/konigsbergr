# Data utilities ----

#' Download OSM data from a specified lon/lat bounding box
#'
#' Downloads OSM data from an [Overpass API server](http://overpass-api.de) and reads it into an [`osmar`][osmar::osmar] object.
#'
#' @seealso [`osmsource_api()`][osmar::osmsource_api] which implements the same function but has stricter controls on the size that can be downloaded.
#'
#' @param xmin Numeric. Minimum longitude
#' @param xmax Numeric. Maximum longitude
#' @param ymin Numeric. Minimum latitude
#' @param ymax Numeric. Maximum latitude
#'
#' @import osmar
#'
#' @param server Character. Hostname of an [Overpass API server](http://overpass-api.de) Defaults to <https://overpass-api.de> although you may specify a different hostname.
#'
#' @return An [`osmar`][osmar::osmar] object.
#' @export
get_osm_bbox <- function(xmin, xmax, ymin, ymax, server = "https://overpass-api.de") {
  tfile <- tempfile()
  message("Downloading...", appendLF = FALSE)
  res <- httr::GET(glue::glue("{server}/api/map?bbox={xmin},{ymin},{xmax},{ymax}"), httr::write_disk(tfile))
  message("complete!")

  message("Reading XML...", appendLF = FALSE)
  osm_res <- get_osm_file(tfile)
  message("complete!")

  assertthat::assert_that(dim(osm_res)[1] > 0, msg = "There are no nodes in the downloaded OSM. Check that you have supplied a valid bounding box.")
  assertthat::assert_that(dim(osm_res)[2] > 0, msg = "There are no ways in the downloaded OSM. Check that you have supplied a valid bounding box.")

  message(glue::glue("Downloaded an OSM extract with {dim(osm_res)[1]} nodes and {dim(osm_res)[2]} ways."))

  unlink(tfile)

  osm_res
}

#' @describeIn get_osm_bbox Read in OSM data from an XML file
#' @param file Filepath for an OSM XML file
#' @export
get_osm_file <- function(file) {
  bigosm::read_big_osm(file, way_keys = "highway")
}
