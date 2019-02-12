
<!-- README.md is generated from README.Rmd. Please edit that file -->

# konigsbergr

The goal of konigsbergr is to recreate the “Bridges of Königsberg”
problem over any city by converting data from OpenStreetMap into a graph
and traversing it.

## Installation

You can install the development version:

``` r
devtools::install_github("dSHARP-CMU/konigsbergr")
```

## Usage

Get OSM data

Project it into a graph and identify bridge attributes - subset for
roads - subset for pedestrians - subset for custom

Run “pathfinder”

Visualize with SF/leaflet
