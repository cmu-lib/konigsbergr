library(konigsbergr)

raw_boston <- get_osm_bbox(xmin = -71.1383,
                           xmax = -71.1132,
                           ymin = 42.3592,
                           ymax = 42.3754)

levels(raw_boston$nodes$attrs$user) <- iconv(levels(raw_boston$nodes$attrs$user), "UTF-8")
levels(raw_boston$ways$attrs$user) <- iconv(levels(raw_boston$ways$attrs$user), "UTF-8")
levels(raw_boston$relations$attrs$user) <- iconv(levels(raw_boston$relations$attrs$user), "UTF-8")

levels(raw_boston$nodes$tags$v) <- iconv(levels(raw_boston$nodes$tags$v), "UTF-8")
levels(raw_boston$ways$tags$v) <- iconv(levels(raw_boston$ways$tags$v), "UTF-8")
levels(raw_boston$relations$tags$v) <- iconv(levels(raw_boston$relations$tags$v), "UTF-8")

boston <- raw_boston
use_data(boston, overwrite = TRUE)
