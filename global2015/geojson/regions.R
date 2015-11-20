###########################################
## This script is used to create a geojson map
## with regions that have OHI assessments

# For more information: http://zevross.com/blog/2014/04/11/using-r-to-quickly-create-an-interactive-online-map-using-the-leafletr-package/
# http://stackoverflow.com/questions/26435861/how-to-read-a-geojson-file-containing-feature-collections-to-leaflet-shiny-direc
# http://www.tuicode.com/article/5637dded499808840885af68

### Need to read in the shape files for each of these regions:

library(sp)
library(rgdal)
library(raster)
# library(leafletR) # to visualize
library(leaflet)
library(htmlwidgets)
library(jsonlite)

map1 <- readOGR('../ohiprep/globalprep/spatial/downres', layer="rgn_eez_gcs_low_res")
# map1 <- map1[map1@data$rgn_nam %in% c('Brazil', 'Canada'), ]


map1 <- map1[map1@data$rgn_nam %in% c('Brazil', 'Canada', 'British Virgin Islands', 'Ecuador', 'Colombia',
                            'China', 'Mexico', 'Peru', 'Venezuela', 'South Korea', 'Japan', 'New Caledonia',
                            'Fiji'), ]
proj4string(map1) <- CRS("+init=epsg:4326")

map1@data$country <- 1:nrow(map1@data)

## save as geojson:
writeOGR(map1, 'global2015/geojson/test/map1.geojson', layer = '', driver='GeoJSON')  # layer is needed, but doesn't make a difference what is included
# dat <- toGeoJSON(map1) # another way of saving

## REading a geojson file:
# # From http://data.okfn.org/data/datasets/geo-boundaries-world-110m
geojson <- readLines('global2015/geojson/test/map1.geojson', warn = FALSE) %>%
  paste(collapse = "\n") %>%
  fromJSON(simplifyVector = FALSE)

geojson$style = list(
  weight = 1,
  color = "#F7FCF5",
  opacity = 1,
  fillOpacity = 0.8
)


pal <- colorBin("Greens", 1:nrow(map1))

geojson$features <- lapply(geojson$features, function(feat){
  feat$properties$style <- list(
    fillColor = pal(as.numeric(feat$properties$country))
  )
  feat
})

leaflet() %>% addGeoJSON(geojson)
exportJSON <- toJSON(geojson)
write(exportJSON, "global2015/geojson/test/map2.geojson")
####################
# uses leaflet and htmlwidgets to save html file
m <- leaflet(data=map1) %>%
  addTiles() %>%
  addPolygons(fillColor = topo.colors(nrow(map1), alpha=NULL), popup=map1@data$rgn_nam, stroke=FALSE)
saveWidget(m, file="map1.html")


## make html using leafletR
sty <- styleCat(prop="country", val=1:nrow(map1@data), style.val = heat.colors(nrow(map1@data)))
map <- leafletR::leaflet(dat, incl.data=TRUE, popup = c('rgn_nam'), style=sty,
                         controls = c("zoom", "scale", "layer"))
browseURL(map) # this doesn't seem to work on Neptune...need to download html file

# don't think this is needed, but Ben creates this...so keeping code here in case I need it
fw = file('global2015/geojson/test/map1.js', 'wb')
cat('var regions = ', file=fw)
cat(readLines('global2015/geojson/test/map1.geojson', n = -1), file=fw)
close(fw)


