###########################################
## This script is used to create a geojson map
## with regions that have OHI assessments

### Need to read in the shape files for each of these regions:

library(sp)
library(rgdal)
library(raster)

map1 <- readOGR('../ohiprep/globalprep/spatial/downres', layer="rgn_eez_gcs_low_res")
map1 <- map1[map1@data$rgn_nam %in% c('Brazil', 'Canada', 'British Virgin Islands', 'Ecuador', 'Colombia',
                            'China', 'Mexico', 'Peru', 'Venezuela', 'South Korea', 'Japan', 'New Caledonia',
                            'Fiji'), ]
proj4string(map1) <- CRS("+init=epsg:4326")

writeOGR(map1, 'global2015/geojson/test/map1.geojson', layer = 'tmp', driver='GeoJSON')  # layer is needed, but doesn't make a difference what is included
writeOGR(map1, 'global2015/geojson/test/map1.js', layer = 'tmp', driver='GeoJSON')
