###########################################
## This script is used to create a geojson map
## with regions that have OHI assessments

# For more information: http://zevross.com/blog/2014/04/11/using-r-to-quickly-create-an-interactive-online-map-using-the-leafletr-package/



### Need to read in the shape files for each of these regions:

library(sp)
library(rgdal)
library(raster)
library(leafletR) # to visualize

map1 <- readOGR('../ohiprep/globalprep/spatial/downres', layer="rgn_eez_gcs_low_res")
map1 <- map1[map1@data$rgn_nam %in% c('Brazil', 'Canada', 'British Virgin Islands', 'Ecuador', 'Colombia',
                            'China', 'Mexico', 'Peru', 'Venezuela', 'South Korea', 'Japan', 'New Caledonia',
                            'Fiji'), ]
proj4string(map1) <- CRS("+init=epsg:4326")

map1@data$country <- 1:nrow(map1@data)

writeOGR(map1, 'global2015/geojson/test/map1.geojson', layer = '', driver='GeoJSON')  # layer is needed, but doesn't make a difference what is included

dat <- toGeoJSON(map1)
sty <- styleCat(prop="country", val=1:nrow(map1@data), style.val = heat.colors(nrow(map1@data)))
map <- leafletR::leaflet(dat, incl.data=TRUE, popup = c('rgn_nam'), style=sty,
                         controls = c("zoom", "scale", "layer"))
browseURL(map) # this doesn't seem to work on Neptune...need to download html file
# don't think this is needed, but Ben creates this...so keeping code here in case I need it
fw = file('global2015/geojson/test/map1.js', 'wb')
cat('var regions = ', file=fw)
cat(readLines('global2015/geojson/test/map1.geojson', n = -1), file=fw)
close(fw)


data(quakes)
dat <- toGeoJSON(data=quakes)

# create and view simple map
map <- leaflet(dat)
map  # redirects to browseURL(map)
map <- leaflet(data=dat, title="Fiji Earthquakes")

# set map size, center and zoom level
map <- leaflet(data=dat, 
               size=c(800,600), center=c(-18.35, 179.75), zoom=6)

# set base map and popup
# magnitude is used as popup (type names(quakes) for available properties)
map <- leaflet(data=dat, 
               base.map="mqsat", popup="mag")

# minimalist? - no base map
map <- leaflet(data=dat, 
               base.map=NA, popup="mag")

# include data in HTML file
map <- leaflet(dat, incl.data=TRUE)
