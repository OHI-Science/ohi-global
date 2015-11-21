###########################################
## This script is used to create a geojson map
## with regions that have OHI assessments

# For more information: http://zevross.com/blog/2014/04/11/using-r-to-quickly-create-an-interactive-online-map-using-the-leafletr-package/
# http://stackoverflow.com/questions/26435861/how-to-read-a-geojson-file-containing-feature-collections-to-leaflet-shiny-direc
# http://www.tuicode.com/article/5637dded499808840885af68

### Need to read in the shape files for each of these regions:
library(dplyr)
library(sp)
library(rgdal)
library(raster)
# library(leafletR) # alternative method for visualization
library(leaflet)
library(htmlwidgets)
library(jsonlite)
library(RColorBrewer)


## Hawaii----
map2 <- readOGR(dsn='/var/data/ohi/git-annex/Global/NCEAS-Regions_v2014/data', layer= 'sp_gcs')
map2 <- map2[map2@data$sp_type == "eez", ]
Hawaii <- map2[map2@data$sp_name %in% c('Hawaii'), ]

Hawaii@data <- Hawaii@data %>%
  dplyr::select(Region = sp_name) %>%
  mutate(Region = "United States, Hawaii")

## US Pacific Coast----
## Identifying the US West Coast: don't need to run this again
# US <- map2[map2@data$sp_name %in% c('United States'), ]
# locator(2)
# US <- crop(US, extent(-135, -112, 50, 30))
# writeOGR(US, dsn='/var/data/ohi/git-annex/Global/NCEAS-Regions_v2014/data', layer= 'USWestCoast_gcs', driver="ESRI Shapefile")

USWC <- readOGR(dsn='/var/data/ohi/git-annex/Global/NCEAS-Regions_v2014/data', layer= 'USWestCoast_gcs')
USWC@data <- USWC@data %>%
  dplyr::select(Region = sp_name) %>%
  mutate(Region = "United States, Pacific Coast")


## Several countries----

map1 <- readOGR('../ohiprep/globalprep/spatial/downres', layer="rgn_eez_gcs_low_res")

map1 <- map1[map1@data$rgn_nam %in% c('Brazil', 'Canada', 'British Virgin Islands', 'Ecuador', 'Colombia',
                                      'China', 'Mexico', 'Peru', 'South Korea', 'Japan',
                                      'Israel', 'Panama', 'Spain', 'Chile', 'Fiji'), ]
proj4string(map1) <- CRS(proj4string(map2))

#map1@data$country <- 1:nrow(map1@data) # don't think I need this anymore.
map1@data <- map1@data %>%
  dplyr::select(Region=rgn_nam)


#### Baltic (BHI)
library(rgeos)
bhi <- readOGR(dsn='/var/data/ohi/git-annex/clip-n-ship/bhi/spatial', layer = 'rgn_offshore_gcs')
bhi <- gBuffer(bhi, width=0.05)
data <- data.frame(id = getSpPPolygonsIDSlots(bhi))
row.names(data) <- getSpPPolygonsIDSlots(bhi)
bhi <- SpatialPolygonsDataFrame(bhi, data=data)

bhi@data <- bhi@data %>%
  dplyr::select(Region = id) %>%
  mutate(Region = "Baltic")

#### British Columbia: don't include for now...wait for Casey to tell me the correct map
# bci <- readOGR(dsn='/var/data/ohi/git-annex/bcprep/regions/rgn_development/mapp_rgns', layer = 'mapp_rgn_all')
# bci2 <- readOGR(dsn='/var/data/ohi/git-annex/bcprep/regions/rgn_development/working', layer = 'ohi-bc-rgns')
# bci3 <- readOGR(dsn='/var/data/ohi/git-annex/bcprep/regions/rgn_development', layer = 'ohibc_rgns_unclipped')
# 
# bci <- spTransform(bci, proj4string(data2))


### Combine layers
regionAll <- rbind(map1, USWC, Hawaii, bhi)
writeOGR(regionAll, dsn='/var/data/ohi/git-annex/Global/NCEAS-Regions_v2014/data/website_OHIplus_regions', 
         layer="allRegions", driver = "ESRI Shapefile")
write.csv(regionAll@data, 'global2015/geojson/regions.csv', row.names=FALSE)

####################
# uses leaflet and htmlwidgets to save html file

popup1 <- paste0('<b>', "Region", '</b>', "<br/>", regionAll@data$Region)
#myPalette <- colorRampPalette(brewer.pal(11, "Spectral"))
myPalette <- colorRampPalette(c("#9E0142", "#D53E4F", "#F46D43", "#FDAE61", "#3288BD", "#5E4FA2"))
#myPalette <- topo.colors(nrow(map1), alpha=NULL)

m <- leaflet() %>%
  addTiles() %>%
#   addTiles(options = tileOptions(noWrap = TRUE)) %>%  
#   fitBounds(-180, -70, 180, 80) %>%
  #  addTiles() %>%
  addPolygons(data = regionAll, 
              fillColor = myPalette(nrow(regionAll)), 
              #fillColor = myPalette,
              popup=popup1, 
              #stroke=FALSE,
              color = myPalette,
              weight = 1,
              opacity = 0.5,
              fillOpacity = 0.3)
saveWidget(m, file="allRegions.html", selfcontained=FALSE)


############## NOT sure if anything down here is necessary anymore
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




