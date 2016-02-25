############################################
## Visualizing Antarctica results
############################################

library(rgdal)
library(sp)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(tidyr)
library(rgeos)
library(maptools)

data <- read.csv('antarctica2015/scores.csv') %>%
  filter(region_id != 0) %>%
  filter(dimension == "score") %>%
  spread(goal, score) %>%
  select(-dimension, sp_id=region_id) %>%
  mutate(sp_id = as.character(sp_id))

antarctica <- readOGR(dsn='/var/data/ohi/git-annex/Global/NCEAS-Regions_v2014/data', layer= 'antarctica_stereographic')
plot(antarctica)
text(getSpPPolygonsLabptSlots(antarctica), labels=antarctica@data$sp_id, cex=0.8)


land <- antarctica[antarctica@data$sp_type %in% c("land", "land-ccamlr"), ]
eez <- antarctica[antarctica@data$sp_type %in% c("eez"), ]
ccamlr <- antarctica[antarctica@data$sp_type %in% c("eez-ccamlr"), ] 

land_f <- fortify(land, region = 'sp_id')
eez_f <- fortify(eez, region = "sp_id")
ccamlr_f <- fortify(ccamlr, region = "sp_id") %>%
  select(long, lat, order, hole, piece, sp_id=id, group) %>%
  left_join(data)

  
col.brks  <- seq(0, 100, length.out = 6)

goals <- c(
'Index' = 'Ocean Health Index',
'FP'    = 'Food Provision',
'NP'    = 'Natural Products',
'TR'    = 'Tourism & Recreation',
'LE'    = 'Coastal Livelihoods & Economies',
'SP'    = 'Sense of Place',
'CW'    = 'Clean Water',
'BD'    = 'Biodiversity',
'ECO'   = 'Economies',
'FIS'   = 'Fisheries',
'ICO'   = 'Iconic Species',
'LSP'   = 'Lasting Special Places',
'HAB'   = 'Habitats', 
'SPP'   = 'Species')


for(goal in names(goals)){
  
#goal = names(goals)[1]

fig <-  ggplot(ccamlr_f, aes(long, lat, group = group)) +
  geom_polygon(aes_string(fill = goal), colour="gray90") +
   scale_fill_gradientn(colours = brewer.pal(10, 'RdYlBu'), space = 'Lab', na.value = 'gray80',
                        breaks = col.brks, labels = col.brks, limits = c(0, 100)) + 
  geom_polygon(data = eez_f, fill = 'white', size = 0.25)  +
   geom_polygon(data = land_f, color = 'gray90', fill = 'gray90', size = 0.25)  +
   theme(axis.line=element_blank(),
         axis.text.x=element_blank(),
         axis.text.y=element_blank(),
         axis.ticks=element_blank(),
         axis.title.x=element_blank(),
         axis.title.y=element_blank(),
         panel.background=element_blank(),
         panel.border=element_blank(),
         panel.grid.major=element_blank(),
         panel.grid.minor=element_blank(),
         plot.background=element_blank()) +
   labs(fill = goals[which(names(goals)==goal)]) 
ggsave(sprintf('global2015/AntarcticaReporting/maps/%s.png', goals[which(names(goals)==goal)]), width=8, height=6)
}


### some code for combining polygons
antarctica <- readOGR(dsn='/var/data/ohi/git-annex/Global/NCEAS-Regions_v2014/data', layer= 'antarctica_stereographic_v5')
plot(antarctica)
antarctica <- antarctica[!(antarctica$sp_id == 288100 & antarctica$sp_type=="eez-ccamlr"), ]
region <- readOGR(dsn='/var/data/ohi/git-annex/Global/NCEAS-Regions_v2014/data', layer= 'antarctica_288100')
region@data <- data.frame(sp_type = "eez-ccamlr", sp_id=288100) 
row.names(region@data) <- 0

antarctica_noLine <- rbind(antarctica, region, makeUniqueIDs = TRUE)

writeOGR(antarctica_noLine, dsn='/var/data/ohi/git-annex/Global/NCEAS-Regions_v2014/data', layer= 'antarctica_stereographic', driver="ESRI Shapefile")
