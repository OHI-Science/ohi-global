############################################
## Creating a general Antarctica map
############################################

library(rgdal)
library(sp)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(tidyr)
library(rgeos)
library(maptools)

data <- read.csv('../ohiprep/Antarctica/Other/rgn_labels_ccamlr.csv') %>%
  select(ccamlr_id=ccamlr_id2, sp_id) %>%
  mutate(sp_id = as.character(sp_id))

colors <- data.frame(sp_id = data$sp_id, col= c('#feedde', '#fdd0a2', '#fdae6b', '#fd8d3c', '#e6550d', '#a63603',
  '#fcfbfd', '#efedf5', '#dadaeb', '#bcbddc', '#9e9ac8', '#807dba', '#6a51a3', '#54278f', '#3f007d', '#807dba',
  '#6baed6', '#2171b5', '#084594')) %>%
  mutate(sp_id = as.character(sp_id)) %>%
  mutate(col = as.character(col))
  

data <- left_join(data, colors, by="sp_id")

antarctica <- readOGR(dsn='/home/shares/ohi/git-annex/Global/NCEAS-Regions_v2014/data', layer= 'antarctica_stereographic')
plot(antarctica)
text(getSpPPolygonsLabptSlots(antarctica), labels=antarctica@data$sp_id, cex=0.8)



land <- antarctica[antarctica@data$sp_type %in% c("land", "land-ccamlr"), ]
eez <- antarctica[antarctica@data$sp_type %in% c("eez"), ]
ccamlr <- antarctica[antarctica@data$sp_type %in% c("eez-ccamlr"), ] 

idList <- ccamlr@data$sp_id
# "coordinates" extracts centroids of the polygons, in the order listed at worldMap@data
centroids.df <- as.data.frame(coordinates(ccamlr))
names(centroids.df) <- c("Longitude_c", "Latitude_c")  #more sensible column names

pop.df <- data.frame(id = idList, centroids.df)



land_f <- fortify(land, region = 'sp_id')
eez_f <- fortify(eez, region = "sp_id")
ccamlr_f <- fortify(ccamlr, region = "sp_id") %>%
  select(long, lat, order, hole, piece, sp_id=id, group) %>%
  left_join(data)


  fig <- ggplot(ccamlr_f, aes(long, lat)) +
    geom_polygon(aes(fill = sp_id, group=group), colour="gray90") +
    scale_fill_manual(values = c('248100' = '#fd8d3c',
                                 '248200' = '#fdd0a2',
                                 '248300' = '#fdae6b', 
                                 '248400' = '#fd8d3c',
                                 '248500' = '#e6550d',
                                 '248600' = '#a63603', 
                                 '258410' = '#3f007d',
                                 '258420' = '#9e9ac8',
                                 '258431' = '#dadaeb',
                                 '258432' = '#bcbddc',
                                 '258441' = '#9e9ac8',
                                 '258442' = '#807dba',
                                 '258510' = '#6a51a3',
                                 '258520' = '#54278f',
                                 '258600' = '#3f007d',
                                 '258700' = '#807dba',
                                 '288100' = '#6baed6',
                                 '288200' = '#2171b5',
                                 '288300' = '#084594')) + 
    geom_polygon(data = eez_f, aes(group=group), fill = 'white', size = 0.25)  +
    geom_polygon(data = land_f, aes(group=group), color = 'gray90', fill = 'gray90', size = 0.25)  +
    geom_text(data=pop.df, aes(label=id, x= Longitude_c, y = Latitude_c), color = 'gray90') +
    theme(axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_blank(),
          legend.position = "none") 
  fig
  # +
#    labs(fill = goals[which(names(goals)==goal)]) 
  ggsave('global2015/AntarcticaReporting/maps/generic_map.png', width=7, height=6)
