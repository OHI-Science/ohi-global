#######################################################################
### NCEAS/OHI
### R script to make maps of score data
### Melanie Frazier Oct 17, 2013
#######################################################################

library(rgdal)
library(raster)
library(sp)
library(maptools)
library(rgeos)
library(geosphere)
library(plyr)
require('RColorBrewer')


source("http://nceas.ucsb.edu/~frazier/functions/sort.txt")
setwd("N:/git-annex/Global/FigurePrep_2013OHI")


## Load the map data:
# p <- readOGR("N:/model/GL-NCEAS-OceanRegions_v2013a/data", "rgn_ocean_cntry_gcs")
# p <- readOGR('C:/Users/Melanie/Desktop/NCEAS/data/data', "rgn_ocean_cntry_gcs") #for when my internet connection is particulalry crappy


p <- readOGR("N:/model/GL-NCEAS-OceanRegions_v2013a/data", "rgn_ocean_cntry_mol")



## This isolates the main map parts: 
ocean  <-  p[is.na(p@data$ISO_3digit) & p@data$rgn_id==0,]
land  <-  p[!is.na(p@data$ISO_3digit) & p@data$rgn_id==0,]
rgnOHI <-  p[p@data$rgn_id>0,]

## 2013 data ----
OHIscores2013 <- read.csv("Data2013_round0.csv")  
OHIscores2013 <- subset(OHIscores2013, !(Country.EEZ %in% c("Global (area-weighted average)",
                                                            "Global (EEZ average)")))
row.names(OHIscores2013) <- OHIscores2013$code

PlotData <- subset(OHIscores2013, select=c(-Country.EEZ))

# ## 2012 data ----
# OHIscores2012 <- read.csv("Data2012_round0.csv")  
# OHIscores2012 <- subset(OHIscores2012, !(Country.EEZ %in% c("Global (area-weighted average)",
#                                                             "Global (EEZ average)")))
# row.names(OHIscores2012) <- OHIscores2012$code
# 
# PlotData <- subset(OHIscores2012, select=c(-Country.EEZ))

#############################################
## Making a nice map
#############################################

## Identify the columns of the PlotData to be mapped:
#mapCols <- 2
mapCols <- 2:20

## This loop goes through the columns to be plotted and:
## 1. matches the data row names (rgn_id) to the rgn_id of the OHI regions
## 2. identifies the break points for colors from: 0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100
## 3. plots the maps and saves them to the "fig" file in the working directory 

for (i in 1:length(mapCols)){ 
  #i <- 14
  s=mapCols[i]
  # Index map
  fld = names(PlotData)[s]
  v= PlotData[match(rgnOHI@data$rgn_id, row.names(PlotData)), fld]    
  maps = setNames(list(v), fld)  
  
  fig = sprintf('figs/scores2013/global_map_%s_2013.png', fld)
#  fig = sprintf('figs/global_map_%s_2012.png', fld)
  
  col.brks = seq(0,100,length.out=11)
  
  # ensure color ramp breaks contain all data
  stopifnot(max(v,na.rm=T)<=max(col.brks) & min(v,na.rm=T)>=min(col.brks) & length(col.brks)==11)
  
  # plot map
  png(file=fig, width=1200, height=800, res=150, pointsize=18, type='cairo')
  par(oma=c(0,0,0,0),
      mar=c(1.5,1,0,1))
  plot(ocean, col='gray90', border=NA)
  plot(rgnOHI, border="grey75", add=TRUE,
       col=brewer.pal(10, 'RdYlBu')[cut(v, col.brks, labels=1:10, include.lowest=TRUE)])
  plot(land, col='gray80', border='grey75', add=TRUE)
  
  # get plotting dimensions
  p=par('usr'); px=diff(p[1:2]); py=diff(p[3:4]) # c(x1, x2, y1, y2)
  
  # add label
  text(x=p[1]+px*0.02, y=p[4]-py*0.1, labels=fld, pos=4) # +p.dx*0.1
  
  # plot legend
  yh=0.05; x1=p[1]+px*0.2; x2=p[2]-px*0.2; y1=p[3]; y2=p[3]+py*yh
  ix = seq(x1, x2, length.out=10)
  ixd = diff(ix)[1]/2
  par(xpd=TRUE) # turn off clipping to plot region
  image(x = ix,
        y = c(y1, y2),
        z = matrix(1:10), col=brewer.pal(10, 'RdYlBu'), add=T)
  rect(x1-ixd,y1,x2+ixd,y2, border='gray20')
  rect(x1-ixd*3,y1,x1-ixd,y2, border='gray20')
  text(x = c(x1-ixd*2, seq(x1-ixd, x2+ixd, length.out=11)),
       y = y1, cex=0.6, pos=1, # adj=c(0.5,0), # , offset=0.1,
       labels=c('NA',as.character(col.brks))) 
  par(xpd=F) # turn back on clipping to plot region
  
  # finish fig
  dev.off() #; system(sprintf('open %s', fig))
}

# Difference data ----
OHIdiff <- read.csv("TableDiffs2013_2012.csv")
OHIdiff <- subset(OHIdiff, !(Country.EEZ %in% c("Global (area-weighted average)",
                                                            "Global (EEZ average)")))
row.names(OHIdiff) <- OHIdiff$code

OHIdiff <- subset(OHIdiff, select=c(code, Index, FP, AO, NP,
                                                CS, CP, TR, LE, 
                                                SP, CW, BD, 
                                                ECO, LIV, FIS, MAR,
                                                ICO, LSP, HAB, SPP))
OHIdiff <- plyr::rename(OHIdiff, c(code="rgn_id",
                             Index="Ocean Health Index",
                                   FP = "Food Provision",
                                   AO= "Artisanal Fishing Opportunity",
                                   NP= "Natural Products",
                                   CS="Carbon Storage",
                                   CP="Coastal Protection",
                                   TR= "Tourism & Recreation",
                                   LE="Coastal Livelihoods & Economies",
                                   SP="Sense of Place",
                                   CW="Clean Water",
                                   BD="Biodiversity",
                                   ECO="Economies",
                                   LIV="Livelihoods",
                                   FIS="Fisheries",
                                   MAR="Mariculture",
                                   ICO="Iconic Species",
                                   LSP="Lasting Special Places",
                                   HAB="Habitats", 
                                   SPP="Species"))

PlotData <- OHIdiff



## Identify the columns of the PlotData to be mapped:
mapCols <- 2:20
for (i in 1:length(mapCols)){ 
#  i <- 1
  s=mapCols[i]
  # Index map
  fld = names(PlotData)[s]
  v = PlotData[match(rgnOHI@data$rgn_id, row.names(PlotData)), fld]    
  maps = setNames(list(v), fld)  
  
  fig = sprintf('figs/difs/global_map_diffs_%s.png', fld)
  col.brks <- c(-100, -50, -10, -2, -1, 0, 1, 2, 10, 50, 100)
  # ensure color ramp breaks contain all data
  stopifnot(max(v,na.rm=T)<=max(col.brks) & min(v,na.rm=T)>=min(col.brks) & length(col.brks)==11)
  
  # plot map
  png(file=fig, width=1200, height=800, res=150, pointsize=18, type='cairo')
  par(oma=c(0,0,0,0),
      mar=c(1.5,1,0,1))
  plot(ocean, col='gray90', border=NA)
  plot(rgnOHI, border="grey75", add=TRUE,
       col=brewer.pal(10, 'RdYlBu')[cut(v, col.brks, labels=1:10, include.lowest=TRUE)])
  plot(land, col='gray80', border='grey75', add=TRUE)
  
  # get plotting dimensions
  p=par('usr'); px=diff(p[1:2]); py=diff(p[3:4]) # c(x1, x2, y1, y2)
  
  # add label
  text(x=0, y=p[4]-py*0.1, labels=fld, cex=1.3) # +p.dx*0.1
  #text(x=p[1]+px*0.02, y=p[4]-py*0.1, labels=fld, pos=4) # +p.dx*0.1
  
  # plot legend
  yh=0.05; x1=p[1]+px*0.2; x2=p[2]-px*0.2; y1=p[3]; y2=p[3]+py*yh
  ix = seq(x1, x2, length.out=10)
  ixd = diff(ix)[1]/2
  par(xpd=TRUE) # turn off clipping to plot region
  image(x = ix,
        y = c(y1, y2),
        z = matrix(1:10), col=brewer.pal(10, 'RdYlBu'), add=T)
  rect(x1-ixd,y1,x2+ixd,y2, border='gray20')
  rect(x1-ixd*3,y1,x1-ixd,y2, border='gray20')
  text(x = c(x1-ixd*2, seq(x1-ixd, x2+ixd, length.out=11)),
       y = y1, cex=0.6, pos=1, # adj=c(0.5,0), # , offset=0.1,
       labels=c('NA',as.character(col.brks))) 
  par(xpd=F) # turn back on clipping to plot region
  
  # finish fig
  dev.off() #; system(sprintf('open %s', fig))
}
