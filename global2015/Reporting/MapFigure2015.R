#######################################################################
### NCEAS/OHI
### R script to make maps of score data
### Melanie Frazier Oct 17, 2013
#######################################################################


library(rgdal)
# library(raster)
library(maptools)
#library(rgeos)
#library(gpclib)
# library(geosphere)
require('RColorBrewer')
# library(shapefiles)
# source('http://nceas.ucsb.edu/~frazier/functions/sort.txt')


dir_global <- setwd('~/github/ohi-global')
dir_rept   <- file.path(dir_global, 'global2015/Reporting')
dir_data   <- file.path(dir_rept, 'data')

dir_ohiprep <- '~/github/ohiprep'
dir_sp     <-  file.path(dir_ohiprep, 'globalprep/spatial/downres')
  # writeOGR dsn needs to be an absolute path? apparently '~' causes issues. getwd() expands the '~'.
source(file.path(dir_ohiprep, 'src/R/common.R'))
  # in ohiprep

### load spatial layer
rgn_eez <- readOGR(dsn = path.expand(dir_sp), 'rgn_eez_gcs_low_res')
eezlist <- rgn_eez@data$rgn_id

## 2013 data ----
ohi2015 <- read.csv(file.path(dir_data, 'scores_eez2015.csv'), stringsAsFactors = FALSE) %>%
  rename(rgn_name = country)
### ??? can we include region IDs on here?

rgn_names        <- read.csv('~/github/ohi-global/eez2013/layers/rgn_global.csv', stringsAsFactors = FALSE) %>%
  rename(rgn_name = label)

ohi2015 <- ohi2015 %>% 
  left_join(rgn_names, by = 'rgn_name') %>%
  filter(!is.na(rgn_id)) ### gets rid of global average
  
ohi2015 <- ohi2015 %>%
  filter(rgn_id %in% eezlist) %>%
  select(-scenario, -dimension) %>%
  arrange(rgn_id)

### The ID number for fortify(rgn_eez) is simply the row number within the @data.
### So create a lookup table for row number (zero to N-1) to rgn_id.
rgn_lookup <- data.frame(id     = c(0:(nrow(rgn_eez@data)-1)), 
                         rgn_id = rgn_eez@data$rgn_id)

### Fortify the rgn_eez from shapefile into dataframe.  Then attach the region
### ID by the polygon ID (row number from @data)
rgn_eez_df <- fortify(rgn_eez) %>%
  mutate(id = as.integer(id)) %>%
  left_join(rgn_lookup, by = 'id')


#############################################
## Making a nice map
#############################################

## Identify the columns of the PlotData to be mapped:
mapFlds   <- names(ohi2015 %>% select(-rgn_name, -rgn_id))
col.brks  <- seq(0, 100, length.out = 6)
myPalette <- colorRampPalette(c(brewer.pal(10, 'RdYlBu')))
year      <- 2015

## This loop goes through the columns to be plotted and:
## 1. matches the data row names (rgn_id) to the rgn_id of the OHI regions
## 2. identifies the break points for colors from: 0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100
## 3. plots the maps and saves them to the 'fig' file in the working directory 

for (fld in mapFlds){ # fld <- mapFlds[1]
  fig_save = file.path(dir_rept, sprintf('figures/global_map_%s_%s.png', fld, year))
  cat(sprintf('Creating map for %s...\n', fld))
  
  ### Separate out a simple data frame of rgn_id and field value; rename field to 'val'
  ### so it's easier to call with dplyr and ggplot functions
  fld_val  <- ohi2015[ , c('rgn_id', fld)]
  names(fld_val)[2] = 'val'
  
  fld_data <- rgn_eez_df %>%
    left_join(fld_val, 
              by = 'rgn_id')
  
  fld_name <- expand_fld(fld)
  
  rgns_plot(fld_data, fld)
}

expand_fld <- function(fld) {
  switch(fld,  'Index'='Ocean Health Index',
               'FP'   = 'Food Provision',
               'AO'   = 'Artisanal Fishing Opportunity',
               'NP'   = 'Natural Products',
               'CS'   = 'Carbon Storage',
               'CP'   = 'Coastal Protection',
               'TR'   = 'Tourism & Recreation',
               'LE'   = 'Coastal Livelihoods & Economies',
               'SP'   = 'Sense of Place',
               'CW'   = 'Clean Water',
               'BD'   = 'Biodiversity',
               'ECO'  = 'Economies',
               'LIV'  = 'Livelihoods',
               'FIS'  = 'Fisheries',
               'MAR'  = 'Mariculture',
               'ICO'  = 'Iconic Species',
               'LSP'  = 'Lasting Special Places',
               'HAB'  = 'Habitats', 
               'SPP'  = 'Species')
}

rgns_plot <- function(rgn_df, fld, title = sprintf('OHI Scores: %s', fld), fig_save = NULL) {
  df_plot <- ggplot(data = rgn_df, aes(x = long, y = lat, group = group, fill = val)) +  
    scale_fill_gradientn(colours = brewer.pal(10, 'RdYlBu'), space = 'Lab', na.value = 'gray80',
                         breaks = col.brks, labels = col.brks, limits = c(0, 100)) + 
    scale_x_continuous(breaks = seq(-180, 180, by = 30), expand = c(0, 2)) +
    scale_y_continuous(breaks = seq( -90,  90, by = 30), expand = c(0, 2)) +
    geom_polygon(color = 'gray80', size = 0.1) +
    borders('world', color='gray40', fill='gray45') + # create a layer of borders
    labs(title = title, fill = fld, x = NULL, y = NULL)
  df_plot
  
  if(!is.null(fig_save)) {
    cat(sprintf('Saving map to %s...\n', fig_save))
    ggsave(fig_save, width = 10, height = 6)
    return(invisible(df_plot))
  } else {
    return(df_plot)
  }  
}


# Difference data ----
OHIdiff <- read.csv('TableDiffs2013_2012.csv')
OHIdiff <- subset(OHIdiff, !(Country.EEZ %in% c('Global (area-weighted average)',
                                                            'Global (EEZ average)')))
row.names(OHIdiff) <- OHIdiff$code

OHIdiff <- subset(OHIdiff, select=c(code, Index, FP, AO, NP,
                                                CS, CP, TR, LE, 
                                                SP, CW, BD, 
                                                ECO, LIV, FIS, MAR,
                                                ICO, LSP, HAB, SPP))
OHIdiff <- plyr::rename(OHIdiff, c(code='rgn_id',
                             Index='Ocean Health Index',
                                   FP = 'Food Provision',
                                   AO = 'Artisanal Fishing Opportunity',
                                   NP= 'Natural Products',
                                   CS='Carbon Storage',
                                   CP='Coastal Protection',
                                   TR= 'Tourism & Recreation',
                                   LE='Coastal Livelihoods & Economies',
                                   SP='Sense of Place',
                                   CW='Clean Water',
                                   BD='Biodiversity',
                                   ECO='Economies',
                                   LIV='Livelihoods',
                                   FIS='Fisheries',
                                   MAR='Mariculture',
                                   ICO='Iconic Species',
                                   LSP='Lasting Special Places',
                                   HAB='Habitats', 
                                   SPP='Species'))

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
  plot(rgnOHI, border='grey75', add=TRUE,
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
