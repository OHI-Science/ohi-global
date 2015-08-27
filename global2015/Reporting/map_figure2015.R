#######################################################################
### NCEAS/OHI
### R script to make maps of score data
### Aug 25, 2015: Casey O'Hara
###   * Modified to incorporate ggplot library, and added functions for more generic plotting
### Oct 17, 2013: Melanie Frazier 
###   * Original script
#######################################################################


library(rgdal)
library(maptools)
require('RColorBrewer')

dir_global <- setwd('~/github/ohi-global')
dir_rept   <- file.path(dir_global, 'global2015/Reporting')
dir_data   <- file.path(dir_rept, 'data')

dir_ohiprep <- '~/github/ohiprep'
dir_sp     <-  file.path(dir_ohiprep, 'globalprep/spatial/downres')
  # writeOGR dsn needs to be an absolute path? apparently '~' causes issues. getwd() expands the '~'.
source(file.path(dir_ohiprep, 'src/R/common.R'))
  # in ohiprep
source(file.path(dir_rept, 'map_fxns.R'))


### Get regions map - converts shapefile to data frame
rgn_eez_df <- get_rgn_df


### get OHI data and clean it up.
ohi_data <- read.csv(file.path(dir_data, 'scores_eez2015.csv'), stringsAsFactors = FALSE) %>%
  rename(rgn_name = country)

if (!('rgn_id' %in% names(ohi_data))) {
  rgn_names        <- read.csv('~/github/ohi-global/eez2013/layers/rgn_global.csv', stringsAsFactors = FALSE) %>%
    rename(rgn_name = label)
  
  ohi_data <- ohi_data %>% 
    left_join(rgn_names, by = 'rgn_name') %>%
    filter(!is.na(rgn_id)) ### gets rid of global average
}

year <- ohi_data$scenario[1]  

ohi_data <- ohi_data %>%
  filter(rgn_id %in% rgn_eez_df$rgn_id) %>%
  select(-scenario, -dimension) %>%
  arrange(rgn_id)


### Making a set of score maps -----

### Identify the columns of the OHI data to be mapped.  The for loop with
### call each field in turn to attach data and plot it.
mapFlds   <- names(ohi_data %>% select(-rgn_name, -rgn_id))

for (fld in mapFlds) { 
  
  # fld <- mapFlds[1]
  fig_save = file.path(dir_rept, sprintf('figures/global_map_%s_%s.png', fld, year))
  cat(sprintf('Creating map for %s...\n', fld))
  
  ### Separate out a simple data frame of rgn_id and field value; rename field to 'val'
  ### so it's easier to call with dplyr and ggplot functions
  fld_val  <- ohi2015[ , c('rgn_id', fld)]
  names(fld_val)[2] = 'val'
  
  ### join the fld_data info to the spatial info in rgn_eez_df
  fld_data <- rgn_eez_df %>%
    left_join(fld_val, 
              by = 'rgn_id')

  ### expand the field name to create a title
  fld_name <- expand_fld(fld)
  title <- sprintf('OHI 2015 Scores: %s (%s)', fld, fld_name)
  
  ### Call plot_scores function; with fig_save parameter will only save, not plot.
  ### But the function returns the plot invisibly, so can assign it to ohiplot and then plot it.
  ohiplot <- plot_scores(fld_data, fld, fig_save = fig_save, title = title)
  
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



