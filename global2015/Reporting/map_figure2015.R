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
library(ggplot2)

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
rgn_df  <- get_rgn_df()


### get OHI data and clean it up.
scores_df <- read.csv(file.path(dir_data, 'scores_eez2015.csv'), stringsAsFactors = FALSE) %>%
  rename(rgn_name = country)

if (!('rgn_id' %in% names(scores_df))) {
  cat('Adding region ID numbers to scores file.\n')
  rgn_names        <- read.csv('~/github/ohi-global/eez2013/layers/rgn_global.csv', stringsAsFactors = FALSE) %>%
    rename(rgn_name = label)
  
  scores_df <- scores_df %>% 
    left_join(rgn_names, by = 'rgn_name') %>%
    filter(!is.na(rgn_id)) ### gets rid of global average
}

### set year to use in file name
year <- scores_df$scenario[1]  

### establish list of fields for mapping
mapFlds   <- names(scores_df %>% select(-rgn_name, -rgn_id, -scenario, -dimension))

### Loop over each field, plotting each map in turn and saving to file.
for (fld in mapFlds) { # fld <- mapFlds[1]
  
   fig_save = file.path(dir_rept, sprintf('figures/global_map_%s_%s.png', fld, year))
   cat(sprintf('Creating map for %s...\n', fld))

   ohiplot <- plot_scores_easy(scores_df, fld, rgn_df, fig_save = fig_save, title = title)
  
}


# Difference data ----
OHIdiff <- read.csv('TableDiffs2013_2012.csv')
OHIdiff <- subset(OHIdiff, !(Country.EEZ %in% c('Global (area-weighted average)',
                                                            'Global (EEZ average)')))
row.names(OHIdiff) <- OHIdiff$code

