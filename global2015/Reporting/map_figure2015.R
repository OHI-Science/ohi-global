#######################################################################
### NCEAS/OHI
### R script to make maps of score data
### Aug 25, 2015: Casey O'Hara
###   * Modified to incorporate ggplot library, and added functions for more generic plotting
### Oct 17, 2013: Melanie Frazier 
###   * Original script
#######################################################################
<<<<<<< HEAD


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
=======
dir_rept <- '~/github/ohi-global/global2015/Reporting'
>>>>>>> cc24c57e8392a542c7c0cdbcf28c3ea9946ab61b
source(file.path(dir_rept, 'map_fxns.R'))

source(file.path('~/github/ohiprep/src/R/common.R'))
# in ohiprep
library(ggplot2)

### Get regions map - converts shapefile to data frame
<<<<<<< HEAD
rgn_df  <- get_rgn_df()


### get OHI data and clean it up.
scores_df <- read.csv(file.path(dir_data, 'scores_eez2015.csv'), stringsAsFactors = FALSE) %>%
  rename(rgn_name = country)

if (!('rgn_id' %in% names(scores_df))) {
  cat('Adding region ID numbers to scores file.\n')
  rgn_names        <- read.csv('~/github/ohi-global/eez2013/layers/rgn_global.csv', stringsAsFactors = FALSE) %>%
    rename(rgn_name = label)
  
  scores_df <- scores_df %>% 
=======
rgn_df <- get_rgn_df() %>%
  filter(rgn_id %in% scores_data$rgn_id)


### get OHI data and clean it up.
scores_data <- read.csv(file.path(dir_data, 'scores_eez2015.csv'), stringsAsFactors = FALSE) %>%
  rename(rgn_name = country)

if (!('rgn_id' %in% names(scores_data))) {
  rgn_names        <- read.csv('~/github/ohi-global/eez2013/layers/rgn_global.csv', stringsAsFactors = FALSE) %>%
    rename(rgn_name = label)
  
  scores_data <- scores_data %>% 
>>>>>>> cc24c57e8392a542c7c0cdbcf28c3ea9946ab61b
    left_join(rgn_names, by = 'rgn_name') %>%
    filter(!is.na(rgn_id)) ### gets rid of global average, if left_join didn't
}

<<<<<<< HEAD
### set year to use in file name
year <- scores_df$scenario[1]  

### establish list of fields for mapping
mapFlds   <- names(scores_df %>% select(-rgn_name, -rgn_id, -scenario, -dimension))

### Loop over each field, plotting each map in turn and saving to file.
for (fld in mapFlds) { # fld <- mapFlds[1]
=======
scores_data <- scores_data %>%
  arrange(rgn_id)


### Making a set of score maps -----

### Identify the columns of the OHI data to be mapped.  The for loop with
### call each field in turn to attach data and plot it.
mapFlds   <- names(scores_data %>% select(-rgn_name, -rgn_id, -scenario, -dimension))

for (fld in mapFlds) { 
  
  # fld <- mapFlds[1]
  fig_save = file.path(dir_rept, sprintf('figures/global_map_%s_%s.png', fld, scores_data$scenario[1]))
  cat(sprintf('Creating map for %s...\n', fld))
  
  ### Separate out a simple data frame of rgn_id and field value; rename field to 'val'
  ### so it's easier to call with dplyr and ggplot functions
  fld_val  <- scores_data[ , c('rgn_id', fld)]
  names(fld_val)[2] = 'val'
  
  ### join the fld_data info to the spatial info in rgn_eez_df
  fld_data <- rgn_df %>%
    left_join(fld_val, 
              by = 'rgn_id')

  ### expand the field name to create a title
  fld_name <- expand_fld(fld)
  title <- sprintf('OHI 2015 Scores: %s (%s)', fld, fld_name)
>>>>>>> cc24c57e8392a542c7c0cdbcf28c3ea9946ab61b
  
   fig_save = file.path(dir_rept, sprintf('figures/global_map_%s_%s.png', fld, year))
   cat(sprintf('Creating map for %s...\n', fld))

   ohiplot <- plot_scores_easy(scores_df, fld, rgn_df, fig_save = fig_save, title = title)
  
}


<<<<<<< HEAD
=======
single_plot <- plot_scores_easy(scores_data, mapFlds[1], 
                                rgn_df = rgn_df, 
                                fig_save = '~/github/ohi-global/global2015/Reporting/figures/test_index.png', 
                                leg_on = TRUE)
### to test out the "plot_scores_easy()" function


>>>>>>> cc24c57e8392a542c7c0cdbcf28c3ea9946ab61b
# Difference data ----
OHIdiff <- read.csv('TableDiffs2013_2012.csv')
OHIdiff <- subset(OHIdiff, !(Country.EEZ %in% c('Global (area-weighted average)',
                                                            'Global (EEZ average)')))
row.names(OHIdiff) <- OHIdiff$code

