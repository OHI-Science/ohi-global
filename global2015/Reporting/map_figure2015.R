#######################################################################
### NCEAS/OHI
### R script to make maps of score data
### Aug 25, 2015: Casey O'Hara
###   * Modified to incorporate ggplot library, and added functions for more generic plotting
### Oct 17, 2013: Melanie Frazier 
###   * Original script
#######################################################################

source('~/github/ohiprep/src/R/common.R')  # in ohiprep
dir_rept <- '~/github/ohi-global/global2015/Reporting'
source(file.path(dir_rept, 'map_fxns.R'))

### set scenario and desired map projection
scenario <- 2015
prj      <- 'mol'    ### note: 'gcs' is way faster.

### get OHI data and rename column headings
scores_df <- read.csv(file.path(dir_rept, sprintf('data/scores_eez%s.csv', scenario)), stringsAsFactors = FALSE) %>%
  rename(rgn_name = country, rgn_id = region_id)

### load region data frame, so doesn't need to reload every time through the loop.  Also
### load the land data frame, if plotting in Mollweide projection.
rgn_df <- get_rgn_df(prj = prj)
if(prj == 'mol' & !exists('land_poly')) {
  land_poly  <- get_land_df()
  ocean_poly <- get_ocean_df() ### assume if land_poly doesn't exist, ocean_poly doesn't either...
}


### establish list of fields for mapping
mapFlds   <- names(scores_df %>% select(-rgn_name, -rgn_id, -scenario, -dimension))

### Loop over each field, plotting each map in turn and saving to file.
for (fld in mapFlds) { # fld <- mapFlds[1]
  
  fig_save = file.path(dir_rept, sprintf('figures/maps_by_goal_%s/global_map_%s_%s_%s.png', prj, fld, scenario, prj))
  cat(sprintf('Creating map for %s...\n', fld))
  
  ohiplot <- plot_scores_easy(scores_df, fld, rgn_df, title = title, prj = prj, fig_save = fig_save)
  # ohiplot
}


single_plot <- plot_scores_easy(scores_df, fld = 'LE', 
                                #rgn_df = rgn_df, 
                                #fig_save = '~/github/ohi-global/global2015/Reporting/figures/test_index.png', 
                                leg_on = TRUE,
                                prj = 'mol')
single_plot
### to test out the "plot_scores_easy()" function


# Difference data ----
OHIdiff <- read.csv('TableDiffs2013_2012.csv')
OHIdiff <- subset(OHIdiff, !(Country.EEZ %in% c('Global (area-weighted average)',
                                                            'Global (EEZ average)')))
row.names(OHIdiff) <- OHIdiff$code

