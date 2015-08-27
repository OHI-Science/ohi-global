### map_fxns.R 
### Aug 26, 2015: Casey O'Hara

library(rgdal)
library(maptools)
library(ggplot2)
require('RColorBrewer')
library(tidyr)
library(dplyr)

dir_global <- setwd('~/github/ohi-global')
dir_rept   <- file.path(dir_global, 'global2015/Reporting')
dir_data   <- file.path(dir_rept, 'data')

dir_ohiprep <- '~/github/ohiprep'
dir_sp     <-  file.path(dir_ohiprep, 'globalprep/spatial/downres')
# writeOGR dsn needs to be an absolute path? apparently '~' causes issues. getwd() expands the '~'.


get_rgn_df <- function(dsn = '~/github/ohiprep/globalprep/spatial/downres', layer = 'rgn_eez_gcs_low_res') {
  rgn_eez <- readOGR(dsn = path.expand(dsn), layer)
  
  ### The ID number for fortify(rgn_eez) is simply the row number within the @data.
  ### So create a lookup table for row number (zero to N-1) to rgn_id.
  rgn_lookup <- data.frame(id     = c(0:(nrow(rgn_eez@data)-1)), 
                           rgn_id = rgn_eez@data$rgn_id)
  
  ### Fortify the rgn_eez from shapefile into dataframe.  Then attach the region
  ### ID by the polygon ID (row number from @data)
  rgn_eez_df <- fortify(rgn_eez) %>%
    mutate(id = as.integer(id)) %>%
    left_join(rgn_lookup, by = 'id')
  
  return(rgn_eez_df)
}

expand_fld <- function(fld) {
  switch(fld,  
         'Index' = 'Ocean Health Index',
         'FP'    = 'Food Provision',
         'AO'    = 'Artisanal Fishing Opportunity',
         'NP'    = 'Natural Products',
         'CS'    = 'Carbon Storage',
         'CP'    = 'Coastal Protection',
         'TR'    = 'Tourism & Recreation',
         'LE'    = 'Coastal Livelihoods & Economies',
         'SP'    = 'Sense of Place',
         'CW'    = 'Clean Water',
         'BD'    = 'Biodiversity',
         'ECO'   = 'Economies',
         'LIV'   = 'Livelihoods',
         'FIS'   = 'Fisheries',
         'MAR'   = 'Mariculture',
         'ICO'   = 'Iconic Species',
         'LSP'   = 'Lasting Special Places',
         'HAB'   = 'Habitats', 
         'SPP'   = 'Species',
         fld)
}

plot_scores <- function(rgn_df, fld, fig_save = NULL, 
                        title = NULL, leg_title = FALSE, leg_on = TRUE) {
  if(is.null(title)) {
    fld_name <- expand_fld(fld)
    title <- sprintf('Scores: %s (%s)', fld, fld_name)
  }
  
  col.brks  <- seq(0, 100, length.out = 6)
  
  df_plot <- ggplot(data = rgn_df, aes(x = long, y = lat, group = group, fill = val)) +  
    theme(axis.ticks = element_blank(), axis.text = element_blank(),
          legend.position = ifelse(leg_on, 'right', 'none')) + 
    scale_fill_gradientn(colours = brewer.pal(10, 'RdYlBu'), space = 'Lab', na.value = 'gray80',
                         breaks = col.brks, labels = col.brks, limits = c(0, 100)) + 
    geom_polygon(color = 'gray80', size = 0.1) +
    borders('world', color='gray40', fill='gray45') + # create a layer of borders
    scale_x_continuous(breaks = seq(-180, 180, by = 30), expand = c(0, 2)) +
    scale_y_continuous(breaks = seq( -90,  90, by = 30), expand = c(0, 2)) +
    labs(title = title, 
         fill  = ifelse(leg_title & leg_on, fld, ''),
         x = NULL, y = NULL)
  df_plot
  
  if(!is.null(fig_save)) {
    cat(sprintf('Saving map to %s...\n', fig_save))
    ggsave(fig_save, width = 10, height = 6)
    return(invisible(df_plot))
  } else {
    return(df_plot)
  }  
}

plot_scores_easy <- function(scores_df, fld, rgn_df = NULL, fig_save = NULL, 
                             title = NULL, leg_title = FALSE, leg_on = TRUE) {
### combines plot_scores() with extracting the field values, getting the region shapefile,
### attaching the field values to the region shapefile.  If no region dataframe is
### passed, it will download and process it.
  fld_val  <- scores_df[ , c('rgn_id', fld)]
  names(fld_val)[2] = 'val'
  
  if(is.null(rgn_df)) {
    rgn_df <- get_rgn_df() %>%
      filter(rgn_id %in% scores_df$rgn_id)
  }
  
  ### join the fld_data info to the spatial info in rgn_df; will drop any
  ### regions not included in the rgn_id list for rgn_df.
  fld_data <- rgn_df %>%
    left_join(fld_val, 
              by = 'rgn_id')
  
  
  ### expand the field name to create a title
  fld_name <- expand_fld(fld)
  scenario <- ifelse('scenario' %in% tolower(names(scores_df)),
                     paste('_', scores_df$scenario[1]),
                     ifelse('year' %in% tolower(names(scores_df)),
                            paste('_', scores_df$year[1]),
                            ''))
  title <- sprintf('OHI %s Scores: %s (%s)', scenario, fld, fld_name)
  
  ### Call plot_scores function; with fig_save parameter will only save, not plot.
  ### But the function returns the plot invisibly, so can assign it to ohiplot and then plot it.
  ohiplot <- plot_scores(fld_data, fld, fig_save = fig_save, 
                         title = title, leg_title = leg_title, leg_on = leg_on)
  return(invisible(ohiplot))
}

