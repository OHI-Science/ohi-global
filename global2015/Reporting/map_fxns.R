### map_fxns.R 
### Aug 26, 2015: Casey O'Hara
library(rgdal)
library(maptools)
require('RColorBrewer')
library(ggplot2)

<<<<<<< HEAD
get_rgn_df <- function(dsn = '~/github/ohiprep/globalprep/spatial/downres', layer = 'rgn_eez_gcs_low_res') {
  rgn_shp <- readOGR(path.expand(dsn), layer, p4s = "+proj=longlat +datum=WGS84")
=======
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
>>>>>>> cc24c57e8392a542c7c0cdbcf28c3ea9946ab61b
  
  ### The ID number for fortify(rgn_shp) is simply the row number within the @data.
  ### So create a lookup table for row number (zero to N-1) to rgn_id.
  rgn_lookup <- data.frame(id     = c(0:(nrow(rgn_shp@data)-1)), 
                           rgn_id = rgn_shp@data$rgn_id)
  
#   if (proj == 'moll') {
#     cat('transforming spatial polygons data frame to Mollweide projection\n')
#     rgn_shp <- spTransform(rgn_shp, CRS("+proj=moll +R=10567000 +lon_0=0 +x_0=0 +y_0=0 +units=m +towgs84=0,0,0,0,0,0,0 +no_defs"))
#   }
  
  
  ### Fortify the rgn_eez from shapefile into dataframe.  Then attach the region
  ### ID by the polygon ID (row number from @data)
  rgn_df <- fortify(rgn_shp) %>%
    mutate(id = as.integer(id)) %>%
    left_join(rgn_lookup, by = 'id')
  
  return(rgn_df)
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

<<<<<<< HEAD

plot_scores <- function(fld_data, fld, fig_save = NULL, title = NULL, 
                        leg_title = FALSE, leg_on = TRUE) {
=======
plot_scores <- function(rgn_df, fld, fig_save = NULL, 
                        title = NULL, leg_title = FALSE, leg_on = TRUE) {
>>>>>>> cc24c57e8392a542c7c0cdbcf28c3ea9946ab61b
  if(is.null(title)) {
    fld_name <- expand_fld(fld)
    title <- sprintf('Scores: %s (%s)', fld, fld_name)
  }
  
  col.brks  <- seq(0, 100, length.out = 6)
<<<<<<< HEAD

  df_plot <- ggplot(data = fld_data, aes(x = long, y = lat, group = group, fill = val)) +  
    theme(axis.ticks = element_blank(), axis.text = element_blank(), 
          plot.title = element_text(hjust = 0),
          legend.position = ifelse(leg_on, 'right', 'none'),
          text = element_text(colour = 'gray20', size = 10, family = 'Helvetica'),
          title = element_text(size = rel(1.5), face = 'bold')) +
    scale_y_continuous(breaks = (-2:2) * 30, expand = c(0, 2)) +
    scale_x_continuous(breaks = (-6:6) * 30, expand = c(0, 2)) +
    scale_fill_gradientn(colours = brewer.pal(10, 'RdYlBu'), space = 'Lab', na.value = 'gray80',
                         breaks = col.brks, labels = col.brks, limits = c(0, 100)) + 
    geom_polygon(color = 'gray80', size = 0.1) +
    borders('world', color='gray40', size = 0.25, fill='gray45') +
    labs(title = title, 
         x = NULL, y = NULL,
         fill = ifelse(leg_title & leg_on, fld, ''))
=======
  
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
>>>>>>> cc24c57e8392a542c7c0cdbcf28c3ea9946ab61b
  df_plot
  
  if(!is.null(fig_save)) {
    cat(sprintf('Saving map to %s...\n', fig_save))
    ggsave(fig_save, width = 10, height = 6)
    return(invisible(df_plot))
  } else {
    return(df_plot)
  }  
}

<<<<<<< HEAD

plot_scores_easy <- function(scores_df, fld, rgn_df = NULL, fig_save = NULL,
                             title = NULL, leg_title = FALSE, leg_on = TRUE) {
  ### Separate out a simple data frame of rgn_id and field value; rename field to 'val'
  ### so it's easier to call with dplyr and ggplot functions
  
  if(is.null(rgn_df)) {
    rgn_df <- get_rgn_df()
  }
  fld_val  <- scores_df[ , c('rgn_id', fld)]
  names(fld_val)[2] = 'val'
  
  ### join the fld_data info to the spatial info in rgn_df.  inner_join() will eliminate
  ### any polygons for unreported regions (NA regions will still be included)
  fld_data <- rgn_df %>%
    inner_join(fld_val, 
               by = 'rgn_id')
  
  ### expand the field name to create a title
  fld_name <- expand_fld(fld)
  scenario <- ifelse('year' %in% names(scores_df),     paste(' ', scores_df$year[1], sep = ''),
              ifelse('scenario' %in% names(scores_df), paste(' ', scores_df$scenario[1], sep = ''),
              ''))
  title <- sprintf('OHI%s Scores: %s (%s)', scenario, fld, fld_name)
  
  ### Call plot_scores function; with fig_save parameter will only save, not plot.
  ### But the function returns the plot invisibly, so can assign it to ohiplot and then plot it.
  ohiplot <- plot_scores(fld_data, fld, fig_save = fig_save, title = title, leg_title = leg_title, leg_on = leg_on)
  
}
=======
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

>>>>>>> cc24c57e8392a542c7c0cdbcf28c3ea9946ab61b
