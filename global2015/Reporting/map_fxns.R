### map_fxns.R 
### Aug 26, 2015: Casey O'Hara
require('rgdal')
require('maptools')
require('RColorBrewer')
require('ggplot2')
require('stringr')

if(!exists('dir_global')) 
  dir_global <- ifelse(dir.exists('~/github'), '~/github/ohi-global', '~/ohi-global')

get_rgn_df <- function(dsn = str_replace(dir_global, 'ohi-global', 'ohiprep/globalprep/spatial/downres'),
                       layer = NULL, prj = 'gcs') {
  if(is.null(layer)) layer <- sprintf('rgn_eez_%s_low_res', prj)
  rgn_shp <- readOGR(dsn = path.expand(dsn), layer, verbose = FALSE) #, p4s = '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')

  ### The ID number for fortify(rgn_shp) is simply the row number within the @data.
  ### So create a lookup table for row number (zero to N-1) to rgn_id.
  rgn_lookup <- data.frame(id     = c(0:(nrow(rgn_shp@data) - 1)), 
                           rgn_id = rgn_shp@data$rgn_id)
  
#   if (prj == 'mol') {
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


get_land_df <- function(dsn = sprintf('%s/../ohiprep/globalprep/spatial/downres', dir_global),
                        layer = 'rgn_land_mol_low_res') {
### gets Mollweide land forms for plotting.
  rgn_shp <- readOGR(dsn = path.expand(dsn), layer, verbose = FALSE)
  
  ### Fortify the rgn_eez from shapefile into dataframe.  Then attach the region
  ### ID by the polygon ID (row number from @data)
  rgn_df <- fortify(rgn_shp)
  
  return(rgn_df)
}

get_ocean_df <- function(dsn = sprintf('%s/../ohiprep/globalprep/spatial/downres', dir_global),
                         layer = 'rgn_all_mol_low_res') {
  ### gets Mollweide ocean regions (all) for plotting.
  rgn_shp <- readOGR(dsn = path.expand(dsn), layer, verbose = FALSE)
  
  ### Fortify the rgn_eez from shapefile into dataframe.  Then attach the region
  ### ID by the polygon ID (row number from @data)
  rgn_df <- fortify(rgn_shp)
  
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


plot_scores <- function(rgn_df, fld, fig_save = NULL, prj = 'gcs',
                        title = NULL, leg_title = FALSE, leg_on = TRUE) {
  if(is.null(title)) {
    fld_name <- expand_fld(fld)
    title <- sprintf('Scores: %s (%s)', fld, fld_name)
  }
  
  col.brks  <- seq(0, 100, length.out = 6)

  df_plot <- ggplot(data = rgn_df, aes(x = long, y = lat, group = group, fill = val)) +  
    theme(axis.ticks = element_blank(), axis.text = element_blank(),
          text = element_text(family = 'Helvetica', color = 'gray30', size = 12),
          plot.title = element_text(size = rel(1.5), hjust = 0, face = 'bold'),
          legend.position = ifelse(leg_on, 'right', 'none')) + 
    scale_fill_gradientn(colours = brewer.pal(10, 'RdYlBu'), space = 'Lab', na.value = 'gray80',
                         breaks = col.brks, labels = col.brks, limits = c(0, 100)) + 
    labs(title = title, 
         fill  = ifelse(leg_title & leg_on, fld, ''),
         x = NULL, y = NULL) 
  if(prj == 'mol'){
  ### For Mollweide, 'border()' doesn't seem to work.  Load in land and ocean polygons to plot directly.
    if(!exists('land_poly'))
      land_poly <- get_land_df()
    if(!exists('ocean_poly'))
      ocean_poly <- get_ocean_df()
    
    df_plot <- df_plot +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
      geom_polygon(data = ocean_poly, color = 'gray92', fill = 'gray92', size = 0.25) +
      geom_polygon(color = 'gray80', size = 0.1) +
      geom_polygon(data = land_poly, color = 'gray40', fill = 'gray45', size = 0.25)
    ### df_plot order: oceans (light grey), then EEZ score polygons, then land polygons (dark grey).
  } else {
  ### For default projection, use standard grey background for oceans, and use borders() for land forms.
    df_plot <- df_plot +   
      geom_polygon(color = 'gray80', size = 0.1) +
      borders('world', color = 'gray40', fill = 'gray45', size = .25) + # create a layer of borders
      scale_x_continuous(breaks = seq(-180, 180, by = 30), expand = c(0, 2)) +
      scale_y_continuous(breaks = seq( -90,  90, by = 30), expand = c(0, 2))
  }

  if(!is.null(fig_save)) {
    cat(sprintf('Saving map to %s...\n', fig_save))
    ggsave(fig_save, width = 10, height = 6)
    return(invisible(df_plot))
  } else {
    return(df_plot)
  }  
}


plot_scores_easy <- function(scores_df, fld, rgn_df = NULL, fig_save = NULL, prj = 'gcs',
                             title = NULL, leg_title = FALSE, leg_on = TRUE) {
  ### Separate out a simple data frame of rgn_id and field value; rename field to 'val'
  ### so it's easier to call with dplyr and ggplot functions
  
  if(is.null(rgn_df)) {
    rgn_df <- get_rgn_df(prj = prj)
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
  ohiplot <- plot_scores(fld_data, fld, fig_save = fig_save, title = title, 
                         leg_title = leg_title, leg_on = leg_on, prj = prj)
  return(ohiplot)
}
