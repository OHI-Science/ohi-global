### map_fxns.R 
### Aug 26, 2015: Casey O'Hara


get_rgn_df <- function(dsn = '~github/ohiprep/globalprep/spatial/downres', layer = 'rgn_eez_gcs_low_res') {
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
         'SPP'  = 'Species',
         fld)
}

plot_scores <- function(rgn_df, fld, fig_save = NULL, title = NULL) {
  if(is.null(title)) {
    fld_name <- expand_fld(fld)
    title <- sprintf('OHI Scores: %s', fld_name)
  }
  
  col.brks  <- seq(0, 100, length.out = 6)
  
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

