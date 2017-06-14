
rgn_sf   <- sf::read_sf(dsn = 'spatial',
                        layer = 'rgn_mol_low')
ocean_sf <- sf::read_sf(dsn = 'spatial',
                        layer = 'all_mol_low') %>%
  select(region_id = rgn_id, geometry)
land_sf  <- sf::read_sf(dsn = 'spatial',
                        layer = 'land_mol_low')

map_df <- rad_df %>%
  filter(dimension %in% c('score'))

map_trend_df <- map_df %>%
  filter(!is.na(value)) %>%
  group_by(region_id, goal) %>%
  do(value = lm(value ~ scenario, data = .)[['coefficients']][['scenario']]) %>%
  mutate(value = round(value, 5),
         dimension = 'annual_change',
         scenario  = 2016)

map_df <- map_df %>%
  bind_rows(map_trend_df) 

# map_df <- map_df %>%
#   filter(dimension == 'annual_change')
  
for(scen in map_df$scenario %>% unique()) {
  ### scen <- rad_df$scenario[1]
  scen_df <- map_df %>%
    filter(scenario == scen)

  for(dimen in scen_df$dimension %>% unique()) {
    ### dimen <- scen_df$dimension[1]
    scen_folder <- paste(dimen, scen, sep = '_')
    
    dim_df <- scen_df %>%
      filter(dimension == dimen)

    for(gl in dim_df$goal %>% unique()) {
      ### gl <- dim_df$goal[1]
      tmp_df <- dim_df %>%
        filter(goal == gl)
      
      if(!dir.exists(file.path('maps', scen_folder))) dir.create(file.path('maps', scen_folder))
      map_file <- file.path('maps', scen_folder, paste0(tolower(gl), '_', dimen, '_', scen, '.png'))
      
      goal_title <- as.character(goal_names[goal_names$goal_code == gl, 2])
      goal_title <- ifelse(gl == 'Index' & dimen == 'score',
                           str_replace(goal_title, 'Index', 'Overall Index'),
                           goal_title)
      
      # if(!file.exists(map_file)) {
      map_gl <- plot_scores(tmp_df, fld_name = 'value',
                            title = sprintf('%s: %s %s',
                                            goal_title,
                                            dimen %>% str_replace_all('_', ' '),
                                            scen),
                            leg_title = tools::toTitleCase(dimen %>% str_replace_all('_', '\n')),
                            type = dimen)
  
        message('Saving map for ', map_file)
  
        ggsave(plot = map_gl, filename = map_file, width = 6.5, height = 3.5, dpi = 100)
      # } else {
      #   message('Map file exists: ', map_file)
      # }
    }
  }
}

plot_scores <- function(data_df,
                        fld_name,
                        title,
                        leg_title,
                        type = 'score') {

  colors_spec = brewer.pal(10, 'RdYlBu')
  
  if(type %in% c('score')) {
    col_brks  <- seq(0, 100, length.out = 6)
  }
  if(type == 'annual_change') {
    x <- range(data_df[[fld_name]], na.rm = TRUE) %>% abs() %>% max()
    trend_breaks <- c(1, 2, 5, 10, 25, 100)
    y <- min(trend_breaks[trend_breaks > x])
    col_brks  <- seq(-y, y, length.out = 5)
    message('range value = ', 
            paste(range(data_df[[fld_name]], na.rm = TRUE), collapse = ' to '), 
            '; col_brks set to ', paste(col_brks, collapse = ', '))
  }
  
  rgn_data_sf <- rgn_sf %>%
    left_join(data_df, by = 'region_id')

  sf_plot <- ggplot(data = rgn_data_sf) +
    ggtheme_blank(textsize = 8) +
    theme(axis.text = element_blank()) +
    scale_fill_gradientn(colours = colors_spec, space = 'Lab', na.value = 'gray70',
                         breaks = col_brks,
                         labels = col_brks, limits = range(col_brks)) +
    labs(title = title,
         fill  = leg_title,
         x = NULL, y = NULL)

  sf_plot <- sf_plot +
    geom_sf(data = ocean_sf, color = 'gray97', fill = 'gray97', size = 1) +
    geom_sf(color = 'gray80', aes_string(fill = fld_name), size = 0.1) +
    geom_sf(data = land_sf, color = 'gray85', fill = 'gray80', size = 0.25)
  ### sf_plot order: oceans (light grey), then EEZ score polygons, then land polygons (dark grey).

  return(sf_plot)

}
