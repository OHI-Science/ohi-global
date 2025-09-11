### map_fxns.R 
### Sept 2022: Cullen Molitor


### load libraries
if (!require(librarian)){install.packages('librarian')}
librarian::shelf(
  tidyverse,
  here,
  terra,
  tidyterra
)

land <- terra::vect(land_file, crs = mollweide)

ocean <- terra::vect(ocean_file, crs = mollweide)

ohi_regions <- terra::vect(region_file, crs = mollweide) 

plot_theme <- function(legend) {
  theme_classic() +
    theme(axis.ticks = element_blank(), 
          axis.text = element_blank(),
          text = element_text(family = 'Helvetica', color = 'gray30', size = 12),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          plot.title = element_text(size = rel(1.5), hjust = 0, face = 'bold'),
          plot.background = element_rect(fill = 'white', color = NA),
          panel.background = element_rect(fill = 'white', color = NA),
          legend.position = legend)
}

plot_scores_map <- function(metric = "scores") {
  
  if (metric == "scores") {
    
    for (year in 2012:as.numeric(scenario)) {
      
      cat("Year: ", year, "\n")
      
      year_year <- paste0('year_', year)
      dir_year <- here::here(dir_goal_maps, year_year)
      if (!dir.exists(dir_year) & year != scenario) dir.create(dir_year)
      
      scores_file <- here::here(dir_results_data, paste0('scores_eez_', year, '.csv')) %>% 
        readr::read_csv() %>% 
        dplyr::rename(rgn_id = region_id) 
      
      # merge scores into regions
      scores_shape <- terra::merge(ohi_regions, scores_file)
      
      # convert to sf for plotting
      scores_sf <- sf::st_as_sf(scores_shape)
      ocean_sf  <- sf::st_as_sf(ocean)
      land_sf   <- sf::st_as_sf(land)
      
      for (i in seq_along(goals)) {
        
        cat(paste0("    Goal: ", goal_names$long_goal[i], ' (', goals[i], ")\n"))
        
        p1 <- ggplot() +
          geom_sf(data = ocean_sf, color = NA, fill = 'grey97') +
          geom_sf(data = scores_sf, color = 'gray80', size = .1, 
                  aes(fill = !!sym(goals[i]))) +
          geom_sf(data = land_sf, fill = "grey80", color = "gray85", size = .25) +
          scale_fill_gradientn(colours = colors, space = 'Lab', na.value = 'gray80',
                               breaks = col.brks.maps, labels = col.brks.maps, limits = c(0, 100)) +
          labs(title = goal_names$long_goal[i], fill = NULL) +
          plot_theme(legend = "right")
        
        fn <- paste0('global_map_', goals[i], '_', year, '_mol.png')
        if (year == scenario) {
          filename <- here::here(dir_goal_maps, fn)
        } else {
          filename <- here::here(dir_year, fn)
        }
        
        ggsave(filename = filename, plot = p1, width = 10, height = 6)
      }
    }
    
    # ---- No-legend maps for scenario year ----
    cat("Year: ", scenario, " (no legend)\n")
    
    year_year <- paste0('year_', scenario, '_nolegend')
    dir_year <- here::here(dir_goal_maps, year_year)
    if (!dir.exists(dir_year)) dir.create(dir_year)
    
    scores_file <- here::here(dir_results_data, paste0('scores_eez_', scenario, '.csv')) %>% 
      readr::read_csv() %>% 
      dplyr::rename(rgn_id = region_id)
    
    scores_shape <- terra::merge(ohi_regions, scores_file)
    scores_sf <- sf::st_as_sf(scores_shape)
    ocean_sf  <- sf::st_as_sf(ocean)
    land_sf   <- sf::st_as_sf(land)
    
    for (i in seq_along(goals)) {
      cat(paste0("    Goal: ", goal_names$long_goal[i], ' (', goals[i], ")\n"))
      
      p1 <- ggplot() +
        geom_sf(data = ocean_sf, color = NA, fill = 'grey97') +
        geom_sf(data = scores_sf, color = 'gray80', size = .1, 
                aes(fill = !!sym(goals[i]))) +
        geom_sf(data = land_sf, fill = "grey80", color = "gray85", size = .25) +
        scale_fill_gradientn(colours = colors, space = 'Lab', na.value = 'gray80',
                             breaks = col.brks.maps, labels = col.brks.maps, limits = c(0, 100)) +
        labs(title = goal_names$long_goal[i], fill = NULL) +
        plot_theme(legend = 'none')
      
      fn <- paste0('global_map_', goals[i], '_', scenario, '_mol.png')
      filename <- here::here(dir_year, fn)
      ggsave(filename = filename, plot = p1, width = 10, height = 6)
    }
    
  } else {  #### Trend maps
    
    trends_df <- here::here(dir_results_data, paste0('trends_', scenario, '.csv')) %>% 
      readr::read_csv() %>%
      dplyr::rename(rgn_name = country, rgn_id = region_id) 
    
    trend_shape <- terra::merge(ohi_regions, trends_df)
    trend_sf <- sf::st_as_sf(trend_shape)
    ocean_sf <- sf::st_as_sf(ocean)
    land_sf  <- sf::st_as_sf(land)
    
    dir_trend <- here::here(dir_results_figures, "map_trends")
    if (!dir.exists(dir_trend)) dir.create(dir_trend)
    
    for (i in seq_along(goals)) {
      cat(paste0("Goal: ", goal_names$long_goal[i], ' (', goals[i], ")\n"))
      
      trends_sf <- trend_sf
      trends_sf$val2 <- cut(dplyr::pull(trends_sf, !!goals[i]), col.brks.trends, include.lowest = TRUE)
      tmp <- rev(names(table(trends_sf$val2)))
      tmp_labels <- gsub("-100", "min", tmp)
      tmp_labels <- gsub("100", " max", tmp_labels)
      
      p1 <- ggplot() +
        geom_sf(data = ocean_sf, color = NA, fill = 'grey97') +
        geom_sf(data = trends_sf, color = 'gray80', size = .1, 
                aes(fill = val2)) +
        geom_sf(data = land_sf, fill = "grey80", color = "gray85", size = .25) +
        scale_fill_manual(values = rev(colors_trend), na.value = 'gray80',
                          breaks = tmp, labels = tmp_labels, limits = tmp) +
        labs(title = goal_names$long_goal[i], fill = NULL) +
        plot_theme(legend = ifelse(goals[i] == "Index", "right", "none"))
      
      fn <- paste0('trends_map_', goals[i], '_mol.png')
      filename <- here::here(dir_trend, fn)
      ggsave(filename = filename, plot = p1, width = 10, height = 6)
    }
  }
}






