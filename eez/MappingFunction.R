#' PlotMap
#' Plots static maps of OHI scores.
#'
#' @param scores dataframe with at least 2 columns: rgn_id and values.Must have the same length as the number of regions to map
#' @param rgn_poly dataframe of spatial boundaries; prepared by PrepSpatial()
#' @param map_title optional title for map
#' @param include_land whether or not to map land behind OHI regions. SEE TODOs BELOW
#' @param fld_rgn usually rgn_id or region_id; default 'region_id'
#' @param fld_score column name of value to plot; default 'score'
#' @param scale_label default to 'score' TODO: necessary?
#' @param scale_limits default to c(0, 100)
#' @param print_fig logical to print to display; default TRUE
#' @param fig_path file path to save png; default NULL
#'
#' @return (invisible) ggplot object
#' @export
#'
#' @examples
#'
#'

##### temporary (until added to ohicore)
library(ggplot2) # install.packages('ggplot2')
library(RColorBrewer) # install.packages('RColorBrewer')
library(dplyr)
library(tidyr)
library(sf)
library(rnaturalearth)

PlotMap <- function(goal_plot       = NA,
                    dimension_plot  = "score",
                    year_plot       = NA,
                #    scale_limits    = c(0, 100),
                    print_fig       = TRUE, ### print to display
                    fig_path        = NULL) { ### path to save the plot as an image
  # allow fig_png to be NULL and then pass it back as a list of ggplot objects so that you could modify it more on {
  
  #fig_path = "maps"
  
  ### get spatial file
  rgn_poly <- read_sf("spatial/regions_gcs.geojson")
  
  if(sum(names(rgn_poly) == "rgn_id") == 1){
    rgn_poly <- rgn_poly %>%
      rename(region_id = "rgn_id")
  } 

  
  ### score data
  scores <- read.csv("scores.csv") 
  
  ## if there is no year variable in the data, the current year is assigned
  if(sum(names(scores) == "year") == 0){
    scores$year <- substring(date(), 21, 24)
  } 
  
  ## if there are multiple years in the dataset and no year_plot argument, 
  ## the most recent year of data is selected
  if(is.na(year_plot)){
    scores <- scores %>%
      filter(year == max(year))
  } 

 ## filters the goal of interest, otherwise all goals are printed
  if(!is.na(goal_plot)){
    scores <- scores %>%
      filter(goal %in% goal_plot)
  }
    
  goal_plots <- unique(scores$goal)
  
  scores <- scores %>%
    spread(goal, score) %>%
    filter(dimension == dimension_plot)

  ### join polygon with scores
  score_rgn <- rgn_poly %>%
    left_join(scores, by = 'region_id')
  
  # ctrys50m <- ne_countries(scale = 50, type = "countries", returnclass = "sf") %>%
  #   select(iso_a3, iso_n3, admin)
  
  for(goal in goal_plots){ # goal = "FIS"
  
  df_plot <-  ggplot() +
#    geom_sf(data = ctrys50m, fill="grey") +
    geom_sf(data = score_rgn, aes_string(fill = goal), color = "gray80", size = 0.1) +
    coord_sf(datum = NA) + 
    scale_fill_gradientn(colours = brewer.pal(10, 'RdYlBu'),
                         na.value = 'gray80',
                         limits = c(0, 100),
                         name = scale_label) + 
    theme(axis.ticks = element_blank(),
          axis.text  = element_blank(),
          axis.title = element_blank(),
          text       = element_text(family = 'Helvetica', color = 'gray30', size = 12),
          plot.title = element_text(size = rel(1.5), hjust = 0, face = 'bold'),
          legend.position = 'right') +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank()) +
    labs(title = goal_plot) 
    
  
  
  if(print_fig) {
    print(df_plot)
  }
  
 save_fp <- paste0(fig_path, "/", goal_plot, "_", unique(score_rgn$year), ".png")
  if(!is.null(fig_path)) {
    ggsave(filename = save_fp, plot = df_plot, width = 7, height = 7)
  }
  }

}