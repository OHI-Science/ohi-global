### server_fxns.R
library(raster)
library(stringr)
library(tmap)
library(RColorBrewer)

# create a blank ggplot theme
ggtheme_basic <- function(textsize = 10) {
  theme_bw() +
  theme_update(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          plot.background = element_blank(),
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          text = element_text(size = textsize),
          plot.title = element_text(size = textsize * 1.5))
}


#################################.
##### Species Map Functions #####
#################################.
# These functions take a single species scientific name as input, then grab all 
# occurrence cells and associated Aquamaps probability and/or IUCN proportional area
# per cell

get_spp_map_df <- function(species) { ### species <- spp_list$name[1]
  message('in get_spp_map_df(), looking for species: ', species)

  spp_id <- spp_list %>%
    filter(name == species) %>%
    dplyr::select(am_sid, iucn_sid, name) %>%
    distinct()
  
  iucn_spp_map <- iucn_spp_cells %>%
    filter(iucn_sid %in% spp_id$iucn_sid) %>%
    group_by(loiczid) %>%        
    summarize(iucn_sid = first(iucn_sid))
      ### The group_by() and summarize() are to eliminate duped cells (e.g. 
      ### one am_sid matching two iucn_sids)
    
  am_spp_map   <- am_spp_cells %>%
    filter(am_sid == spp_id$am_sid)
  
  spp_map_df <- full_join(iucn_spp_map, am_spp_map, by = 'loiczid') %>%
    mutate(am   = ifelse(is.na(am_sid), NA, 1),
           iucn = ifelse(is.na(iucn_sid), NA, 2)) %>%
    dplyr::select(loiczid, am, iucn) %>%
    mutate(both = ifelse(!is.na(iucn), ### IUCN presence, so...
                              ifelse(is.na(am), 2, 3), ### if no AM, assign 2; if AM, assign 3. 
                              am))          ### no IUCN, so assign 1 for AM and NA for none
  
  return(spp_map_df)
}

get_rast <- function(spp_map_df, type, legend_classes = c(1, 2, 3)) {
  ### legend classes is to force the legend to include all, even if
  ### some classes are missing. 
  ### e.g. c(1, 2, 3) for AM/IUCN/Both, c(1, 2) for shallow/deep corals
  message('in get_rast()')
  message('... rasterizing to type = ', type)

  force_legend <- data.frame(loiczid = legend_classes)
  force_legend[type] <- legend_classes
      ### annoying but adding back in one of each value, to force the legend

  spp_map_type <- spp_map_df %>%
    bind_rows(force_legend)
  rast_obj <- raster::subs(loiczid_raster, spp_map_type, 
                     by    = 'loiczid', 
                     which = type, 
                     subsWithNA = TRUE) %>%
    setNames('presence')
  
  return(rast_obj)
}

assemble_map_tmap <- function(map_rast, spp) {
  message('in assemble_map_tmap()')
  map_obj <- tm_shape(fao_rgn) +
    tm_polygons(border.col = 'grey40',
                col = '#f6f8ff',
                lwd = .25) +
    tm_shape(land, is.master = TRUE) +
      tm_polygons(border.col = 'grey25', 
                  col = 'grey80', 
                  lwd = 0.25) + 
    tm_shape(map_rast) +
      tm_raster(# palette = c('#FFAEB9', '#41B6C4', '#0C2C84'),
                palette = c('#1b9e77', '#d95f02', '#7520b3'),
                style   = 'cat',
                breaks  = c(1, 2, 3),
                labels  = c('Aquamaps',   'IUCN',    'Both'),
                auto.palette.mapping = FALSE,
                colorNA = NULL,
                showNA  = TRUE,
                title = spp,
                alpha = 1)  +
    tm_layout(legend.text.size = 1,
              legend.title.size = 1.2,
              legend.outside = FALSE,
              legend.position = c('left', 'bottom'),
              legend.bg.color = '#f6f8ff',
              legend.bg.alpha = .9,
              outer.margins = 0, inner.margins = 0, asp = 2.1)
        
  return(map_obj)
}

#####################################################.
##### Functions for quadplots and barcharts tab #####
#####################################################.

create_barchart <- function(expt_rev) {
  message('in create_barchart()')
  
  if(expt_rev != 'all') {
    spp_gp_quadrants <- quad_gp_list %>%
      filter(expert)
  } else {
    spp_gp_quadrants <- quad_gp_list %>%
      filter(!expert)
  }
  
  quad_names <- data.frame('quad' = c('q4', 'q3', 'q2', 'q1'),
                           'quad_name' = factor(c('poorly aligned', 
                                                  'area-aligned', 
                                                  'dist-aligned',
                                                  'well-aligned'),
                                                ordered = TRUE))
  break_nums <- seq(0, 100, 20)
  
  spp_gp_quadrants <- spp_gp_quadrants %>% 
    mutate(quad = factor(quad, levels = c('q4', 'q3', 'q2', 'q1'))) %>%
    transform(spp_group_text = reorder(spp_group_text, pct_q1))
  
  barchart_spp_gp_quads <- ggplot(spp_gp_quadrants, 
                                  aes(x = spp_group_text, 
                                      y = pct_quad,
                                      fill = quad, 
                                      weight = pct_quad)) +
    ggtheme_basic(textsize = 12) +
    geom_bar(stat = 'identity', alpha = 1) +
    scale_fill_manual(values = c('q1' = '#4dac26',
                                 'q2' = '#b8e186', 
                                 'q3' = '#f1b6da',
                                 'q4' = '#d01c8b'),
                      labels = quad_names$quad_name,
                      guide = guide_legend(reverse = TRUE)) +
    scale_y_continuous(expand = c(0, 0), 
                       limits = c(0, 1.1),
                       breaks = break_nums/100,
                       labels = sprintf('%s%%', break_nums)) + 
    ### add grid lines; horizontal but then get flipped
    geom_hline(yintercept = break_nums/100, size = 0.25, color = 'white', alpha = .5) +
    ### add text
    geom_text(aes(label = sprintf('n = %s', n_spp), y = 1.01), hjust = 0, 
              size = 3,
              color = 'grey30') +
    coord_flip() +
    labs(x = 'Taxonomic Group', 
         y = 'Percent of species by quadrant', 
         fill = 'Alignment')
  
  return(barchart_spp_gp_quads)
  
}

create_quadplot <- function(taxa_sel, expt_rev) {
  message('in create_quadplot()')
  
  if(taxa_sel == 'all') {
    quad_list_tmp <- quad_list
  } else {
    quad_list_tmp <- quad_list %>%
      filter(spp_group_text == taxa_sel)
  }
  
  if(expt_rev != 'all') {
    quad_list_tmp <- quad_list_tmp %>%
      filter(reviewed)
  }
  
  ### define windows for labels
  q1 <- c('x1' = 84, 'x2' = 98, 'y1' = 92.5, 'y2' = 97.5)
  q2 <- c('x1' =  2, 'x2' = 22, 'y1' = 92.5, 'y2' = 97.5)
  q3 <- c('x1' = 84, 'x2' = 98, 'y1' =  2.5, 'y2' = 7.5)
  q4 <- c('x1' =  2, 'x2' = 18, 'y1' =  2.5, 'y2' = 7.5)
  
  quad_list_labs <- quad_list_tmp %>%
    rename(x = area_ratio, y = dist_align) %>%
    mutate(fade = FALSE,
           fade = ifelse((x > q1[1] & x < q1[2] & y > q1[3] & y < q1[4]), TRUE, fade),
           fade = ifelse((x > q2[1] & x < q2[2] & y > q2[3] & y < q2[4]), TRUE, fade),
           fade = ifelse((x > q3[1] & x < q3[2] & y > q3[3] & y < q3[4]), TRUE, fade),
           fade = ifelse((x > q4[1] & x < q4[2] & y > q4[3] & y < q4[4]), TRUE, fade)) %>%
    rename(area_ratio = x, dist_align = y)
    
  
  
  scatter_quadplot <- ggplot(quad_list_labs,
                             aes(x = area_ratio, 
                                 y = dist_align,
                                 key = name,
                                 key2 = reviewed)) +
    ggtheme_basic(textsize = 12) +
    ### color the quadrant backgrounds:
    annotate('rect', xmin = area_align_mean, xmax = 100, 
             ymin = dist_align_mean, ymax = 100, 
             alpha = .3, 
             fill= '#4dac26')  + 
    annotate('rect', xmax = area_align_mean, xmin =   0, 
             ymin = dist_align_mean, ymax = 100, 
             alpha = .3, 
             fill= '#b8e186') + 
    annotate('rect', xmin = area_align_mean, xmax = 100, 
             ymax = dist_align_mean, ymin =   0, 
             alpha = .3, 
             fill= '#f1b6da') + 
    annotate('rect', xmax = area_align_mean, xmin =   0, 
             ymax = dist_align_mean, ymin =   0, 
             alpha = .3, 
             fill= '#d01c8b') + 
    geom_point(data = quad_list_labs %>% filter(!fade),
               color = '#4d4dac', alpha = .6) + 
    geom_point(data = quad_list_labs %>% filter(fade),
               color = '#4d4dac', alpha = .15)
  
  ### Manage scales for color and size 
  scatter_quadplot <- scatter_quadplot +
    scale_x_continuous(expand = c(0, 0), 
                       limits = c(-1, 101),
                       breaks = c(seq(0, 100, 25)),
                       labels = c('0%', '25%', '50%', '75%', '100%')) +
    scale_y_continuous(expand = c(0, 0),
                       limits = c(-1, 101),
                       breaks = c(seq(0, 100, 25)),
                       labels = c('0%', '25%', '50%', '75%', '100%'))
  
  ### here are quadrant and mean labels:
  scatter_quadplot <- scatter_quadplot +
    annotate('text', x = 91, y = 95, hjust = 1, vjust = .5, size = 3, color = 'grey20', 
             fontface = 'bold', label = 'Well-aligned') + 
    annotate('text', x =  12, y = 95, hjust = 0, vjust = .5, size = 3, color = 'grey20',
             fontface = 'bold', label = 'Distribution-aligned') + 
    annotate('text', x = 91, y =  5, hjust = 1, vjust = .5, size = 3, color = 'grey20',
             fontface = 'bold', label = 'Area-aligned') + 
    annotate('text', x =  10, y =  5, hjust = 0, vjust = .5, size = 3, color = 'grey20',
             fontface = 'bold', label = 'Poorly aligned') +
  
    annotate(geom = 'text',
             x = area_align_mean, y = 5,
             hjust = 0, vjust = 0,
             color = 'grey30', 
             size = 2,
             fontface = 'bold.italic', angle = 90,
             label = sprintf('Mean = %s%%', round(area_align_mean, 1))) +
    annotate(geom = 'text',
             x = 5, y = dist_align_mean,
             hjust = 0, vjust = 0,
             color = 'grey30', 
             size = 2,
             fontface = 'bold.italic', angle = 0,
             label = sprintf('Mean = %s%%', round(dist_align_mean, 1)))
  
  scatter_quadplot <- scatter_quadplot +
    labs(x = 'Area ratio', 
         y = 'Distribution alignment')
  
  return(scatter_quadplot)
}

create_miniquad <- function(spp_sel) {
  message('in create_miniquad()')
  
  scatter_miniquad <- ggplot(quad_list %>% 
                               filter(name == spp_sel),
                             aes(x = area_ratio, 
                                 y = dist_align)) +
    ggtheme_basic(textsize = 8) +
    theme(panel.grid.major = element_line(color = 'grey80'),
          axis.text  = element_blank()) +
    ### color the quadrant backgrounds:
    annotate('rect', xmin = area_align_mean, xmax = 100, 
             ymin = dist_align_mean, ymax = 100, 
             alpha = .3, 
             fill= '#4dac26')  + 
    annotate('rect', xmax = area_align_mean, xmin =   0, 
             ymin = dist_align_mean, ymax = 100, 
             alpha = .3, 
             fill= '#b8e186') + 
    annotate('rect', xmin = area_align_mean, xmax = 100, 
             ymax = dist_align_mean, ymin =   0, 
             alpha = .3, 
             fill= '#f1b6da') + 
    annotate('rect', xmax = area_align_mean, xmin =   0, 
             ymax = dist_align_mean, ymin =   0, 
             alpha = .3, 
             fill= '#d01c8b') + 
    geom_point(data = quad_list, 
               aes(x = area_ratio, y = dist_align),
               # color = '#4d4dac', alpha = .2) +
               color = 'grey50', alpha = .1) +
    geom_point(color = 'red3', size = 3, alpha = .8) +
    labs(x = 'Area ratio', 
         y = 'Dist. align') +
    
    coord_cartesian(xlim = c(0, 100), ylim = c(0, 100), expand = FALSE)
  
  return(scatter_miniquad)
}

########################################.
##### functions for coral maps tab #####
########################################.

create_coralquad <- function(coral_spp) {
  message('in create_coralquad()')
  ### basically a mini-quad showing the before and after of the coral species
  # coral_spp <- spp_coral_align$name[1]
  
  spp_coralmap <- spp_coral_align %>%
    filter(name == coral_spp & method == 'all depth') %>%
    dplyr::select(name, area_ratio, dist_align) %>%
    left_join(spp_coral_align %>%
                filter(name == coral_spp & method != 'all depth') %>%
                dplyr::select(name, area_ratio_clip = area_ratio, dist_align_clip = dist_align),
              by = 'name')
                
  coral_quad <- ggplot(spp_coralmap,
                          aes(x = area_ratio, y = dist_align)) +
    ggtheme_basic(textsize = 8) +
    theme(panel.grid.major = element_line(color = 'grey80'),
          axis.text  = element_blank()) +
    ### color the quadrant backgrounds:
    annotate('rect', xmin = area_align_mean, xmax = 100, 
             ymin = dist_align_mean, ymax = 100, 
             alpha = .3, 
             fill= '#4dac26')  + 
    annotate('rect', xmax = area_align_mean, xmin =   0, 
             ymin = dist_align_mean, ymax = 100, 
             alpha = .3, 
             fill= '#b8e186') + 
    annotate('rect', xmin = area_align_mean, xmax = 100, 
             ymax = dist_align_mean, ymin =   0, 
             alpha = .3, 
             fill= '#f1b6da') + 
    annotate('rect', xmax = area_align_mean, xmin =   0, 
             ymax = dist_align_mean, ymin =   0, 
             alpha = .3, 
             fill= '#d01c8b') + 
    geom_point(data = quad_list, 
               aes(x = area_ratio, y = dist_align),
               # color = '#4d4dac', alpha = .2) +
               color = 'grey50', alpha = .1) +
    geom_point(data = spp_coral_align %>%
                 filter(method == 'all depth'), 
               aes(x = area_ratio, y = dist_align, group = iucn_sid),
               color = 'grey60', alpha = .1) +
    ### plot start point, then end point, then segment
    geom_segment(aes(xend = area_ratio_clip, yend = dist_align_clip),
                 color = 'grey30', size = .6,
                 arrow = arrow(angle = 15, type = 'closed', length = unit(.16, 'inches'))) +
    geom_point(color = 'grey40', size = 3, show.legend = FALSE) +
    geom_point(aes(x = area_ratio_clip, y = dist_align_clip), 
               color = 'red3', size = 3, show.legend = FALSE) +
    labs(x = 'Area ratio', 
         y = 'Dist. align') +
    
    coord_cartesian(xlim = c(0, 100), ylim = c(0, 100), expand = FALSE)
  
  return(coral_quad)
}

create_coral_map <- function(coral_spp) {
  message('in create_coral_map()')
  
  coral_spp_id <- coral_spp_list %>%
    filter(name == coral_spp)
  
  coral_cells <- spp_coral_cells %>%
    filter(iucn_sid == coral_spp_id$iucn_sid) %>%
    distinct()
  
  rast_coral_depth <- raster::subs(x = loiczid_raster, 
                                  y = coral_cells %>% dplyr::select(loiczid, deep), 
                                  by = 'loiczid', which = 'deep', 
                                  subsWithNA = TRUE) %>%
    crop(extent(c(-180, 180, -63, 85)))
  
  map_coral_depth <- 
    tm_shape(bathy_rast) +
    tm_raster(breaks = c(0, 1, Inf),
              palette = c('#f6faff', '#d6daee'),
              auto.palette.mapping = FALSE,
              labels  = c('< 200 m', '> 200 m'),
              colorNA = NULL,
              title = 'Ocean depth',
              alpha = 1) +
    tm_shape(rast_coral_depth) +
      tm_raster(breaks = c(0, 1),
                style   = 'cat',
                palette = c('coral1', 'coral4'),
                auto.palette.mapping = FALSE,
                labels  = c('< 200 m', '> 200 m'),
                colorNA = NULL,
                title = 'IUCN coral presence',
                alpha = 1) +
    tm_shape(land, is.master = TRUE) +
      tm_polygons(border.col = 'grey25', 
                  col = 'grey80', 
                  lwd = 0.25)
  
  map_coral_depth <- map_coral_depth +
    tm_layout(bg.color = '#f6faff',
              legend.text.size = 1,
              legend.title.size = 1.2,
              legend.outside = FALSE,
              legend.position = c('left', 'bottom'),
              legend.bg.color = 'white',
              legend.bg.alpha = .5,
              outer.margins = 0, inner.margins = 0, asp = 2.1)
  
  return(map_coral_depth)
}

# create_coral_barchart <- function() {
#   message('in create_coral_barchart()')
#   
#   coral_quads <- read_csv('data/coral_quads_app.csv')
# 
#   coral_quads <- coral_quads %>%
#     mutate(quad_name = factor(quad_name, 
#                               levels = c('poorly aligned', 
#                                          'area-aligned', 
#                                          'dist-aligned',
#                                          'well-aligned'),
#                               ordered = TRUE),
#            quad      = factor(quad, levels = c('q4', 'q3', 'q2', 'q1'), 
#                               ordered = TRUE))
# 
#   quad_names <- c('poorly aligned', 'area-aligned', 
#                   'dist-aligned',   'well-aligned')
#   break_nums <- seq(0, 100, 20)
#   
#   ### Plot the bar chart
#   barchart_coral_quads <- ggplot(coral_quads, 
#                                  aes(x = method, fill = quad, weight = pct_quad)) +
#     ggtheme_basic(textsize = 12) +
#     geom_bar(stat = 'count', alpha = 1) +
#     scale_fill_manual(values = c('q4' = '#d01c8b', 
#                                  'q3' = '#f1b6da', 
#                                  'q2' = '#b8e186',
#                                  'q1' = '#4dac26'),
#                       labels = quad_names,
#                       guide = guide_legend(reverse = TRUE)) +
#     scale_y_continuous(expand = c(0, 0), 
#                        limits = c(0, 1),
#                        breaks = break_nums/100,
#                        labels = sprintf('%s%%', break_nums)) + 
#     ### add grid lines; horizontal but then get flipped
#     geom_hline(yintercept = break_nums/100, size = 0.25, color = 'white', alpha = .5) +
#     coord_flip() +
#     labs(x = 'Depth limit', 
#          y = 'Percent of corals by quadrant', 
#          fill = 'Alignment')
#   
#   # barchart_coral_quads
#   
#   return(barchart_coral_quads)
#   
# }
