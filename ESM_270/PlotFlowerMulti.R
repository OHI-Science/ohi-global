## PlotFlowerMulti.r

library(dplyr) #install.packages('dplyr')
# TODO: change to read_csv

PlotFlowerMulti <- function(scores          = read.csv('scores.csv'), # dataframe with regions, goals, dimensions, scores
                            rgns_to_plot, # vector of regions to plot. Can be single many, eg 1 or c(1, 5)
                            rgn_names       = read.csv('layers/rgn_global.csv'),
                            conf            = read.csv('conf/goals.csv'),
                            fld_value_id    = 'region_id', # header of scores variable; likely 'rgn_id' or 'region_id'
                            fld_value_score = 'score', # header of scores variable; likely 'score' or 'value'
                            dim_choice      = 'score', # header of scores variable; likely "future", "pressures", "resilience", "score", "status", "trend"
                            print_fig       = FALSE,
                            save_fig        = TRUE,
                            name_fig        = 'reports/figures/flower',
                            # scale_label     = 'score',
                            # scale_limits    = c(0, 100),
                            overwrite       = TRUE, 
                            color_scheme    = 'new' ) { 
  # TODO: interactive = FALSE
  # DEBUG: scores=read.csv('scores.csv'); rgn_names = read.csv('layers/rgn_global.csv'); fld_value_id = 'region_id'; fld_value_score = 'score';dim_choice = 'score'; print_fig = TRUE; save_fig = TRUE; path_figures = 'reports/figures'; scale_label = 'score';  scale_limits = c(0, 100);overwrite = TRUE; color_scheme = 'new'
  
  ## setup ----
  
  ## check field values in scores column names
  if ( !fld_value_score %in% names(scores) | !fld_value_id %in% names(scores) ) {
    stop(sprintf('Column name "%s" or "%s" not found in scores variable, please modify PlotFlower() function call.',
                 fld_value_score, fld_value_id))
  }
  
  ## if exists, filter dimension for 'score'
  if ( 'dimension' %in% names(scores) ) {
    scores <- scores %>%
      dplyr::filter(dimension == dim_choice)
  }
  
  
  ## weights for FIS vs. MAR 
  # ##TODO: add this as variable to read in
  # weights_fis <- read_csv(sprintf("layers/fp_wildcaught_weight.csv")) %>%
  #   rbind(data.frame(rgn_id=0, w_fis=mean(weights$w_fis)))
  # 
  
  ## getting the goals that will be plotted:
  # conf <-  read_csv("conf/goals.csv") # temporarily putting this in the function call 
  
  goals_supra = na.omit(unique(conf$parent)) # goals comprised of subgoals, not included in plot
  
  conf <- conf %>%
    dplyr::filter(!(goal %in% goals_supra)) %>%
    dplyr::select(goal, order_color, order_hierarchy, weight, name_flower) %>%
    dplyr::mutate(name_flower = gsub("\\n", "\n", name_flower, fixed = TRUE)) %>%
    dplyr::mutate(goal = as.character(goal)) %>%
    dplyr::arrange(order_hierarchy)
  
  scores <- scores %>%
    # TODO: don't know what this did @Mel?  mutate(region_id = ifelse(region_id==300, 0, region_id)) %>%   #convert the 300 (i.e., only eez's averaged to zero)
    dplyr::mutate(goal = as.character(goal)) %>%
    dplyr::filter(region_id <= 250) %>%       # get rid of high seas regions
    dplyr::filter(region_id != 213)
  
  # TODO, not sure this is needed
  # region names, ordered by GLOBAL and alphabetical
  # rgn_names2 = rbind(
  #   data.frame(
  #     region_id=0, 
  #     country='GLOBAL'),
  #   rgn_names) %>%
  #   arrange(country) %>%
  #   filter(region_id != 213)
  
  
  ## loop through regions
  for (r in rgns_to_plot){  # r=1
    
    ## region name for title
    rgn_name = rgn_names %>%
      dplyr::filter(rgn_id == r)
    rgn_name = rgn_name$label
    print(sprintf('Flower Plot for %s (rgn_id %s) . . .', rgn_name, r))
    
    ## combine to goal weighting
    scores_r = scores %>%
      dplyr::filter(region_id == r) %>%
      dplyr::inner_join(conf, by="goal") %>%
      dplyr::arrange(order_color)
    
    score_Index <-  subset(scores, region_id==r & goal == 'Index', score, drop=T)
    
    # get colors for aster, based on 10 colors, but extended to all goals. subselect for goals.wts
    if(color_scheme == "new"){
      scores_r$cols.goals.all = as.character(  # note! if color scheme is all crazy, make sure it's not a factor!
        cut(scores_r$score, 
            breaks=seq(0, 100, by=10), 
            include.lowest=TRUE, 
            labels=RColorBrewer::brewer.pal(10, 'RdYlBu'))) 
    } else {
      # TODO: update this 
      # scores_r$cols.goals.all = colorRampPalette(RColorBrewer::brewer.pal(11, 'Spectral'), space='Lab')(length(goals.all))
    }
    
    # #TODO: fix this weights after correcting for fisheries/mariculture contributions
    # scores_r$weight[scores_r$goal == "FIS"] <-   weights$w_fis[weights$rgn_id == rgn_id]
    # scores_r$weight[scores_r$goal == "MAR"] <- 1 - weights$w_fis[weights$rgn_id == rgn_id]
    
    ## flower plot ----
    
    ## fig name 
    fig_png = sprintf('%s_%s.png', name_fig, gsub(' ','_', rgn_name))
   
    res=72
    if (overwrite | !file.exists(fig_png)){
       png(fig_png, width=res*7, height=res*7, bg = "transparent")
      
      
      # TODO: also a pdf    
       # fig_pdf = sprintf('%s/flowerPlots/flower_%s.pdf', path_figures, gsub(' ','_', rgn_name))
      # if (overwrite | !file.exists(fig_pdf)){
      #       pdf(fig_pdf)
      
     PlotFlower(main       = rgn_name,
                 lengths    = ifelse(is.na(scores_r$score), 100, scores_r$score),
                 widths     = scores_r$weight,
                 fill.col   = ifelse(is.na(scores_r$cols.goals.all), 'grey80', scores_r$cols.goals.all),
                 labels     = ifelse(is.na(scores_r$score), 
                                     paste(scores_r$name_flower, '-', sep='\n'), 
                                     paste(as.character(scores_r$name_flower), round(scores_r$score), sep='\n')),
                 center     = round(score_Index),
                 max.length = 100, disk=0.4, label.cex=0.9, label.offset=0.155, cex=2.2, cex.main=2.5)
      dev.off()      
      
      # repeat it so that it prints in the Plot window; need to change PlotFlower so that this is not needed. 
      PlotFlower(main       = rgn_name,
                 lengths    = ifelse(is.na(scores_r$score), 100, scores_r$score),
                 widths     = scores_r$weight,
                 fill.col   = ifelse(is.na(scores_r$cols.goals.all), 'grey80', scores_r$cols.goals.all),
                 labels     = ifelse(is.na(scores_r$score), 
                                     paste(scores_r$name_flower, '-', sep='\n'), 
                                     paste(as.character(scores_r$name_flower), round(scores_r$score), sep='\n')),
                 center     = round(score_Index),
                 max.length = 100, disk=0.4, label.cex=0.9, label.offset=0.155, cex=2.2, cex.main=2.5)
      
      # return(p)
      #system(sprintf('convert -density 150x150 %s %s', fig_pdf, fig_png)) # imagemagick's convert
    }
    
    # flower md this was for the rmd
    #   cat(sprintf('![flower plot of %s](figures/%s)\n\n', rgn_name, basename(fig_png)))
    
    
    
    
  }
}