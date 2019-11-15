#  Flowerplots for Antarctica

#load libraries
library(dplyr)
library(tidyr)
library(hwriter)
library(RColorBrewer)
library(knitr)
library(googleVis)
library(ohicore)
library(sp)
library(rgdal)


goals <- c('Index', 'AO', 'SPP', 'BD', 'HAB', 'CP', 'CS', 'CW', 'ECO', 'LE', 'LIV', 'FIS', 'FP', 'MAR', 'ICO', 'SP', 'LSP', 'NP', 'TR')

#---------------------------
## General settings to control
colorScheme <- 'new'  # color scheme to use on flower plots ("new" = color reflects size score and is not the original rainbow)
scenario <- 2015


# getting the goals that will be plotted:
conf <-  read.csv(sprintf("antarctica%s_seaice_explore/conf/goals.csv", scenario), stringsAsFactors=FALSE) 

goals_supra = na.omit(unique(conf$parent)) # goals comprised of subgoals, not included in plot

conf <- conf %>%
  filter(!(goal %in% goals_supra)) %>%
  select(goal, order_color, order_hierarchy, weight, name_flower) %>%
  mutate(name_flower = gsub("\\n", "\n", name_flower, fixed = TRUE)) %>%
  arrange(order_hierarchy)


data <- read.csv(sprintf('antarctica%s_seaice_explore/scores.csv', scenario), stringsAsFactors=FALSE) 

data <- data %>%
  filter(dimension == "score")   # focus only on score data

# region names, ordered by GLOBAL and alphabetical
rgn_names2 = unique(data$region_id)

# loop through regions to plot flowers
for (rgn_id in rgn_names2){  #rgn_id=248100
  
  # header md
  message(sprintf('\n## %s\n\n', rgn_id))
  
  # region scores    
  g_x <- subset(data, region_id==rgn_id) %>%
    inner_join(conf, by="goal") %>%
    arrange(order_color)
  x <-  subset(data, region_id==rgn_id & goal == 'Index', score, drop=T)
  
  # get colors for aster, based on 10 colors, but extended to all goals. subselect for goals.wts
  if(colorScheme == "new"){
    g_x$cols.goals.all = cut(g_x$score, breaks=seq(0, 100, by=10), include.lowest=TRUE, 
                             labels=RColorBrewer::brewer.pal(10, 'RdYlBu')) } else {
                               g_x$cols.goals.all = colorRampPalette(RColorBrewer::brewer.pal(11, 'Spectral'), space='Lab')(length(cols.goals.all))
                             }
  

  # res=72
  
  res=150
  ## start plot
  png(sprintf('global2015/AntarcticaReporting_seaice_explore/flowers/flower_%s_%s.png', unique(g_x$region_id), scenario),
      width=res*6, height=res*6, bg = "transparent")
  #par(oma=c(0,0,3,0), mar=c(6, 4, 0, 2) + 0.1)
  PlotFlower(main = unique(g_x$region_id),
             lengths=ifelse(
               is.na(g_x$score),
               100,
               g_x$score),
             widths=g_x$weight,
             fill.col=ifelse(
               is.na(g_x$cols.goals.all), 
               'grey80', 
               as.character(g_x$cols.goals.all)),
             labels  =ifelse(
               is.na(g_x$score), 
               paste(g_x$name_flower, '-', sep='\n'), 
               paste(as.character(g_x$name_flower), round(g_x$score), sep='\n')),
             center=round(x),
             #  max.length = 100, disk=0.4, label.cex=0.9, label.offset=0.155, cex=2.2, cex.main=2.5)
             max.length = 100, disk=0.3, label.cex=1.5, label.offset=0.15, cex=3, cex.main=3)
  
  dev.off()      
  #system(sprintf('convert -density 150x150 %s %s', fig_pdf, fig_png)) # imagemagick's convert
  
  
  }
