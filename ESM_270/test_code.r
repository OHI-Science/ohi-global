# testing setup with Steve at Bren
setwd('ESM_270')
library(ohicore)
source('calculate_scores.r') # will ~30 seconds
source('PlotFlowerMulti.r')
PlotFlowerMulti(scores       = read.csv('scores.csv'), 
                rgns_to_plot = 163, 
                conf         = read.csv('conf/goals.csv'), 
                name_fig     = 'reports/figures/flower_orig') # should pop up a figure in the Viewer pane bottom right (no worries if it looks cut off; it also saves a .png in the ohi-global/ESM_270/reports/figures/ folder).
