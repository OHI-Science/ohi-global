source("global2017/Results/PlotFlower_draft.R")

library(dplyr)
PlotFlower(region_plot     = NA,
                       year_plot       = NA,
                       assessment_name = "Global average",
                       scenario_folder   = "eez",
                       scores_file = "scores.csv",
                       dir_fig_save    = "global2018/figures/flowerplots_2018",
                       save = TRUE,
                       legend_include = FALSE)
list.files("global2018/figures/flowerplots_2018", pattern="png")
