### request to make PDF version of flowerplots

#load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(hwriter)
library(RColorBrewer)
library(knitr)
library(googleVis)
library(ohicore)
library(sp)
library(rgdal)
library(DT)
library(broom)

# Set working directory when not knitting:
# setwd("global2017/Results")

goals <- c('Index', 'AO', 'SPP', 'BD', 'HAB', 'CP', 'CS', 'CW', 'ECO', 'LE', 'LIV', 'FIS', 'FP', 'MAR', 'ICO', 'SP', 'LSP', 'NP', 'TR')
goal_names <- data.frame(goal=goals, long_goal=c("Index", 
                                                 "Artisanal opportunities",
                                                 "Species condition (subgoal)",
                                                 "Biodiversity",
                                                 "Habitat (subgoal)",
                                                 "Coastal protection",
                                                 "Carbon storage",
                                                 "Clean water",
                                                 "Economies",
                                                 "Livelihoods & economies",
                                                 "Livelihoods",
                                                 "Fisheries (subgoal)",
                                                 "Food provision",
                                                 "Mariculture (subgoal)",
                                                 "Iconic species (subgoal)",
                                                 "Sense of place",
                                                 "Lasting special places (subgoal)",
                                                 "Natural products",
                                                 "Tourism & recreation"))

## General settings to control
scenario <- "2017" #identify scenario of focus (this can be changed to obtain data for other years)
benchmark = 2016  # year that is used for old vs. new OHI analyses
oldCommit = 'ce63333cea0c556498a361744acf27d9d966c87b' # sha commit for 2015 analysis
colorScheme <- 'new'  # color scheme to use on flower plots ("new" = color reflects size score and is not the original rainbow)
saveFile <- 'global2017' #location where files that are created are to be saved

## General files to load
rgn_names <- read.csv(sprintf('../../eez/layers/rgn_global.csv', scenario)) %>%
  dplyr::select(region_id = rgn_id, country = label) %>%
  dplyr::mutate(country = as.character(country))

# the date in the below variable must be updated (for the results to reflect the new data)
dateFile = '2017-11-22' #date extension on data files used for all tables/figures



source('flowerPlot_pdf.R')

PlotFlower(
  region_plot     = NA,
  dir_fig_save = "figures/FlowerPlots/pdf_version",
  assessment_name = "Global average",
  year_plot = 2017,
  scores_file = sprintf("../OHI_final_scores_%s.csv", dateFile),
  scenario_folder = "../../eez"
)

