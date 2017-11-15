suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(sp)
  library(geojsonio)
  library(leaflet)
  library(htmltools)
  library(DT)
  library(knitr)
  library(printr)  # devtools::install_github('yihui/printr')
  library(ohicore) # devtools::install_github('ohi-science/ohicore')
})

## brewed vars
study_area      <- "Global"
key             <- "ohi-global"
dir_scenario_gh <- "https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/eez"

## knitr options for all webpages
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)

## read in variables if they exist (i.e. don't try for prep repos)
scores_csv <- file.path(dir_scenario_gh, 'scores.csv')
layers_csv <- file.path(dir_scenario_gh, 'layers.csv')
conf_csv   <- file.path(dir_scenario_gh, 'conf/goals.csv')

## if statements in case this is an OHI+ prep repo without these files
if (RCurl::url.exists(scores_csv)) scores <- readr::read_csv(scores_csv)
if (RCurl::url.exists(layers_csv)) layers <- readr::read_csv(layers_csv)
if (RCurl::url.exists(conf_csv))   weight <- readr::read_csv(conf_csv) %>%
  select(goal, weight)

## save local copy of conf/web/goals.Rmd
conf_goals_rmd <- file.path(dir_scenario_gh, 'conf/web/goals.Rmd')

if (RCurl::url.exists(conf_goals_rmd)) {
  conf_goals <- readr::read_lines(conf_goals_rmd)
  readr::write_lines(conf_goals, path = 'conf_goals.Rmd', append = FALSE)
}

## save local copy of conf/web/layers_all.Rmd
layers_all_rmd <- file.path(dir_scenario_gh, 'conf/web/layers_all.Rmd')

if (RCurl::url.exists(layers_all_rmd)) {
  layers_all <- readr::read_lines(layers_all_rmd)
  readr::write_lines(layers_all, path = 'layers_all.Rmd', append = FALSE)
}

