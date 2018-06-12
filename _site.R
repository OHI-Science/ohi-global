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
dir_scenario_gh <- "https://raw.githubusercontent.com/OHI-Science/ohi-global/published/eez"



## knitr options for all webpages
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)

## read in variables if they exist (i.e. don't try for prep repos)
scores_csv <- file.path(dir_scenario_gh, 'scores.csv')
layers_csv <- file.path(dir_scenario_gh, 'layers.csv')
conf_csv   <- file.path(dir_scenario_gh, 'conf/goals.csv')

## if statements in case this is an OHI+ prep repo without these files
if (RCurl::url.exists(scores_csv)) scores <- readr::read_csv(scores_csv)
if (RCurl::url.exists(layers_csv)) layers <- readr::read_csv(layers_csv)
if (RCurl::url.exists(conf_csv))   goals  <- readr::read_csv(conf_csv)
weight <- goals %>%
  select(goal, weight)


## save local copies of Rmds to knit-child ----


to_copy <- c('eez/conf/web/goals.Rmd',
             'eez/conf/web/layers_all.Rmd',
             'eez/conf/web/layers_table.Rmd',
             'global_supplement/OHI.bib',
             'global_supplement/methods-in-ecology-and-evolution.csl')


for (f in to_copy) { # f <-  'global_supplement/OHI.bib'

  fp <- file.path(dir_scenario_gh, f)

  ## if the url exists, save a copy.
  if (RCurl::url.exists(fp)) {
    f_web   <- readr::read_lines(fp)
    if ( tools::file_ext(fp) == 'Rmd' ) {
      f_local <- paste0('local_', basename(fp))
    } else {
      f_local <- basename(fp)
    }
    readr::write_lines(f_web, path = f_local, append = FALSE)
    message(sprintf('saving %s', f_local))
  } else {
    message(sprintf('%s does not exist', fp))
  }
}
