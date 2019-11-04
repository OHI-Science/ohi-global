# _site.R will copy metadata information from the ohi-global assessment branch
# (published branch for release; draft branch for development).
# It copies these files from the raw.githubusercontent url and saves them with a `local_`
# prefix. When the website is built with `rmarkdown::render_site()`, they will
# be knit-childed into the correct page; for example `local_goals.Rmd` will be
# knit-childed into `goals.Rmd` and become ohi-science.org/ohi-global/goals.html.

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
dir_scenario_gh <- "https://raw.githubusercontent.com/OHI-Science/ohi-global/draft" # ultimately, published branch!



## knitr options for all webpages
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)

## read in variables if they exist (i.e. don't try for prep repos)
scores_csv <- file.path(dir_scenario_gh, 'eez/scores.csv')
layers_csv <- file.path(dir_scenario_gh, 'eez/layers.csv')
conf_csv   <- file.path(dir_scenario_gh, 'eez/conf/goals.csv')

## if statements in case this is an OHI+ prep repo without these files
if (RCurl::url.exists(scores_csv)) scores <- readr::read_csv(scores_csv) else stop(sprintf("error, %s file missing", scores_csv))
if (RCurl::url.exists(layers_csv)) layers <- readr::read_csv(layers_csv) else stop(sprintf("error, %s file missing", layers_csv))
if (RCurl::url.exists(conf_csv))   goals  <- readr::read_csv(conf_csv)   else stop(sprintf("error, %s file missing", conf_csv))

# write a message if any are false!
weight <- goals %>%
  select(goal, weight)


## save local copies of Rmds to knit-child ----


to_copy <- c('documents/website/goals.Rmd',
             'documents/website/layers_all.Rmd',
             'documents/website/layers_table.Rmd',
             'documents/website/OHI.bib',
             'documents/website/methods-in-ecology-and-evolution.csl')


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
