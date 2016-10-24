suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(sp)
  library(geojsonio)
  library(leaflet)
  library(htmltools)
  library(DT)
  library(knitr)
  library(printr)  # devtools::install_github('yihui/printr')
  library(ohicore) # devtools::install_github('ohi-science/ohicore')
})

# brewed vars
study_area      = "Global"
gh_repo         = "ohi-global"
gh_branch_data  = "draft"
scenario_dir    = "eez2015"
app_url         = "http://ohi-science.nceas.ucsb.edu/ohi-global"
ohirepos_commit = "91f077d8e3f5fc08e21b04eab26c87d2050b6e9e"
map_shrink_pct  = 5

# derived vars
dir_data        = sprintf('%s_%s', gh_repo, gh_branch_data)
dir_scenario    = sprintf('%s/%s', dir_data, scenario_dir)

# knitr options
knitr::opts_chunk$set(echo = F, message = F, warning = F)

# if dir_data not found, then git clone
if (!file.exists(dir_data)){
  system(sprintf('git clone https://github.com/ohi-science/%s.git %s', dir_data))
}

# read config
config = new.env()
source(file.path(dir_scenario, 'conf/config.R'), config)


