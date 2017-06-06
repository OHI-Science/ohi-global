library(tidyverse) 
library(plotly)
library(shinythemes)

### initialize variables needed by ui.R
continents <- read_csv('data/georegion_labels.csv') %>%
  .$continent %>%
  unique()


