library(tidyverse) 
library(plotly)
library(shinythemes)

continents <- read_csv('data/georegion_labels.csv') %>%
  .$continent %>%
  unique()

warning(list.files('pages') %>% paste(collapse = ', '))

warning('pages/scoretrend_side1.md File exists? ', file.exists('pages/scoretrend_side1.md'))
