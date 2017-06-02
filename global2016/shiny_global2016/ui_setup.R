library(tidyverse) 
library(plotly)
library(shinythemes)

continents <- read_csv('data/georegion_labels.csv') %>%
  .$continent %>%
  unique()
