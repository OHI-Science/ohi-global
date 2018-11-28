## checking on some habitat data stuff, particularly sea ice

library(here)
library(dplyr)
library(tidyr)

# Quick check of Jan Mayan (rgn_id 144)
habs_list <- list.files(here("eez/layers"), pattern="health", full.names = TRUE)
habs_data <- lapply(habs_list, read.csv)

habs_combined <- bind_rows(habs_data) %>%
  filter(rgn_id == 144)

habs_list <- list.files(here("eez/layers"), pattern="extent", full.names = TRUE)
habs_data <- lapply(habs_list, read.csv)

habs_combined <- bind_rows(habs_data) %>%
  filter(rgn_id == 144)

# has seaice edge (but no shoreline) and soft-bottom habitats
# check with original data
## this matches info. here: http://www.npolar.no/en/themes/climate/indicators/sea-ice/
# but then why does iceland have sea ice: 
# ftp://sidads.colorado.edu/pub/projects/noaa/iicwg/IICWG-2006/1_Monday/Sea%20Ice%20monitoring/iicwg__07.pdf

habs_list <- list.files(here("eez/layers"), pattern="health", full.names = TRUE)
habs_data <- lapply(habs_list, read.csv)
habs_combined <- bind_rows(habs_data) %>%
  filter(rgn_id == 143)
