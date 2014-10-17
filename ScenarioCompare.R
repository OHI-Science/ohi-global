setwd("C:/Users/Melanie/github/ohi-global")

source('../ohiprep/src/R/common.R')

# new paths based on host machine
dirs = list(
  neptune_data  = dir_neptune_data, 
  neptune_local = dir_neptune_local,
  ohiprep       = '../ohiprep',
  ohicore       = '../ohicore')

library(devtools)
install_github("git2r", "ropensci")
devtools::load_all(dirs$ohicore)

# devtools::install_github('ohi-science/ohicore')
# library(ohicore)
launch_cmp()
