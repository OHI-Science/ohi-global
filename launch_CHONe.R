rm(list=ls())

devtools::load_all('~/Documents/GitHub/ohicore')
require(methods)
require(ohicore)

# populate CHONe folder and modify layers and functions
setwd('~/Documents/GitHub/ohi-global')
#delete
unlink('eezCHONE/conf/*');unlink('eezCHONE/layers/*');unlink('eezCHONE/spatial/*')
unlink('eezCHONE/layers.csv');unlink('eezCHONE/scores.csv');


# create new layers.csv, and layers folder for Canada-CHONe2014 ####################
file.copy('eez2013/layers.csv', 'eezCHONE/layers.csv', overwrite = T)
file.copy('eez2013/scores.csv', 'eezCHONE/scores.csv', overwrite = T)
fl=list.files('eez2013/layers')
file.copy(paste('eez2013/layers/',fl,sep = ""),paste('eezCHONE/layers/',fl,sep = ""), overwrite = T)
fl=list.files('eez2013/conf')
file.copy(paste('eez2013/conf/',fl,sep = ""),paste('eezCHONE/conf/',fl,sep = ""), overwrite = T)
fl=list.files('eez2013/spatial')
file.copy(paste('eez2013/spatial/',fl,sep = ""),paste('eezCHONE/spatial/',fl,sep = ""), overwrite = T)

#modify
setwd('~/Documents/GitHub/ohi-global/eez2013')
#source("../layers_Canada-CHONe2014.R")

# calculate
source("calculate_scores.R")
launch_app()
