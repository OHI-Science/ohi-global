## Overview
## calculate_scores.R calculates scores for all OHI dimensions (status, trend, pressures, resilience, likely future state, and overall Index scores).


## set working directory 
setwd('ESM_270')

# load scenario configuration. "Conf" is an ohicore function that loads all materials from "conf" folder
conf = Conf('conf')

# run checks on data layers. this ohicore function checks that the data files in "layers"
# folder match their registration information in "layers.csv". 

CheckLayers('layers.csv', 'layers', flds_id=conf$config$layers_id_fields)

# load scenario layers. this ohicore function compiles all data layers into one large data file for
# easy extraction of individual layers for calculation. It doesn't manipulate
# the data themselves.

layers = Layers('layers.csv', 'layers')


# calculate scores
scores = CalculateAll(conf, layers, debug=F)
write.csv(scores, 'scores.csv', na='', row.names=F)

# lookup table of region names and ID numbers
rgn_names = read.csv('layers/rgn_labels.csv')
