# presume that working directory in current scenario directory, eg:
# setwd('~/github/ohi-global/eez2013')
library(ohicore)

# load conf
conf = Conf('conf')

# run checks on layers
#CheckLayers('layers.csv', 'layers', flds_id=conf$config$layers_id_fields)

# load layers
layers = Layers('layers.csv', 'layers')

# calculate scores
scores = CalculateAll(conf, layers, debug=T)
write.csv(scores, 'scores.csv', na='', row.names=F)
