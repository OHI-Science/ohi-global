# presume that working directory in current scenario directory, eg:
devtools::load_all('~/Documents/GitHub/ohicore')
library(ohicore) # devtools::install('~/github/ohicore')

# load conf
conf = Conf('conf')

# run checks on layers
CheckLayers('layers.csv', 'layers', flds_id=conf$config$layers_id_fields)

# load layers
layers = Layers('layers.csv', 'layers')

# calculate scores
scores = CalculateAll(conf, layers, debug=F)
write.csv(scores, 'scores.csv', na='', row.names=F)
