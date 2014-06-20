require(ohicore)

wd = "highseas2014"
scenario=list(
  conf   = ohicore::Conf(file.path(wd, "conf")),
  layers = ohicore::Layers(file.path(wd, "layers.csv"), file.path(wd, "layers")),
  scores = read.csv(file.path(wd, "scores.csv"), na.strings=""),
  spatial = file.path(wd, "spatial"),
  dir    = wd)
