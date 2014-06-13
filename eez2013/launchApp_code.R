require(methods)
require(ohicore)
require(ohigui)

wd = getwd()
ohigui::launchApp(scenario=list(
  conf   = ohicore::Conf("conf"),
  layers = ohicore::Layers("layers.csv", "layers"),
  scores = read.csv("scores.csv", na.strings=""),
  spatial = file.path(wd, "spatial"),
  dir    = wd), launch.browser=T)
