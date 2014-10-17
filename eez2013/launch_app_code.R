# setwd('~/github/ohi-global/eez2013')
# require(methods)
# #require(ohicore) # 
# devtools::load_all('~/github/ohicore')
# #options(shiny.error=traceback)
# #options(shiny.error=recover)
# launch_app()

# for (p in c('ohicore','ohigui','rCharts')){  
#   if (p %in% rownames(installed.packages())){
#     lib = subset(as.data.frame(installed.packages()), Package==p, LibPath, drop=T)
#     remove.packages(p, lib)  
#   }
# }
# 
# # install dependencies
# for (p in c('devtools')){
#   if (!require(p, character.only=T)){
#     install.packages(p)
#     require(p, character.only=T)
#   }
# }
# 
# # install packages
# install_github('ohi-science/rCharts')
# install_github('ohi-science/ohicore')

# get scenarios and launch
#library(ohicore)
devtools::load_all('~/github/ohicore')
#get_scenarios('ohi-science/ohi-global', '~/ohi-global')

options(shiny.error=traceback)
#options(shiny.error=recover)
devtools::load_all('~/github/ohicore')
launch_app('~/github/ohi-global/eez2013')


