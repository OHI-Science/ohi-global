# transfer global scenarios out of ohicore
# TODO: create true regions_gcs.js for Antarctica & High Seas
#       eez2014 reshape input data problem once in pressures, many in resilience: Aggregation function missing: defaulting to length

# Melanie access on PC 
#setwd("C:/Users/Melanie/Github/ohi-global")
# access on Mac (is the '/github/' part needed on Macs?)
setwd(file.path('~/github/ohi-global'))
setwd('~/ohi-global')

## check to see if following also works on Mac:
source('../ohiprep/src/R/common.R')

# new paths based on host machine
dirs = list(
  neptune_data  = dir_neptune_data, 
  neptune_local = dir_neptune_local,
  ohiprep       = '../ohiprep',
  ohicore       = '../ohicore')


# # if above works on mac...delete the following
# # get paths based on host machine
# dirs = list(
#   neptune_data  = '/Volumes/data_edit', 
#   neptune_local = '/Volumes/local_edit',
#   ohiprep       = '../ohiprep',
#   ohicore       = '../ohicore')

# load ohicore (must first download using directions from here: )
#library(ohicore) # or 
devtools::load_all(dirs$ohicore)

do.layercopy  = T
do.layercheck = T
do.calculate  = T
do.other      = F
do.merge      = T

# scenarios
scenarios = list(
  eez2014     = list(
    google_key   = '0At9FvPajGTwJdEJBeXlFU2ladkR6RHNvbldKQjhiRlE',
    fld_dir      = 'dir_2014a',
    fld_fn       = 'fn_2014a',
    f_spatial    = c('../ohiprep/Global/NCEAS-Regions_v2014/data/regions_gcs.js'),
    do           = F),
  eez2013     = list(
    google_key   = '0At9FvPajGTwJdEJBeXlFU2ladkR6RHNvbldKQjhiRlE',
    fld_dir      = 'dir_2013a',
    fld_fn       = 'fn_2013a',
    f_spatial    = c('../ohiprep/Global/NCEAS-Regions_v2014/data/regions_gcs.js'),
    do           = T),
  eez2012     = list(
    google_key   = '0At9FvPajGTwJdEJBeXlFU2ladkR6RHNvbldKQjhiRlE',
    fld_dir      = 'dir_2012a',
    fld_fn       = 'fn_2012a',
    f_spatial    = c('../ohiprep/Global/NCEAS-Regions_v2014/data/regions_gcs.js'),
    do           = F),
  antarctica2014 = list(
    google_key   = '0ArcIhYsFwBeNdHNxNk1iRHc1S05KLWsyb0ZtZjRjZnc',
    fld_dir      = 'dir_2013a',
    fld_fn       = 'fn_2013a',
    f_spatial    = c('../ohiprep/Global/NCEAS-Regions_v2014/data/regions_gcs.js'),
    do           = F),
  highseas2014   = list(
    google_key   = '0ArcIhYsFwBeNdG9KVlJ6M0ZxV1dtVDJDQ3FLVWJQWFE',
    fld_dir      = 'dir_2013a',
    fld_fn       = 'fn_2013a',
    f_spatial    = c('../ohiprep/Global/NCEAS-Regions_v2014/data/regions_gcs.js'),
    do           = F))

# sync functions.R: overwrite eez2012 and eez2014 with eez2013 (note LE's use of eez2013 argument)
for (dir in c('eez2012','eez2014')){
  stopifnot(file.copy('eez2013/conf/functions.R', file.path(dir, 'conf/functions.R'), overwrite=T))
}

# get rid of old debug files without scenario prefix
# for (dir in c('eez2012','eez2013','eez2014')){
#   unlink(list.files(file.path(dir, 'reports/debug'), '^np_.*', full.names=T))
# }

for (i in 1:length(scenarios)){ # i=2
  
  # vars
  scenario   = names(scenarios)[[i]]
  fld_dir    = scenarios[[i]][['fld_dir']]
  fld_fn     = scenarios[[i]][['fld_fn']]
  google_key = scenarios[[i]][['google_key']]
  do         = scenarios[[i]][['do']]
  
  #   print(scenario)
  #   print(fld_dir)
  #   print(fld_fn)
  #   print(do)
  
  
  
  if (!do) next()
  
  cat(sprintf('\nScenario: %s\n', scenario))
  
  # create dirs
  dirs_scenario = c(scenario, sprintf('%s/%s', scenario, c('temp','layers','conf','spatial')))
  for (dir in dirs_scenario) {
    if (!file.exists(dir)) dir.create(dir, showWarnings=F)
  }
  
  if (do.layercopy){
    # load Google spreadsheet for copying layers
    cat(sprintf('\n  Google spreadsheet editable URL:\n    https://docs.google.com/spreadsheet/ccc?key=%s\n', google_key) )
    g.url = sprintf('https://docs.google.com/spreadsheet/pub?key=%s&output=csv', scenarios[[i]][['google_key']])
    g = read.csv(textConnection(RCurl::getURL(g.url, ssl.verifypeer = FALSE)), skip=1, na.strings='', stringsAsFactors=F)
    write.csv(g, sprintf('%s/temp/layers_0-google.csv', scenario), na='', row.names=F)
    
    # fill in for 2014
    if (scenario=='eez2014'){
      g = g %>%
        mutate(
          dir_2014a = ifelse(is.na(dir_2014a), dir_2013a, dir_2014a),
          fn_2014a = ifelse(is.na(fn_2014a), fn_2013a, fn_2014a))
    }
    
    # swap dir
    g$dir_in = sapply(
      str_split(g[[fld_dir]], ':'),   
      function(x){ sprintf('%s/%s', dirs[x[1]], x[2])})
    g$fn_in = g[[fld_fn]]

    # filter
    lyrs = g %.%
      filter(ingest==T) %.%
      mutate(
        path_in        = file.path(dir_in, fn_in),
        path_in_exists = file.exists(path_in),
        filename = sprintf('%s.csv', layer),
        path_out = sprintf('%s/layers/%s', scenario, filename)) %.%
      select(
        targets, layer, layer_old, name, description, 
        fld_value, units,
        path_in, path_in_exists, filename, path_out) %.%
      arrange(targets, layer)
    write.csv(lyrs, sprintf('%s/temp/layers_1-ingest.csv', scenario), na='', row.names=F)
    
    if (nrow(filter(lyrs, !path_in_exists)) != 0){
      message('The following layers paths do not exist:\n')
      print(filter(lyrs, !path_in_exists) %.% select(layer, path_in), row.names=F)
      stop('Resolve paths in google doc with filesystem.')
    }
    
    # copy layers
    for (j in 1:nrow(lyrs)){ # j=45
      stopifnot(file.copy(lyrs$path_in[j], lyrs$path_out[j], overwrite=T))
    }
    
    # delete extraneous files
    files_extra = setdiff(list.files(sprintf('%s/layers',scenario)), as.character(lyrs$filename))
    unlink(sprintf('%s/layers/%s', scenario, files_extra))
    
    # layers registry
    write.csv(select(lyrs, -path_in, -path_in_exists, -path_out), sprintf('%s/layers.csv', scenario), row.names=F, na='')
  }
  
  if (do.layercheck){
    # load conf
    conf   = Conf(sprintf('%s/conf', scenario))
    
    # run checks on layers
    CheckLayers(layers.csv = sprintf('%s/layers.csv', scenario), 
                layers.dir = sprintf('%s/layers', scenario), 
                flds_id    = conf$config$layers_id_fields)
    # system(sprintf('open %s/layers.csv', scenario))
  }
  
  if (do.calculate){
    
    # calculate scores from directory of scenario
    setwd(sprintf('%s', scenario)) # load_all(dirs$ohicore)
    
    # load configuration and layers
    conf   = Conf('conf')
    layers = Layers('layers.csv','layers')
    
    # calculate scores
    #try({    })
    scores = CalculateAll(conf, layers, debug=T)
     write.csv(scores, 'scores.csv', na='', row.names=F)
    
    # restore working directory
    setwd('..') 
    
    # archive scores on disk (out of github, for easy retrieval later)
    csv = sprintf('%s/git-annex/Global/NCEAS-OHI-Scores-Archive/scores/scores_%s_%s.csv', dirs$neptune_data, scenario, format(Sys.Date(), '%Y-%m-%d'))
    write.csv(scores, csv, na='', row.names=F)
    
    source('global2014/merge_scores.R')  
  }
  
  if (do.other){
    # spatial  
    for (f in scenarios[[scenario]][['f_spatial']]){ # f = f_spatial[1]
      stopifnot(file.exists(f))
      file.copy(f, sprintf('%s/spatial/%s', scenario, basename(f)))
    }
    
    # delete old shortcut files
    for (f in c('launchApp.bat','launchApp.command','launchApp_code.R','scenario.R')){
      path = sprintf('%s/%s',scenario,f)
      if (file.exists(path)) unlink(path)
    }
    
    # save shortcut files not specific to operating system
    write_shortcuts(scenario, os_files=0)
    
    # launch on Mac # setwd('~/github/ohi-global/eez2013'); launch_app()
    #system(sprintf('open %s/launch_app.command', scenario))
  }
}


# DEBUG comparison for 2013a
source('../ohidev/report/compare_scores.R')

# comparison 2014a
# source('../ohidev/report/compare_scenarios.R')

###############################################33
## resilience calculations
# get resilience layers
rm = conf$resilience_matrix
rm = within(rm, {component[is.na(component)] = ''}) #in component variable replace NA with nothing
rw = conf$resilience_weights
rc = conf$config$resilience_components
rk = conf$config$resilience_categories
r.layers = setdiff(names(rm), c('goal','component','component_name'))
if (!all(subset(layers$meta, layer %in% r.layers, val_0to1, drop=T))){
  message('Error: Not all resilence layers range in value from 0 to 1!')
  print(subset(layers$meta, layer %in% r.layers & val_0to1==F, c('val_min','val_max'), drop=F))
  stop('')    
}
stopifnot(all(r.layers %in% rw$layer))

# setup initial data.frame for column binding results by region
D = rename(SelectLayersData(layers, layers=conf$config$layer_region_labels, narrow=T), c('id_num'='region_id'))[,'region_id',drop=F]
regions = D[['region_id']]

# w.layers: weighting vector [layer]
weights = setNames(rw$weight, rw$layer)

# t: typing vector [layer] 
types = setNames(rw$type, rw$layer)  

# iterate goals
subgoals = subset(conf$goals, !goal %in% unique(conf$goals$parent), goal, drop=T)


# for (g in subgoals){ # 
  g=subgoals[13]
  
  if (debug) cat(sprintf('goal: %s\n', g))    
  r.g = subset(rm, goal==g)
  
#   if (nrow(r.g)==1){
#     # simple single component goal
#     
#     # extract relavant resilience layers to goal
#     lyrs = na.omit(as.character(r.g[!names(r.g) %in% c('goal','component','component_name')]))
#y     
#     # r: resilience value matrix [region_id x layer: value]
#     r = acast(subset(SelectLayersData(layers, layers=lyrs), id_num %in% regions), id_num ~ layer, value.var='val_num')
#     names(dimnames(r)) <- c('region_id', 'layer')
#     
#     # b: boolean value matrix [region_id x layer]
#     b <- ifelse(!is.na(r),T,F); head(b)
#     
#     # w: weighting matrix [region_id x layer]
#     w <- CalculateResilienceMatrix(b, weights[dimnames(b)[[2]]]); head(w)
#     
#     # R: resilience score [region_id]
#     R = CalculateResilienceScore(r, types[dimnames(b)[[2]]], w)
#     
#     # assign to resilience matrix    
#     D = merge(D, setNames(data.frame(as.integer(names(R)), R), c('region_id',g)))
#   } 
# 
# else {
#     stopifnot(g %in% names(rc))
    stopifnot(rc[[g]][['layer']] %in% layers$meta$layer)
    
    lyr_agg = rename(subset(SelectLayersData(layers, layers=rc[[g]][['layer']], narrow=T),  id_num %in% regions),
                     c('id_num'='region_id','val_num'='value'))
    #stopifnot(subset(layers$meta, layer %in% rc[[g]][['layer']], val_0to1, drop=T))
    
    # check that all components are in lyr_agg
    cond.1 = sub('(.*)( only|, with |, without )(.*)', '\\1', r.g$component)
    cond.2 = sub('(.*)( only|, with |, without )(.*)', '\\3', r.g$component)
    component_categories = unique(na.omit(c(ifelse(nchar(cond.1)>0, cond.1, NA),
                                            ifelse(nchar(cond.2)>0, cond.2, NA))))      
    
if (!all(component_categories %in% unique(lyr_agg$category))){
      cat(sprintf('Based on the following components for %s:\n  %s', g, paste(r.g$component, collapse='\n  ')))
      #         stop(sprintf('The following component categories for %s are not in the aggregation layer %s categories (%s): %s', g, rc[[g]][['layer']], 
      #                      paste(unique(lyr_agg$category), collapse=', '),
      #                      paste(component_categories[!component_categories %in% lyr_agg$category], collapse=', ')))
    }
  #}      
  
  if (nrow(r.g) > 1 && rc[[g]][['level']]=='region_id'){
    # multiple components within goal, selecting component row for layers to use, and calculating Resilience per region_id            
    
    # check for valid component conditions
    cond = with(r.g, 
                data.frame(
                  component    = component,
                  cond.default = ifelse(component=='', TRUE, NA),
                  cond.only    = grepl('(.*) only',          component),
                  cond.with    = grepl('(.*), with (.*)',    component),
                  cond.without = grepl('(.*), without (.*)', component), stringsAsFactors=F))
    class(cond$component)
    
    # ensure only one TRUE per condition
    stopifnot(all.equal(apply(cond[,-1], 1, sum, na.rm=T), rep(1,length(r.g$component))))
    # break down condition into individual components needed for later evaluation
    cond = cbind(cond,
                 cond.only.1     = ifelse(cond$cond.only==TRUE, gsub("(.*) only",             "\\1", r.g$component), NA),
                 cond.with.1     = ifelse(cond$cond.with==TRUE, gsub("(.*), with (.*)",       "\\1", r.g$component), NA),
                 cond.with.2     = ifelse(cond$cond.with==TRUE, gsub("(.*), with (.*)",       "\\2", r.g$component), NA),
                 cond.without.1  = ifelse(cond$cond.without==TRUE, gsub("(.*), without (.*)", "\\1", r.g$component), NA),
                 cond.without.2  = ifelse(cond$cond.without==TRUE, gsub("(.*), without (.*)", "\\2", r.g$component), NA))      
    
    # iterate regions
    for (id in D$region_id){ # 
      id=35
      # get components in given region
      components = subset(lyr_agg, region_id==id, category, drop=T)
      
      if (length(components)==0) next
      # ?: CS default '' needs components or ok if 0 when having default?
      
      # get condition for region "to see what condition my condition was in"
      cond.components = cond[,c('cond.default','cond.only.1','cond.with.1','cond.with.2','cond.without.1','cond.without.2')]
      components.in.cond = as.data.frame(apply(cond.components, c(1,2), function(x) x %in% components), row.names=cond[['component']])
      
      # TODO: for HAB, seems wrong that for regions to qualify for "* only" component conditions, they can only have that component, even if other like sea_ice_edge included
      components.in.cond[['cond.only.1']] = ifelse(components.in.cond[['cond.only.1']]==T & length(components)==1, T, F) #converts everything to FALSE, unless there is only one component     
      components.in.cond[['cond.without.2']] = !components.in.cond[['cond.without.2']] # invert without predicate
      components.in.cond[is.na(cond.components)] = NA
      # assign condition to default if default ('') row exists and no other condition found to be True
      if ('' %in% rownames(components.in.cond)){
        if(!any(apply(components.in.cond[''!=rownames(components.in.cond),], 1, function(x) all(x==T,na.rm=T)))){
          components.in.cond['','cond.default'] = TRUE
        }        
      }
      
      # get condition based on which is true
      condition = rownames(components.in.cond)[apply(components.in.cond, 1, function(x) all(x==T,na.rm=T))]
      #if (identical(condition, character(0))) condition = NA
      if (identical(condition, character(0))){
        if (debug) cat(sprintf('  skipping region %s for %s since no matching conditions, but having components: %s\n', id, g, paste(components, collapse=', ')))
        next # Wierd: with layers.Global2013.www2013, g=HAB, id=35, get condition=NA. and for HAB then lyrs bonks       
      } 
      
      lyrs <- na.omit(as.character(subset(rm, goal==g & component==condition)[,c(-1,-2)]))
      
      # r: resilience value matrix [region_id x layer: value]
      r = acast(subset(SelectLayersData(layers, layers=lyrs), id_num == id), id_num ~ layer, value.var='val_num')
      names(dimnames(r)) <- c('region_id', 'layer')
      
      if (nrow(r)==0) next # eg for g=CP, id=162 (Antarctica), condition='sea_ice_shoreline only'
      
      # b: boolean value matrix [region_id x layer]
      b <- ifelse(!is.na(r),T,F)
      
      # w: weighting matrix [region_id x layer]
      w <- CalculateResilienceMatrix(b, weights[dimnames(b)[[2]]])
      
      # R: resilience score [region_id]
      R = CalculateResilienceScore(r, types[dimnames(b)[[2]]], w)
      
#          ###################### CalculateResilienceScore ##############
#           t= types[dimnames(b)[[2]]]
#           gamma=0.5
#           resilience_categories=c('environmental', 'regulatory', 'social')
#       stopifnot(all(is.matrix(r), is.vector(t)))
#       stopifnot(all(t %in% resilience_categories))
#       stopifnot(all(names(dimnames(r)) == c('region_id', 'layer')))    
#       stopifnot(all(dimnames(r)$layer %in% names(t)))
#       if (missing(w)) {
#         w <- rep(1, dim(r)[[2]])
#         names(w) <- dimnames(r)$layer
#         w <- ohi.model.resilience.matrix(!is.na(r), w)
#       } else {
#         stopifnot(is.matrix(w))
#         stopifnot(all(names(dimnames(w)) == c('region_id', 'layer')))
#       }
#       if (getOption('debug', FALSE)) {
#         stopifnot(min(r, na.rm=T) >= 0  && max(r, na.rm=T) <= 1)   #  [0, 1]
#         stopifnot(min(w, na.rm=T) >= 0  && max(w, na.rm=T) <= 2)   #  [0, 2]
#       }
#       stopifnot(all(dimnames(w)$layer %in% dimnames(r)$layer))
#       
#       # align
#       t <- t[dimnames(r)$layer]
#       w <- w[dimnames(r)$region_id, dimnames(r)$layer, drop=F]
#       stopifnot(all(dimnames(r)$layer == dimnames(w)$layer))
#       stopifnot(all(dimnames(r)$layer == names(t)))
#       
#       # compute by category
#       for (k in resilience_categories) {
#         #k="environmental"
#         l <- paste('r', k, sep='_')
#         if (k %in% t) {
#           l.r <- r[,names(t)[t == k], drop=F]
#           l.mask <- ifelse(!is.na(l.r), 1, NA)
#           l.w <- w[,dimnames(l.r)$layer, drop=F]
#           l.score <- apply(l.r*l.w, 1, sum, na.rm=T) / apply(l.mask*l.w, 1, sum, na.rm=T)
#           assign(l, l.score)
#         } else {
#           assign(l, rep(NA, nrow(r)))
#         }
#       }
#       
#       # compute
#       scores.e <- apply(cbind(get('r_environmental'), get('r_regulatory')), 1, mean, na.rm=T)
#       scores.s <- get('r_social')
#       scores <- apply(cbind(scores.e, scores.s), 1, weighted.mean, w=c(gamma,1-gamma), na.rm=T)
#       names(scores) <- dimnames(r)$region_id
#       return(round(scores*100, 2))
#     }
#  
#          ######################
      
            
      # assign to resilience matrix
      if (!g %in% names(D)) D[[g]] = NA
      D[D$region_id==id,g] = R         
    }    
  }
  
  if (nrow(r.g) > 1 && rc[[g]][['level']]=='region_id-category'){
    # multiple components within goal, calculating Resilience per category, and averaging up to region_id (for NP only)
    
    # iterate regions
    for (id in D$region_id){ # id=1
      
      # get components in given region
      components = subset(lyr_agg, region_id==id, category, drop=T)      
      if (length(components)==0) next
      
      # iterate components
      R.id.k = numeric()
      for (k in components){ # k=components[1]
        
        # extract relavant resilience layers to goal
        lyrs <- na.omit(as.character(subset(rm, goal==g & component==k)[,c(-1,-2)]))
        
        # r: single region resilience value matrix [region_id x layer: value]
        r = acast(subset(SelectLayersData(layers, layers=lyrs), id_num==id), id_num ~ layer, value.var='val_num')
        names(dimnames(r)) <- c('region_id', 'layer')
        
        if (nrow(r)==0) next # eg for g=CP, id=162 (Antarctica), condition='sea_ice_shoreline only'
        
        # b: boolean value matrix [region_id x layer]
        b <- ifelse(!is.na(r),T,F)
        
        # w: weighting matrix [region_id x layer]
        w <- CalculateResilienceMatrix(b, weights[dimnames(b)[[2]]])
        
        # R: resilience score [region_id]
        R = CalculateResilienceScore(r, types[dimnames(b)[[2]]], w)
        R.id.k = c(R.id.k, setNames(R, k))                  
      }  
      
      # assign to resilience matrix
      if (!g %in% names(D)) D[[g]] = NA
      D[D$region_id==id,g] = round(weighted.mean(R.id.k, subset(lyr_agg, region_id==id, value, drop=T)), 2)    
    }      
  }
} # end for g in...


