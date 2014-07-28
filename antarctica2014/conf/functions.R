Setup = function(){
  # empty for now
}

FIS = function(layers){
  # scores
  return(cbind(rename(SelectLayersData(layers, layers=c('fis_status'='status','fis_trend'='trend'), narrow=T),
                      c(id_num='region_id', layer='dimension', val_num='score')), 
               data.frame('goal'='FIS')))
}


FP = function(scores){
  
  # scores
s = scores %.%
    filter(goal %in% c('FIS') & dimension %in% c('status','trend','future','score')) %.%
    # NOTE: resilience and pressure skipped for supra-goals
    mutate(goal = 'FP')
  
  # return all scores
  return(rbind(scores, s))
}


NP = function(layers){
  # scores
  return(cbind(rename(SelectLayersData(layers, layers=c('np_status'='status','np_trend'='trend'), narrow=T),
                      c(id_num='region_id', layer='dimension', val_num='score')), 
               data.frame('goal'='NP')))
}



CP = function(layers){
  
  # layers
  lyrs = list('rk' = c('hab_health' = 'health',
                       'hab_extent' = 'extent',
                       'hab_trend'  = 'trend'))
  lyr_names = sub('^\\w*\\.','', names(unlist(lyrs)))
  
  # get layer data
  D = SelectLayersData(layers, layers=lyr_names)
    # cast
  rk = rename(dcast(D, id_num + category ~ layer, value.var='val_num', subset = .(layer %in% names(lyrs[['rk']]))),
              c(id_num='region_id', 'category'='habitat', lyrs[['rk']]))
  
  # limit to CP habitats and add rank
  habitat.rank = c('coral'            = 4,
                   'mangrove'         = 4,
                   'saltmarsh'        = 3,
                   'seagrass'         = 1,
                   'seaice_shoreline' = 4)
  
  rk = subset(rk, habitat %in% names(habitat.rank))
  rk$rank = habitat.rank[as.character(rk$habitat)]
  
  # assign extent of 0 as NA
  rk$extent[rk$extent==0] = NA
  
  # status  
  r.status = ddply(na.omit(rk[,c('region_id','habitat','rank','extent','health')]), .(region_id), summarize,
                   goal = 'CP',
                   dimension = 'status',
                   score = min(1, sum(rank * health * extent) / (sum(extent) * max(rank)) ) * 100 )    
  
  # trend
  r.trend = ddply(na.omit(rk[,c('region_id','habitat','rank','extent','trend')]), .(region_id), summarize,
                  goal = 'CP',
                  dimension = 'trend',
                  score = sum(rank * trend * extent) / (sum(extent)* max(rank)) * 5)
  
  # return scores
  return(rbind(r.status, r.trend))  
}


TR = function(layers){
  
  # scores
  return(cbind(rename(SelectLayersData(layers, layers=c('tr_status'='status','tr_trend'='trend'), narrow=T),
                      c(id_num='region_id', layer='dimension', val_num='score')), 
               data.frame('goal'='TR')))  
}



ECO = function(layers, status_year){
  #status_year=2013
  trend_years <-  (status_year-3):status_year
  D <- SelectLayersData(layers, layers=c('eco'))
  D <- D %.%
    select(sp_id = id_num, category, year, crew=val_num)
  
  ## change this year to a zero (based on catch data, this seems unlikely)
  D$crew[(D$sp_id=="248500" & D$category=="cf" & D$year=="2013")] <- 0
  
  # calculate status (current year divided by current year minus 4 years)
  D$status <- NA
  
  for(i in 1:dim(D)[1]){
    #i <- 8 #testing
    sp_id_tmp <- D[i, 1]
    year_curr <- D[i, 3]
    category <- D[i,2]
    year_ref <- year_curr-4
    
    
    D$status[i] <- ifelse(identical(D$crew[i]/D$crew[D$sp_id == sp_id_tmp & D$year==year_ref & D$category==category], numeric(0)), 
                                         NA,
                                         D$crew[i]/D$crew[D$sp_id == sp_id_tmp & D$year==year_ref& D$category==category])  
  }
  
  D$status <- ifelse(D$status %in% "NaN", NA,  D$status) # these are zero divided by zero
  D$status <- ifelse(D$status %in% "Inf", 1,  D$status) # these are value divided by zero (should this be NA?)
  D$status <- ifelse(D$status > 1, 1,  D$status)
  
  
  ## weights are the average crew between 2010-2013
  weights <- D %.%
    filter(year %in% 2010:2013) %.%
    group_by(sp_id, category) %.%
    summarize(meanCrew=mean(crew, na.rm=TRUE))
  
  ## merge with other data
  D <- merge(D, weights, all.x=TRUE, by=c("sp_id", "category"))
  
  status_melt <- melt(D, id=c("sp_id", "category", "year"))
  status <- dcast(status_melt, sp_id + year ~ category + variable, mean)
  
  status <- status[status$year %in% trend_years, ]

  status$cf_meanCrew[status$cf_meanCrew %in% "NaN"] <- 0
  status$tour_meanCrew[status$tour_meanCrew %in% "NaN"] <- 0
  
  
  status <- status %.%
    mutate(f_weight = cf_meanCrew/(cf_meanCrew + tour_meanCrew),
           tr_weight = 1-f_weight) %.%
    mutate(status = ifelse(is.na(cf_status*f_weight), 0, cf_status*f_weight) + ifelse(is.na(tour_status*tr_weight), 0, tour_status*tr_weight)) %.%
    mutate(status = status*100)
  
  ##### Status & trend
  status.scores <- status %.%
  filter(year==status_year) %.%
    mutate(goal="ECO", 
           dimension="status") %.%
    select(region_id=sp_id, goal, dimension, score=status)
  
  trend.data <- status %.%
    filter(year %in% trend_years)
  
    lm = dlply(
    trend.data, .(sp_id),
    function(x) lm(I(status/100) ~ year, x))
  
  trend_lm <- ldply(lm, coef)
  
  trend.scores <- trend_lm %.%
   mutate(goal="ECO",
          dimension="trend") %.%
    mutate(score=year*5) %.%
    select(region_id=sp_id, goal, dimension, score) %.%
    mutate(score=ifelse(score>1, 1, score)) %.%
    mutate(score=ifelse(score<(-1), -1, score))
  
  #testing:
  #lm(I(status/100) ~ year, data=subset(trend.data, sp_id == "248500")) ## only one value....
  #lm(I(status/100) ~ year, data=subset(trend.data, sp_id == "248100")) ## only one value....
  
  # return scores
  return(rbind(trend.scores, status.scores))  
}


LE = function(scores){
  
  # scores
  s = scores %.%
    filter(goal %in% c('ECO') & dimension %in% c('status','trend','future','score')) %.%
    # NOTE: resilience and pressure skipped for supra-goals
    mutate(goal = 'LE')
  
  # return all scores
  return(rbind(scores, s))
}


ICO = function(layers){
  
  # scores
  scores = cbind(rename(SelectLayersData(layers, layers=c('ico_status'='status','ico_trend'='trend'), narrow=T),
                         c(id_num='region_id', layer='dimension', val_num='score')), 
                 data.frame('goal'='ICO'))
  return(scores) 
}


LSP = function(layers){
  
  # scores
  scores = cbind(rename(SelectLayersData(layers, layers=c('lsp_prot_area_status'='status','lsp_prot_area_trend'='trend'), narrow=T),
                        c(id_num='region_id', layer='dimension', val_num='score')), 
                 data.frame('goal'='LSP'))
  return(scores) 
}

SP = function(scores){
  
  d = within(
    dcast(
      scores, 
      region_id + dimension ~ goal, value.var='score', 
      subset=.(goal %in% c('ICO','LSP') & !dimension %in% c('pressures','resilience')))
    , {
      goal = 'SP'
      score = rowMeans(cbind(ICO, LSP), na.rm=T)})
  
  
  # return all scores
  return(rbind(scores, d[,c('region_id','goal','dimension','score')]))
}


CW = function(layers){
  # layers
  lyrs = c('po_chemicals' = 'l',
           'po_trash'     = 'd',
           'cw_chemical_trend'   = 'chem_trend',
           'cw_trash_trend'  = 'trash_trend')
    
  # cast data
  d = SelectLayersData(layers, layers=names(lyrs))  
  r = rename(dcast(d, id_num ~ layer, value.var='val_num', subset = .(layer %in% names(lyrs))),
              c(id_num='region_id', lyrs)); head(r); summary(r)
  
  # invert pressures
  r$l = 1 - r$l
  r$d = 1 - r$d
  
  # status
  r$status = psych::geometric.mean(t(r[,c('l','d')]), na.rm=T) * 100
  
  # trend
  r$trend = rowMeans(r[,c('chem_trend','trash_trend')], na.rm=T)
  
  # return scores
  scores = rbind(
    within(r, {
      goal      = 'CW'
      dimension = 'status'
      score     = status}),
    within(r, {
      goal      = 'CW'
      dimension = 'trend'
      score     = trend}))[,c('region_id','goal','dimension','score')]
  return(scores)  
}


HAB = function(layers){
  
  # layers
  lyrs = c('hab_health' = 'health',
           'hab_extent' = 'extent',
           'hab_trend'  = 'trend')
  
#  browser()
  
  # cast data
  d = SelectLayersData(layers, layers=names(lyrs))  
  rk = rename(dcast(d, id_num + category ~ layer, value.var='val_num', subset = .(layer %in% names(lyrs))),
              c(id_num='region_id', 'category'='habitat', lyrs))
  
  # limit to HAB habitats
  rk = subset(rk, habitat %in% c('coral','mangrove','saltmarsh','seaice_extent','seagrass','soft_bottom'))  
  
  # presence as weight
  rk$w = ifelse(!is.na(rk$extent) & rk$extent>0, 1, NA)
  
  # status
  r.status = ddply(na.omit(rk[,c('region_id','habitat','w','health')]), .(region_id), summarize,
                   goal      = 'HAB',
                   dimension = 'status',
                   score     = min(1, sum(w * health) / sum(w)) * 100); summary(r.status)
  
  # trend
  r.trend = ddply(na.omit(rk[,c('region_id','habitat','w','trend')]), .(region_id), summarize,
                  goal      = 'HAB',
                  dimension = 'trend',
                  score     = sum(w * trend) / sum(w) * 5)
  ### should these be multiplied by 5?
  # return scores
  scores = cbind(rbind(r.status, r.trend))
  return(scores)  
}


SPP = function(layers){

  # scores
  scores = cbind(rename(SelectLayersData(layers, layers=c('spp_status'='status','spp_trend'='trend'), narrow=T),
                      c(id_num='region_id', layer='dimension', val_num='score')), 
               data.frame('goal'='SPP'))
  return(scores) 
}

BD = function(scores){
  
  d = within(
    dcast(
      scores, 
      region_id + dimension ~ goal, value.var='score', 
      subset=.(goal %in% c('HAB','SPP') & !dimension %in% c('pressures','resilience'))), 
    {
      goal = 'BD'
      score = rowMeans(cbind(HAB, SPP), na.rm=T)})
  
  # return all scores
  return(rbind(scores, d[,c('region_id','goal','dimension','score')]))
}

PreGlobalScores = function(layers, conf, scores){
    
  # get regions
  rgns = SelectLayersData(layers, layers=conf$config$layer_region_labels, narrow=T)
  
  # limit to just desired regions and global (region_id==0)
  scores = subset(scores, region_id %in% c(rgns$id_num, 0))
      
  return(scores)
}

FinalizeScores = function(layers, conf, scores){
  
  #  browser()
  
  # get area data
  area = SelectLayersData(layers, layers='rgn_area', narrow=TRUE)  
  area <- area %.%
    select(region_id=id_num, area=val_num)
  # get regions
  rgns = SelectLayersData(layers, layers=conf$config$layer_region_labels, narrow=T)
  
  
tmp <-   scores %.%
    join(area, by='region_id') %.% 
  filter(region_id != 0) %.%
  group_by(goal, dimension) %.%
  summarize(region_id0 = weighted.mean(score, area, na.rm=TRUE))
  
  
  # add NAs to missing combos (region_id, goal, dimension)
  d = expand.grid(list(score_NA  = NA,
                       region_id = c(rgns[,'id_num'], 0),
                       dimension = c('pressures','resilience','status','trend','future','score'), 
                       goal      = c(conf$goals$goal, 'Index')), stringsAsFactors=F); head(d)
  
  d = subset(d, 
             !(dimension %in% c('pressures','resilience','trend') & region_id==0)) & 
             !(dimension %in% c('pressures','resilience','status','trend') & goal=='Index'))
  scores = merge(scores, d, all=T)[,c('goal','dimension','region_id','score')]
      
  # order
  scores = arrange(scores, goal, dimension, region_id)
  
  # round scores
  scores$score = round(scores$score, 2)
    
  return(scores)
}
