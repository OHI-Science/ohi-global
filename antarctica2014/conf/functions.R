Setup = function(){
  # empty for now
}

FIS = function(layers, status_year){
   trend_years <- (status_year-4):status_year
  
# catch 
  catch = SelectLayersData(layers, layers='fis_catch', narrow=T) %>%
    select(
      sp_id    = id_num,
      species  = category,
      year,
      catch    = val_num)  
  
  
# catch limits 
  limit = SelectLayersData(layers, layer='fis_reference', narrow=T) %>%
    select(
      sp_id      = id_chr,
      species    = category,
      year,
      limit      = val_num)
  
  
  # ------------------------------------------------------------------------
  # STEP 1. Calculate proportion of catch relative to reference
  # Do each species group separately due to grouped reporting of limits
  # -----------------------------------------------------------------------
  
   # Krill: limits are the combined catch from areas 481, 482, 483, 484
   # NOTE: Krill isn't used to calculate FIS, but it is used later to calculate NP
    krill_catch <- catch %>%
    filter(species == "KRI") %>%
    mutate(sp_id_krill = ifelse(sp_id %in% c(248100, 248200, 248300, 248400), 
                                "248100-248400", sp_id)) %>%
    group_by(sp_id_krill, year) %>%
    mutate(catch_krill = sum(catch, na.rm=TRUE)) %>%
    ungroup() %>%
    filter(year>2007 & catch > 0)
         
   krill_limit <- limit %>%
     filter(species == "KRI") %>%
     select(sp_id_krill = sp_id, year, limit)%>%
     mutate(sp_id_krill = as.character(sp_id_krill))
  
   krill <- krill_limit %>%
     left_join(krill_catch, by=c("sp_id_krill", "year")) %>%
     mutate(c_cmsy = catch_krill/limit) %>%
     select(species, sp_id, year, c_cmsy)
   
   # Toothfish: limits are based on combined TOA and TOP (except for one region)
    tf_catch_combined <- catch %>%
     filter(species %in% c("TOA", "TOP")) %>%
     group_by(sp_id, year) %>%
     summarize(catch_tf = sum(catch, na.rm=TRUE)) %>%
     ungroup() %>%
     mutate(species = "TOP-TOA") %>%
     mutate(sp_id = as.character(sp_id)) 
   
    tf_catch_species <- catch %>%
      filter(species %in% c("TOA", "TOP")) %>%
      group_by(sp_id, year) %>%
      mutate(catch_tf = sum(catch, na.rm=TRUE)) %>%
      ungroup() %>%
      mutate(sp_id = as.character(sp_id)) 
    
    tf_catch <- bind_rows(tf_catch_combined, tf_catch_species)
    
    tf_limit <- limit %>%
     filter(species %in% c("TOP-TOA", "TOP", "TOA")) %>%
     select(sp_id, species, year, limit)%>%
     mutate(sp_id = as.character(sp_id),
            species = as.character(species))
   
   tf <- tf_limit %>%
     left_join(tf_catch, by=c("sp_id", "year", "species")) %>%
     mutate(c_cmsy = catch_tf/limit) %>%
     select(species, sp_id, year, c_cmsy)
   
   cmsy <- rbind(krill, tf)
  
   # ------------------------------------------------------------------------
   # STEP 2. Calculate score based on Ccmsy
   # -----------------------------------------------------------------------
    
  eps <- .25 
  score_range  <- 1-0.25
  value_range <- 0.90-0
  
  scores <- cmsy %>%
    mutate(score = ifelse(c_cmsy > 1.0 , 2.0-c_cmsy, 
                               ifelse(c_cmsy < 0.9, eps + score_range/value_range * c_cmsy, 1))) %>%
    mutate(score = ifelse(score <= 0, 0.1, score))
  
  ## plot a figure to show how c/cmsy is converted to a score
  png('temp/c_cmsyVSscore.png')
  tmp <- data.frame(c_cmsy = seq(0,3, by=0.01)) %>%
    mutate(score = ifelse(c_cmsy > 1.0, 2.0-c_cmsy, 
                          ifelse(c_cmsy < 0.9, eps + score_range/value_range * c_cmsy, 1)))%>%
    mutate(score = ifelse(score < 0, 0, score))
  plot(score ~ c_cmsy, data=tmp, xlab='c/cmsy', ylab="score", type="l")
  dev.off()
  
  ## save data for Natural Products (calculated using Krill)
  np <- scores %>%
    filter(species=="KRI") %>%
    select(sp_id, species, year, score)
  write.csv(np, 'layers/np_krill.csv', row.names=FALSE)

  
  # add in scores
  status_data <- scores %>%
    filter(species != "KRI") %>%
    select(species, sp_id, year, Status=score)
  

  # ------------------------------------------------------------------------
  # STEP 4. Calculate status & trend
  # -----------------------------------------------------------------------
  
  status = status_data %>%
    filter(year==status_year) %>%
    filter(!is.na(Status)) %>%
    mutate(
      score     = round(Status*100),
      dimension = 'status') %>%
    select(region_id=sp_id, dimension, score)
  
  trend = status_data %>%
    filter(year %in% trend_years) %>%
    filter(!is.na(Status)) %>%
    group_by(sp_id) %>%
    do(mdl = lm(Status ~ year, data=.)) %>%
    summarize(region_id = sp_id,
              score = coef(mdl)['year'] * 5) %>%
    ungroup()
  
  trend <- trend %>%
    mutate(score = round(score, 2)) %>%
    mutate(dimension = 'trend') %>%
    mutate(score = ifelse(score < (-1), -1, score)) %>%
    mutate(score = ifelse(score > 1, 1, score)) %>%
    select(region_id, dimension, score) %>%
    ungroup()
  
  # assemble dimensions
  scores = rbind(status, trend) %>% 
    mutate(goal='FIS') %>%
    data.frame()
  return(scores)  
  
}


FP = function(scores){
  
  # scores
s = scores %>%
    filter(goal %in% c('FIS') & dimension %in% c('status','trend','future','score')) %>%
    # NOTE: resilience and pressure skipped for supra-goals
    mutate(goal = 'FP')
scores <- rbind(scores, s)  
  # return all scores
  return(scores)
}


NP = function(layers, status_year){

  ##########################################################
  ####  NOTE: NP score data is calculated in FIS for KRI
  ##########################################################
  trend_years <- (status_year-4):status_year

  status_data <- read.csv("layers/np_krill.csv")
  
  status = status_data %>%
    filter(year==status_year) %>%
    filter(!is.na(score)) %>%
    mutate(
      score     = round(score*100),
      dimension = 'status') %>%
    select(region_id=sp_id, dimension, score)
  
  trend = status_data %>%
    filter(year %in% trend_years) %>%
    filter(!is.na(score)) %>%
    group_by(sp_id) %>%
    do(mdl = lm(score ~ year, data=.)) %>%
    summarize(region_id = sp_id,
              score = coef(mdl)['year'] * 5) %>%
    ungroup()
  
  trend <- trend %>%
    mutate(score = round(score, 2)) %>%
    mutate(dimension = 'trend') %>%
    mutate(score = ifelse(score < (-1), -1, score)) %>%
    mutate(score = ifelse(score > 1, 1, score)) %>%
    select(region_id, dimension, score) %>%
    ungroup()
  
  # assemble dimensions
  scores = rbind(status, trend) %>% 
    mutate(goal='NP') %>%
    data.frame()
  return(scores)  
}




TR = function(layers, status_year){

  trend_years <- (status_year - 4):status_year
  buffer <- 0.35  ## when tourist days are >= max*(1-buffer) the score will be 1
  NAcut <- 500   ## sites that have < NAcut tourist days get an NA score
  
# get data file
  tr_data <- layers$data$tr_days %>%
    select(sp_id, year, days)
  
# calculate relative tourist days:
  tr_data <- tr_data %>%
    group_by(sp_id) %>%
    mutate(maxDays = max(days, na.rm=TRUE)) %>%
    mutate(rel_days = days/max(days, na.rm=TRUE)) %>%
    mutate(rel_days = rel_days/(1-buffer)) %>%
    mutate(rel_days = ifelse(rel_days > 1, 1, rel_days)) %>%
    arrange(sp_id, year) %>%
    ungroup()


## save this as intermediate reference:
  write.csv(tr_data, 'temp/tr_rel_days.csv', row.names=FALSE)
  
  tr_data <- tr_data %>%
    filter(maxDays > NAcut)
  
# calculate status:
  status = tr_data %>%
    filter(year==status_year) %>%
    mutate(
      score     = round(rel_days*100, 2),
      dimension = 'status') %>%
    select(region_id = sp_id, dimension, score)

# calculate trend: 
  trend <- tr_data %>%
    filter(year %in% trend_years) %>%
    group_by(sp_id) %>%
    do(mdl = lm(rel_days ~ year, data = .)) %>%
    summarize(region_id = sp_id,
              score = coef(mdl)['year'] * 5) %>%
    ungroup() %>%
    mutate(score = ifelse(score<(-1), -1, score)) %>%
    mutate(score = ifelse(score>(1), 1, score)) %>%
    mutate(score = round(score, 4)) %>%
    mutate(dimension = "trend") %>%
    select(region_id, dimension, score)
  
  # assemble dimensions
  scores = rbind(status, trend) %>% 
    mutate(goal='TR') %>%
    data.frame()
  
  return(scores)  
}



ECO = function(layers, status_year){
# D <- read.csv('layers/eco.csv') %>%
# select(sp_id, category=sector, year, crew)  
# status_year <- 2015
  
  trend_years <-  (status_year-4):status_year
  meanCrew_cut <- 10      # cut if there were less than 10 crew averaged over the past 5 years
  yearsData_cut <- 3   # cut if there are fewer than 3 years of non-zero crew data over the past 5 years

  D <- SelectLayersData(layers, layers=c('eco'))
  D <- D %>%
    select(sp_id = id_num, category, year, crew=val_num)

  # filter the data, so sites with low/variable crews are not included  
  D <- D %>%
    group_by(sp_id, category) %>%
    mutate(meanCrew = mean(crew[year %in% trend_years])) %>%
    mutate(yearsData = sum(crew[year %in% trend_years] > 0)) %>%
    mutate(crew = ifelse(meanCrew < meanCrew_cut | yearsData < yearsData_cut,
                          0, crew)) %>%
    ungroup() %>%
    data.frame()
  
    
  # calculate status of each category  
  status_cat <- D %>%
    group_by(sp_id, category) %>%
    arrange(year) %>%
    mutate(crew_5 = lag(crew, 4)) %>%
    mutate(status_cat = crew / crew_5) %>%
    select(sp_id, category, year, crew, status_cat) %>%
    ungroup() %>%
    data.frame()
    
  status_cat$status_cat <- ifelse(status_cat$status_cat %in% "NaN", NA,  status_cat$status_cat) # these are zero divided by zero
  status_cat$status_cat <- ifelse(status_cat$status_cat %in% "Inf", 1,  status_cat$status_cat) # these are value divided by zero
  status_cat$status_cat <- ifelse(status_cat$status_cat > 1, 1,  status_cat$status_cat)
  
  
  ## weights are the average crew in status years to weight contribution of tour vs. fis crews
  status_cat_wt <- status_cat %>%
    filter(year %in% trend_years) %>%
    group_by(sp_id, category) %>%
    mutate(meanCrew=mean(crew, na.rm=TRUE)) %>%
    ungroup() %>%
    data.frame()
  
  ## calculate status by taking a weighted mean of category status values
  status <- status_cat_wt %>%
    group_by(sp_id, year) %>%
    summarize(status = weighted.mean(status_cat, meanCrew, na.rm=TRUE)) %>%
    ungroup() %>%
    filter(!is.na(status)) %>%
    data.frame()
  

  ##### Status & trend
  status.scores <- status %>%
  filter(year==status_year) %>%
  mutate(status = status * 100) %>%
    mutate(goal="ECO", 
           dimension="status") %>%
    select(region_id=sp_id, goal, dimension, score=status)
  
  trend.scores <- status %>%
    group_by(sp_id) %>%
    do(mdl = lm(status ~ year, data = .)) %>%
    summarize(sp_id = sp_id, 
              score = coef(mdl)['year'] * 5) %>%
    ungroup() %>%
   mutate(goal="ECO",
          dimension="trend") %>%
    select(region_id=sp_id, goal, dimension, score) %>%
    mutate(score=ifelse(score>1, 1, score)) %>%
    mutate(score=ifelse(score<(-1), -1, score)) %>%
    data.frame()
    
  # return scores
  return(rbind(trend.scores, status.scores))  
}


LE = function(scores){
  
  # scores
  s = scores %>%
    filter(goal %in% c('ECO') & dimension %in% c('status','trend','future','score')) %>%
    # NOTE: resilience and pressure skipped for supra-goals
    mutate(goal = 'LE')
  
  # return all scores
  return(rbind(scores, s))
}


ICO = function(layers){
  
  layers_data <-  SelectLayersData(layers, layers=c('ico_status', 'ico_trend'))
  
  rk <- layers_data %>%
    select(region_id = id_num, sciname = category, iucn_cat=val_chr, layer) %>%
    mutate(iucn_cat = as.character(iucn_cat))
  
  # lookup for weights status
  w.risk_category = data.frame(iucn_cat = c('LC', 'NT', 'VU', 'EN', 'CR', 'EX'),
                               risk_score = c(1,  0.8,   0.6,  0.4,  0.2,  0)) %>%
    mutate(iucn_cat = as.character(iucn_cat))
  
  # lookup for population trend
  w.popn_trend = data.frame(iucn_cat = as.character(c('Decreasing', 'Stable', 'Increasing')),
                            trend_score = c(-0.5, 0, 0.5)) %>%
    mutate(iucn_cat = as.character(iucn_cat))
  
  ####### status
  # STEP 1: take mean of subpopulation scores
  r.status_spp <- rk %>%
    filter(layer == 'ico_status') %>%
    left_join(w.risk_category, by = 'iucn_cat') %>%
    group_by(region_id, sciname) %>%
    summarize(spp_mean = mean(risk_score, na.rm=TRUE) * 100) %>%
    ungroup()
  
  # STEP 2: take mean of populations within regions
  r.status <- r.status_spp %>%
    group_by(region_id) %>%
    summarize(score = mean(spp_mean, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(dimension = "status")
  
  ####### trend
  # STEP 1: take mean of subpopulation scores
  r.trend_spp <- rk %>%
    filter(layer == 'ico_trend') %>%
    left_join(w.popn_trend ,by = 'iucn_cat') %>%
    group_by(region_id, sciname) %>%
    summarize(spp_mean = mean(trend_score, na.rm=TRUE)) %>%
    ungroup()
  
  # STEP 2: take mean of populations within regions
  r.trend <- r.trend_spp %>%
    group_by(region_id) %>%
    summarize(score = mean(spp_mean, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(dimension = "trend")
  
  # return scores
  scores <-  rbind(r.status, r.trend) %>%
    mutate('goal'='ICO') %>%
    select(goal, dimension, region_id, score) %>%
    data.frame()
  
  return(scores)  

  }


LSP = function(layers, status_year){
  
  ref_pct <- 0.30  # reference point (30% of region as protected)
  
  trend_years <-  (status_year-4):status_year 
  
  area_inland <-  SelectLayersData(layers, layers = c('rgn_area_inland')) %>%
    mutate(type = "pa") %>%
    select(type, sp_id = id_num, area_km2 = val_num)
  area_offshore <- SelectLayersData(layers, layers = c('rgn_area')) %>%
    mutate(type = "cmpa") %>%
    select(type, sp_id = id_num, area_km2 = val_num)
  mpa <- SelectLayersData(layers, layers = c("lsp_mpa")) %>%
    select(sp_id = id_num, year, area_mpa_km2 = val_num)


  ## add mpas in same region/year
    mpa  <- mpa %>%
    group_by(sp_id, year) %>%
    summarize(area_mpa_km2 = sum(area_mpa_km2)) %>%
      ungroup()
    
  ## cumulative sum of MPAs in each region  
    mpa_cum <- expand.grid(sp_id = unique(area_offshore$sp_id), year = 1974:status_year) %>%
      left_join(mpa, by=c("sp_id", "year")) %>%
      arrange(sp_id, year) %>%
      mutate(area_mpa_km2 = ifelse(is.na(area_mpa_km2), 0, area_mpa_km2)) %>%
      group_by(sp_id) %>%
      mutate(cum_sum = cumsum(area_mpa_km2)) %>%
      ungroup()
  
  ##  Merge with region area and calculate % area of CCAMLR marine area
    mpa_status <- mpa_cum %>%
      left_join(area_offshore, by="sp_id") %>%
      mutate(status = cum_sum / (area_km2 * ref_pct)) %>%
      mutate(type = "cmpa") %>%
      select(type, sp_id, year, status)
    
  ## save this data as resilience:
  resilience <- mpa_status %>%
    filter(year==status_year) %>%
    select(sp_id, resilience.score=status) %>%
      data.frame()
  write.csv(resilience, "temp/MPAs.csv", row.names=FALSE, quote=FALSE)
  

  ### Land-based: assumed to be status = 100 (all land protected in Antarctica)
  pa_status <- expand.grid(type="pa", sp_id = area_inland$sp_id, year = 1975:max(mpa$year)) %>%
    mutate(type = as.character(type) , 
           sp_id = as.numeric(as.character(sp_id))) %>%
    left_join(area_inland, by=c("type", 'sp_id')) %>%
    mutate(status = ifelse(area_km2==0, NA, 1)) %>%
    select(type, sp_id, year, status)
  
  ## merge inland and offshore data
  
  # first combine inland and offshore area data
  areas <- rbind(area_inland, area_offshore) 
  
  
  # take mean of inland and offshore status values (also an option to use a weighted mean)
  status <- rbind(mpa_status, pa_status) %>%
    left_join(areas, by=c('type', 'sp_id')) %>%
    group_by(sp_id, year) %>%
    summarize(score = mean(status, na.rm=TRUE)) %>%  # average: weighted by area (not using)
#    summarize(score = weighted.mean(status, area_km2, na.rm=TRUE)) %>%  # average: weighted by area (not using)
    ungroup() %>%
    data.frame()
    
  
  ## status
  status_scores <- status %>%
    filter(year==status_year) %>%
    mutate(score=round(score*100, 2)) %>%
    mutate(dimension="status") %>%
    mutate(goal = "LSP") %>%
    select(region_id = sp_id, goal, dimension, score) %>%
    data.frame()
  
  
  #Trend
  trend_scores <- status %>%
    filter(year %in% trend_years) %>%
    group_by(sp_id) %>%
    do(mdl = lm(score ~ year, data = .)) %>%
    summarize(sp_id, 
              score = coef(mdl)['year'] * 5) %>%
    mutate(score = round(score, 4)) %>%
    ungroup() %>%
    mutate(goal="LSP",
           dimension="trend") %>%
    select(region_id = sp_id, goal, dimension, score) %>%
    mutate(score=ifelse(score>1, 1, score)) %>%
    mutate(score=ifelse(score<(-1), -1, score)) %>%
    data.frame()
  
  # return scores
  return(rbind(trend_scores, status_scores))    

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

    lyrs = c('po_chemicals', 
           'po_trash',
           'cw_chemical_trend',
           'cw_trash_trend')
    
  ## At this point, trend assumed to be zero based perfect/near perfect scores
  
  # cast data
  d <-  SelectLayersData(layers, layers=lyrs)  %>%
    select(region_id = id_num, layer, value = val_num)
  
  ### function to calculate geometric mean:  
  geometric.mean2 <- function (x, na.rm = TRUE) {
    if (is.null(nrow(x))) {
      exp(mean(log(x), na.rm = TRUE))
    }
    else {
      exp(apply(log(x), 2, mean, na.rm = na.rm))
    }
  }
  
  d_pressures <- d %>%
    filter(layer %in% grep('po_', lyrs, value=TRUE))  %>%
    mutate(pressure = 1 - value) %>%  # invert pressures
    group_by(region_id) %>%
    summarize(score = geometric.mean2(pressure, na.rm=TRUE)) %>% # take geometric mean
    mutate(score = score * 100) %>%
    mutate(dimension = "status") %>%
    ungroup()
  
  d_trends <- d %>%
    filter(layer %in% grep('_trend', lyrs, value=TRUE)) %>%
    mutate(trend = -1 * value)  %>%  # invert trends
    group_by(region_id) %>%
    summarize(score = mean(trend, na.rm = TRUE)) %>%
    mutate(dimension = "trend") %>%
    ungroup()
  
  scores = rbind(d_pressures, d_trends) %>%
    mutate(goal = "CW") %>%
    select(region_id, goal, dimension, score) %>%
    data.frame()
  
  return(scores)  
}


HAB = function(layers, status_year){
  
  ## extent data to calculate hab_presence
  extent <- SelectLayersData(layers, layers='hab_extent', narrow=TRUE) %>%
    filter(category == "seaice_extent") %>%
    select(region_id=id_num, km2=val_num)
  
  ## data to calculate status/trend
  sea_ice <-  SelectLayersData(layers, layers='hab_sea_ice', narrow=TRUE) %>%
    select(region_id=id_num, year, days=val_num)
   
  ## reference years are first 10 years of the data
  ref_years <- min(sea_ice$year):(min(sea_ice$year) + 9)
  
  ## adding reference days
  sea_ice_status <- sea_ice %>%
    group_by(region_id) %>%
    mutate(ref_days = mean(days[year %in% ref_years])) %>%
    ungroup() %>%
    filter(ref_days >= 10) %>%   # must have at least 10 days of an ice season to be counted.
    mutate(status = days/ref_days) %>%
    mutate(status_mean = rollapply(status, width=5, FUN=mean, align="right", na.rm=TRUE, fill=NA)) %>%
    mutate(status_mean = ifelse(status_mean>1, 1, status_mean)) %>%
    data.frame()
  
  # trend calculation
  # (using 10 years in this case to be more likely to get longer term climate trends rather than shorter term trends):
  trend_years <- status_year:(status_year - 9)
  
  sea_ice_trend <- sea_ice_status %>%
  filter(year %in% trend_years) %>%
    filter(!is.na(status_mean)) %>%
    group_by(region_id) %>%
    do(mdl = lm(status_mean ~ year, data = .)) %>%
    summarize(region_id = region_id,
              score = coef(mdl)['year'] * 5) %>%
    ungroup() %>%
   mutate(score = ifelse(score>1, 1, score)) %>%
   mutate(score = ifelse(score<(-1), (-1), score)) %>%
    mutate(dimension = "trend") %>%
    select(region_id, dimension, score) 
  
  # final status data:
  sea_ice_status <- sea_ice_status %>%
    filter(year == status_year) %>%
    mutate(dimension = "status") %>%
    mutate(status_mean = 100*status_mean) %>%
    select(region_id, dimension, score = status_mean)
  
  # return scores
  scores <-  rbind(sea_ice_status, sea_ice_trend) %>%
    mutate(goal = "HAB") %>%
    data.frame()

  ## create a pressure layer: hd_sea_ice
  ## consider a 50% decrease in season length is a pressure score of 1
  
  ice_scale <- 50
  
  pressure <- sea_ice_status %>%
    mutate(score = 1 - (score-ice_scale)/ice_scale) %>%
    mutate(score = ifelse(score<0, 0, score)) %>%
    select(sp_id=region_id,pressure_score=score)
  if(sum(pressure$pressure_score<0 | pressure$pressure_score>1)>0){  
  stop("check pressure calculation in HAB function, scores are not between 0-1")}
  write.csv(pressure, 'temp/hd_sea_ice.csv', row.names=FALSE, quote=FALSE)
    return(scores)  
}


SPP = function(layers){

  # scores
  scores <- SelectLayersData(layers, layers=c('spp_status' = 'status', 'spp_trend' = 'trend'), narrow = TRUE) %>%
    select(region_id = id_num, dimension = layer, score = val_num) %>%
    mutate(score = ifelse(dimension %in% "status", score * 100, score)) %>%
    mutate(goal = "SPP")
  
  return(scores) 
}

BD = function(scores){
  
  d <-  scores %>%
    filter(goal %in% c('HAB', 'SPP')) %>%
    filter(!(dimension %in% c('pressures', 'resilience'))) %>%
    group_by(region_id, dimension) %>%
    summarize(score = mean(score, na.rm=TRUE))%>%
    mutate(goal = 'BD') %>%
    select(region_id, goal, dimension, score)
  
  # return all scores
  return(rbind(scores, d))
}

PreGlobalScores = function(layers, conf, scores){
    
  # get regions
  rgns = SelectLayersData(layers, layers=conf$config$layer_region_labels, narrow=T)
  
  # limit to just desired regions and global (region_id==0)
  scores = subset(scores, region_id %in% c(rgns$id_num, 0))
      
  return(scores)
}

FinalizeScores = function(layers, conf, scores){
  
  # get regions
  rgns = SelectLayersData(layers, layers=conf$config$layer_region_labels, narrow=T)
  
  # add NAs to missing combos (region_id, goal, dimension)
  d = expand.grid(list(score_NA  = NA,
                       region_id = c(rgns[,'id_num'], 0),
                       dimension = c('pressures','resilience','status','trend','future','score'), 
                       goal      = c(conf$goals$goal, 'Index')), stringsAsFactors=F); head(d)
  
  d = subset(d, 
             !(dimension %in% c('pressures','resilience','trend') & region_id==0) & 
             !(dimension %in% c('pressures','resilience','status','trend') & goal=='Index'))
  scores = merge(scores, d, all=T)[,c('goal','dimension','region_id','score')]
      
  # order
  scores = arrange(scores, goal, dimension, region_id)
  
  # round scores
  scores$score = round(scores$score, 2)
    
  return(scores)
}
