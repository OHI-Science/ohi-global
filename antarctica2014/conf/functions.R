Setup = function(){
  # empty for now
}

FIS = function(layers, status_year){
   
   trend_years <- (status_year-5):status_year
  
  c = SelectLayersData(layers, layers='fis_meancatch', narrow=T) %>%
    select(
      fao_saup_id    = id_chr,
      taxon_name_key = category,
      year,
      mean_catch     = val_num)  
  
  # separate out the region ids:
  c <-  c %>%
    separate(fao_saup_id, c("fao_id", "saup_id"), sep="_") %>%
    separate(taxon_name_key, c("taxon_name", "TaxonKey"), sep="_") %>%
    mutate(TaxonKey = as.numeric(TaxonKey)) %>%
    mutate(mean_catch = as.numeric(mean_catch)) %>%
    mutate(year = as.numeric(year)) %>%
    mutate(stock_id = paste(taxon_name, fao_id, sep="_"))
  
  # b_bmsy data
  b = SelectLayersData(layers, layer='fis_b_bmsy', narrow=T) %>%
    select(
      taxon_name       = category,
      year,
      b_bmsy           = val_num)
  
  # c_cmsy data
  extra_ccmsy = SelectLayersData(layers, layer='fis_c_cmsy', narrow=T) %>%
    select(
      fao_id          = id_num,
      taxon_name      = category,
      year,
      c_cmsy          = val_num)
  
  # region labels
  regions = SelectLayersData(layers, layer='rgn_labels') %>%
    select(
      fao_id    =       category,
      region_id      = id_num)
  
  
  # ------------------------------------------------------------------------
  # STEP 1. Merge the species status data with catch data
  #     AssessedCAtches: only taxa with catch status data
  # -----------------------------------------------------------------------
  AssessedCatches <- inner_join(b, c, 
                          by=c("taxon_name", "year"), type="inner") %>%
    filter(TaxonKey >= 6) %>%
    mutate(penalty = 1)
  
  
  # ------------------------------------------------------------------------
  # STEP 2. Estimate status data for catch taxa without species status
  #     UnAssessedCatches: taxa with catch status data
  # -----------------------------------------------------------------------
  
  UnAssessedCatches <- c[!(c$year %in% AssessedCatches$year &
                             c$taxon_name %in% AssessedCatches$taxon_name), ]
  
  # 2a.  Join UnAssessedCatches data to the b_bmsy summaries for each FAO/Year
  
  # Average status data for assessed stocks by FAO region for each year. 
  # This is used as the starting estimate for unassesed stocks
  # Here, the Median b_bmsy was chosen for TaxonKey >= 600000 
  # and for TaxonKey < 600000
  
  b_summary <- b %>%
    group_by(year) %>%
    summarize(Medianb_bmsy = quantile(b_bmsy, probs=c(0.5)))
    
  # minimum b_bmsy was used in 2013 OHI analysis to provide a conservative estimate of the b_bmsy
  # We now use the median to estimate b_bmsy for these taxa (OHI 2014, High seas, Antarctica).
  
  UnAssessedCatches <- UnAssessedCatches %>%
    left_join(b_summary, by=c("year"))
  
  
  # 2c. Create a penalty table for taxa not identified to species level
  #  *************NOTE *****************************
  #  In some cases, it may make sense to alter the 
  #  penalty for not identifying fisheries catch data to
  #  species level.
  #  ***********************************************
  
  penaltyTable <- data.frame(TaxonKey=1:6, 
                             penalty=c(0.01, 0.25, 0.5, 0.8, 0.9, 1))
  
  UnAssessedCatches <- UnAssessedCatches %>%
    left_join(penaltyTable, by="TaxonKey")
  
  # ------------------------------------------------------------------------
  # STEP 3. Calculate score for all taxa based on status (b/bmsy) and taxa
  # -----------------------------------------------------------------------
  
  #  *************NOTE *****************************
  #  These values can be altered
  #  ***********************************************
  alpha <- 0.5 # a 0 indicates no penalty for underharvesting
  beta <- 0.25 # this is the lowest the underharvesting penalty can get.
  lowerBuffer <- 0.95
  upperBuffer <- 1.05
  
  
  ## Function to calculate score for different scenarios:
  score <- function(data, variable){
    #data <- AssessedCatches
    #variable <- "bmsy"
    ifelse(data[ ,variable]*data[, "penalty"]<lowerBuffer,
           data[ ,variable]*data[, "penalty"],
           ifelse(data[ ,variable]*data[, "penalty"]>upperBuffer,
                  ifelse(1-alpha*(data[ ,variable]*data[, "penalty"]
                                  -upperBuffer)>beta,
                         1-alpha*(data[ ,variable]*data[, "penalty"]-upperBuffer),beta),
                  1))
  }
  
  AssessedCatches$score <- score(data=AssessedCatches, variable="b_bmsy")
  
  # Median is used to calculate score for species with Taxon 6 coding 
  # Not really necessary to separate in this case because "Median_bmsy" is used in both
  # cases, but will maintain code in case this approach changes:
  UnAssessedCatchesT6 <- subset(UnAssessedCatches, penalty==1)
  UnAssessedCatchesT6$score <- score(UnAssessedCatchesT6, "Medianb_bmsy")
  
  UnAssessedCatches <- subset(UnAssessedCatches, penalty!=1)
  UnAssessedCatches$score <- score(UnAssessedCatches, "Medianb_bmsy") 
  
  scores <- rbind(AssessedCatches[,c("taxon_name", "TaxonKey", "year", "fao_id", "mean_catch","score")],
                  UnAssessedCatchesT6[,c("taxon_name", "TaxonKey", "year", "fao_id", "mean_catch","score")],
                  UnAssessedCatches[,c("taxon_name", "TaxonKey", "year", "fao_id", "mean_catch","score")])
  
  # ------------------------------------------------------------------------
  # these species have c/cmsy assessments and their scores are generated using a different method:
  # ------------------------------------------------------------------------
  ## Fill in missing years/regions using mean of data
  # determine mean for each species/year and apply to missing data
  meanCMSY <- extra_ccmsy %>%
    group_by(taxon_name, year) %>%
    summarize(mean_cmsy = mean(c_cmsy, na.rm=TRUE)) %>%
    ungroup()
  
  
  scoresReplace <- scores %>%
    filter(taxon_name %in% c("Dissostichus mawsoni", "Champsocephalus gunnari", "Dissostichus eleginoides")) %>%
    mutate(fao_id = as.integer(fao_id)) %>%
    left_join(extra_ccmsy, by=c("taxon_name", "fao_id", "year")) %>%
    left_join(meanCMSY, by=c("taxon_name", "year")) %>%
    ungroup()
  
#   tmp <- c %>%
#     group_by(year) %>%
#     mutate(totalCatch = sum(mean_catch)) %>%
#     mutate(total3sp = sum(mean_catch[taxon_name %in% c("Dissostichus mawsoni", "Champsocephalus gunnari", "Dissostichus eleginoides")])) %>%
#     select(year, totalCatch, total3sp) %>%
#     unique() %>%
#     mutate(percent = totalCatch/total3sp) %>%
#     arrange(year)
#   
#   tmp <- c %>%
#     filter(year == 2012) %>%
#     arrange(mean_catch) %>%
#     filter(mean_catch > 100) %>%
#     select(taxon_name) %>%
#     unique()

  scoresReplace <- scoresReplace %>%
    mutate(c_cmsy2 = ifelse(is.na(c_cmsy), mean_cmsy, c_cmsy))
  
  
  ## calculate score based on c_cmsy
  eps <- .25 
  score_range  <- 1-0.25
  value_range <- 0.90-0
  
  scoresReplace <- scoresReplace %>%
    mutate(score = ifelse(c_cmsy2 > 1.0, 2.0-c_cmsy2, 
                               ifelse(c_cmsy2 < 0.9, eps + score_range/value_range * c_cmsy2, 1))) 
  
  png('temp/c_cmsyVSscore.png')
  plot(score ~ c_cmsy2, data=scoresReplace, xlab='c/cmsy', ylab="score", xlim=c(0,1), ylim=c(0,1))
  abline(0,1, col="red")
  dev.off()
  
  scoresReplace <- scoresReplace %>%
    select(taxon_name, TaxonKey, year, fao_id, mean_catch, score)
  
  ## replace old scores with newly calculated ccmsy
  
  scores <- scores %>%
    filter(!(taxon_name %in% c("Dissostichus mawsoni", "Champsocephalus gunnari", "Dissostichus eleginoides"))) %>%
    rbind(scoresReplace) %>%
    filter(year %in% trend_years)
  
  # ------------------------------------------------------------------------
  # STEP 4. Calculate status for each saup_id region
  # -----------------------------------------------------------------------
  
  # 4a. To calculate the weight (i.e, the relative catch of each stock per fao_id),
  # the mean catch of taxon is divided by the   
  # sum of mean catch of all species in region r, which is calculated as: 
  
  scores <- scores %>%
    group_by(year, fao_id) %>%
    mutate(SumCatch = sum(mean_catch)) %>%
    ungroup()
  

  scores <- scores %>%
    mutate(wprop = mean_catch/SumCatch) 
  
  
  #  4b. The "score" and "weight" values per taxon per SAUP region are used to  
  #    calculate a geometric weighted mean across taxa for each saup_id region
  
  StatusData <- scores %>%
    group_by(fao_id, year) %>%
    summarize(Status = prod(score^wprop)) %>%
    ungroup()
  
  ### standardized region names
  StatusData <- StatusData %>%
    mutate(fao_id = as.integer(fao_id)) %>%
    left_join(regions, by="fao_id") 
  
  # ------------------------------------------------------------------------
  # STEP 5. Status  
  # -----------------------------------------------------------------------
  status = StatusData %>%
    filter(year==status_year) %>%
    mutate(
      score     = round(Status*100),
      dimension = 'status') %>%
    select(region_id, dimension, score)
  
  # ------------------------------------------------------------------------
  # STEP 6. Calculate trend  
  # -----------------------------------------------------------------------
  # NOTE: Status is rounded to 2 digits before trend is 
  # calculated in order to match OHI 2013 results (is this what we want to do?)
  trend = StatusData %>%
    group_by(region_id) %>%
    do(mdl = lm(Status ~ year, data=.)) %>%
    summarize(region_id = region_id,
              score = coef(mdl)['year'] * 5)
  
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


NP = function(layers){
  # scores
  scores <- SelectLayersData(layers, layers=c('np_status'='status','np_trend'='trend'), narrow=T) %>%
    select(region_id = id_num, dimension = layer, score = val_num) %>%
    mutate(goal = "NP")
  
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
  
#   mpa <- read.csv('../ohiprep/Antarctica/AQ-LSP_v2014/data/lsp_prot_area_offshore.csv')
# 
#   area_inland <- read.csv('../ohiprep/Antarctica/Other_v2014/rgn_area_ccamlr_inland_1km_lyr.csv') %>%
#     mutate(type = "pa")
#   
#   area_offshore <- read.csv('antarctica2014/layers/rgn_area.csv') %>%
#     mutate(type = "cmpa")
#   
#   status_year <- 2015
#   
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
    select(sp_id, resilience.score=status)
  write.csv(resilience, "layers/MPAs.csv", row.names=FALSE)
  
  # add data to layers.csv
  layersData <- read.csv("layers.csv", stringsAsFactors = FALSE)
  newPressure <- data.frame(targets = as.character("resilience"),
                            layer = as.character("MPAs"),
                            name = as.character("Proportion of protected marine habitat (relative to 30% of total marine area)"),
                            description = as.character("Calculated in LSP function"),
                            fld_value = as.character("resilience.score"), 
                            units = as.character("resilience.score"),
                            filename = as.character("MPAs.csv"),
                            fld_id_num = as.character("sp_id"),
                            fld_val_num = as.character("resilience.score"),
                            file_exists = TRUE,
                            val_min = min(resilience$resilience.score, na.rm=TRUE),
                            val_max = max(resilience$resilience.score, na.rm=TRUE),
                            val_0to1 = ifelse(min(resilience$resilience.score, na.rm=TRUE) >= 0 &
                                                max(resilience$resilience.score, na.rm=TRUE) <= 1, 
                                              TRUE, FALSE),
                            num_ids_unique = sum(unique(resilience$sp_id)), 
                            data_na = FALSE)
  layers <- bind_rows(layersData, newPressure)
  write.csv(layers, "layers.csv", row.names=FALSE)
  layers <<-  Layers('layers.csv','layers')
  
  
  ### Land-based: assumed to be status = 100 (all land protected in Antarctica)
  pa_status <- mpa_status %>%
    select(type, sp_id, year) %>%
    mutate(type = "pa", status = 1)
  
  ## merge inland and offshore data
  
  # first combine inland and offshore area data
  areas <- rbind(area_inland, area_offshore) 
  
  
  # take weighted mean of inland and offshore status values, based on area
  status <- rbind(mpa_status, pa_status) %>%
    left_join(areas, by=c('type', 'sp_id')) %>%
    group_by(sp_id, year) %>%
    summarize(score = weighted.mean(status, area_km2, na.rm=TRUE)) %>%
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
  # layers
  lyrs = c('po_chemicals' = 'l',
           'po_trash'     = 'd',
           'cw_chemical_trend'   = 'chem_trend',
           'cw_trash_trend'  = 'trash_trend')
    
  ## At this point, trend assumed to be zero based perfect/near perfect scores
  
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
  write.csv(pressure, 'layers/hd_sea_ice.csv', row.names=FALSE)
  
  # add data to layers.csv
  layersData <- read.csv("layers.csv", stringsAsFactors = FALSE)
  newPressure <- data.frame(targets = as.character("pressures"),
                            layer = as.character("hd_sea_ice"),
                            name = as.character("Relative decreases in sea ice season length (d)"),
                            description = as.character("Calculated in HAB function"),
                            fld_value = as.character("pressure_score"), 
                            units = as.character("pressure_score"),
                            filename = as.character("hd_sea_ice.csv"),
                            fld_id_num = as.character("sp_id"),
                              fld_val_num = as.character("pressure_score"),
                            file_exists = TRUE,
                            val_min = min(pressure$pressure_score, na.rm=TRUE),
                            val_max = max(pressure$pressure_score, na.rm=TRUE),
                            val_0to1 = ifelse(min(pressure$pressure_score, na.rm=TRUE) >= 0 &
                                             max(pressure$pressure_score, na.rm=TRUE) <= 1, 
                                             TRUE, FALSE),
                            num_ids_unique = sum(unique(pressure$sp_id)), 
                              data_na = FALSE)
   layers <- bind_rows(layersData, newPressure)
   write.csv(layers, "layers.csv", row.names=FALSE)
   layers <<-  Layers('layers.csv','layers')
  
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
    summarize(score = mean(score))%>%
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
  
  #  browser()
  
  # get area data
  area = SelectLayersData(layers, layers='rgn_area', narrow=TRUE)  
  area <- area %>%
    select(region_id=id_num, area=val_num)
  # get regions
  rgns = SelectLayersData(layers, layers=conf$config$layer_region_labels, narrow=T)
  
  
tmp <-   scores %>%
    join(area, by='region_id') %>% 
  filter(region_id != 0) %>%
  group_by(goal, dimension) %>%
  summarize(region_id0 = weighted.mean(score, area, na.rm=TRUE))
  
  
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
