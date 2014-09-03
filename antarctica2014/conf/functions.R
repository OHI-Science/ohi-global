Setup = function(){
  # empty for now
}

FIS = function(layers, status_year){
   
   trend_years <- (status_year-5):status_year
  
  c = SelectLayersData(layers, layers='fis_meancatch', narrow=T) %.%
    select(
      fao_saup_id    = id_chr,
      taxon_name_key = category,
      year,
      mean_catch          = val_num)  
  
  # separate out the region ids:
  c$fao_id    <- as.numeric(sapply(strsplit(as.character(c$fao_saup_id), "_"), function(x)x[1]))
  c$saup_id   <- as.numeric(sapply(strsplit(as.character(c$fao_saup_id), "_"), function(x)x[2]))
  c$taxon_name <- sapply(strsplit(as.character(c$taxon_name_key), "_"), function(x)x[1])
  c$TaxonKey  <- as.numeric(sapply(strsplit(as.character(c$taxon_name_key), "_"), function(x)x[2]))
  c$mean_catch     <- as.numeric(c$mean_catch)
  c$year      <- as.numeric(as.character(c$year))
  #Create Identifier for linking assessed stocks with country-level catches
  c$stock_id <- paste(as.character(c$TaxonName),
                      as.character(c$fao_id), sep="_")
  
  # b_bmsy data
  b = SelectLayersData(layers, layer='fis_b_bmsy', narrow=T) %.%
    select(
      taxon_name      = category,
      year,
      b_bmsy           = val_num)
  
  # c_cmsy data
  extra_ccmsy = SelectLayersData(layers, layer='fis_c_cmsy', narrow=T) %.%
    select(
      fao_id    =       id_num,
      taxon_name      = category,
      year,
      c_cmsy           = val_num)
  
  # region labels
  regions = SelectLayersData(layers, layer='rgn_labels') %.%
    select(
      fao_id    =       category,
      region_id      = id_num)
  
  
  # ------------------------------------------------------------------------
  # STEP 1. Merge the species status data with catch data
  #     AssessedCAtches: only taxa with catch status data
  # -----------------------------------------------------------------------
  AssessedCatches <- join(b, c, 
                          by=c("taxon_name", "year"), type="inner")
  
  # include only taxa with species-level data
  AssessedCatches <- AssessedCatches[as.numeric(AssessedCatches$TaxonKey)>=6, ]
  AssessedCatches$penalty <- 1
  
  
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
  
  #  *************NOTE *****************************
  #  Using the minimum B/BMSY score as an starting point
  #  for the estimate of B/BMSY for unassessed taxa not
  #  identified to species level is very conservative.
  #  This is a parameter that can be changed.  Here we used the
  #  median
  #  ***********************************************
  
  b_summary <- ddply(b, .(year), summarize,
                     Medianb_bmsy=quantile(as.numeric(b_bmsy), probs=c(0.5)), 
                     Minb_bmsy=min(as.numeric(b_bmsy))) 
  # minimum b_bmsy was used in 2013 OHI analysis to provide a conservative estimate of the b_bmsy
  # We now use the median to estimate b_bmsy for these taxa (OHI 2014, High seas, Antarctica).
  
  UnAssessedCatches <- join(UnAssessedCatches, b_summary, by=c("year"),
                            type="left", match="all")
  
  
  # 2c. Create a penalty table for taxa not identified to species level
  #  *************NOTE *****************************
  #  In some cases, it may make sense to alter the 
  #  penalty for not identifying fisheries catch data to
  #  species level.
  #  ***********************************************
  
  penaltyTable <- data.frame(TaxonKey=1:6, 
                             penalty=c(0.01, 0.25, 0.5, 0.8, 0.9, 1))
  # 2d.Merge with data
  UnAssessedCatches <- join(UnAssessedCatches, penaltyTable, by="TaxonKey")
  
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
  UnAssessedCatches$score <- score(UnAssessedCatches, "Medianb_bmsy") #this used to be the Minb_bmsy 
  
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
    summarize(mean_cmsy = mean(c_cmsy, na.rm=TRUE))
  
  
  scoresReplace <- scores %>%
    filter(taxon_name %in% c("Dissostichus mawsoni", "Champsocephalus gunnari", "Dissostichus eleginoides")) %>%
    mutate(fao_id = as.integer(fao_id)) %>%
    left_join(extra_ccmsy, by=c("taxon_name", "fao_id", "year")) %>%
    left_join(meanCMSY, by=c("taxon_name", "year"))
  
  scoresReplace$c_cmsy2 <- ifelse(is.na(scoresReplace$c_cmsy), scoresReplace$mean_cmsy, scoresReplace$c_cmsy)
  
  
  ## calculate score based on c_cmsy
  eps <- .25 
  score_range  <- 1-0.25
  value_range <- 0.90-0
  
  scoresReplace$score = ifelse(scoresReplace$c_cmsy2 > 1.0, 2.0-scoresReplace$c_cmsy2, 
                               ifelse(scoresReplace$c_cmsy2 < 0.9, eps + score_range/value_range * scoresReplace$c_cmsy2, 1)) 
  
  scoresReplace <- scoresReplace %.%
    select(taxon_name, TaxonKey, year, fao_id, mean_catch, score)
  
  ## replace old scores with newly calculated ccmsy
  
  scores <- scores[!(scores$taxon_name %in% c("Dissostichus mawsoni", "Champsocephalus gunnari", "Dissostichus eleginoides")), ]
  
  scores <- rbind(scores, scoresReplace)
  
  scores <- scores[scores$year %in% trend_years, ]
  # ------------------------------------------------------------------------
  # STEP 4. Calculate status for each saup_id region
  # -----------------------------------------------------------------------
  
  # 4a. To calculate the weight (i.e, the relative catch of each stock per fao_id),
  # the mean catch of taxon is divided by the   
  # sum of mean catch of all species in region r, which is calculated as: 
  
  smc <- ddply(.data = scores, .(year, fao_id), summarize, 
               SumCatch = sum(mean_catch)) 
  
  scores<-join(scores,smc,by=c("year","fao_id"))
  
  scores$wprop<-scores$mean_catch/scores$SumCatch 
  
  
  #  4b. The "score" and "weight" values per taxon per SAUP region are used to  
  #    calculate a geometric weighted mean across taxa for each saup_id region
  StatusData <- ddply(.data = scores, .(fao_id, year), summarize, Status = prod(score^wprop)) 
  
  ### standardized region names
  StatusData <- StatusData %.%
    mutate(fao_id = as.integer(fao_id)) %.%
    left_join(regions, by="fao_id") 
  
  # ------------------------------------------------------------------------
  # STEP 5. Status  
  # -----------------------------------------------------------------------
  status = StatusData %.%
    filter(year==status_year) %.%
    mutate(
      score     = round(Status*100),
      dimension = 'status') %.%
    select(region_id, dimension, score)
  
  # ------------------------------------------------------------------------
  # STEP 6. Calculate trend  
  # -----------------------------------------------------------------------
  # NOTE: Status is rounded to 2 digits before trend is 
  # calculated in order to match OHI 2013 results (is this what we want to do?)
  trend = ddply(StatusData, .(region_id), function(x){
    mdl = lm(Status ~ year, data=x)
    data.frame(
      score     = round(coef(mdl)[['year']] * 5, 2),
      dimension = 'trend')}) %.%
    select(region_id, dimension, score)
  # %.% semi_join(status, by='rgn_id')
  
  # assemble dimensions
  scores = rbind(status, trend) %.% mutate(goal='FIS')
  return(scores)  
  
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
             !(dimension %in% c('pressures','resilience','status','trend') & goal=='Index')
  scores = merge(scores, d, all=T)[,c('goal','dimension','region_id','score')]
      
  # order
  scores = arrange(scores, goal, dimension, region_id)
  
  # round scores
  scores$score = round(scores$score, 2)
    
  return(scores)
}
