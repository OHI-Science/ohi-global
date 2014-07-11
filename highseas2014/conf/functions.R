
FIS = function(layers, status_year=2011){
  library(stringr)

  # layers used: fis_meancatch, fis_b_bmsy, FAOregions

#   #mel stuff
#   fis_meancatch <- read.csv("N:\\model\\GL-HS-AQ-Fisheries_v2013\\HighSeas\\data\\cnk_fis_meancatch.csv")
#   fis_b_bmsy <- read.csv("N:\\model\\GL-NCEAS_FIS_v2013a\\RevisingFIS\\data\\fis_b_bmsy_lyr.csv")
#   FAOregions <- read.csv("N:\\model\\GL-HS-AQ-Fisheries_v2013\\HighSeas\\data\\FAOregions.csv")
# 
#   c <- fis_meancatch
#   c$fao_saup_id <- as.character(c$fao_saup_id)
#   b <- fis_b_bmsy
#   a <- FAOregions
#   

#status_year=2011
#   
#     # end: mel stuff
  
  # catch data
 c = SelectLayersData(layers, layer='fis_meancatch', narrow=T) %.%
    select(
      fao_saup_id    = id_chr,
      taxon_name_key = category,
      year,
      mean_catch     = val_num) %.%
    mutate(
      fao_id     = as.numeric(str_replace(fao_saup_id   , '^(.*)_(.*)$', '\\1')),
      saup_id    = as.numeric(str_replace(fao_saup_id   , '^(.*)_(.*)$', '\\2')),
      taxon_name =            str_replace(taxon_name_key, '^(.*)_(.*)$', '\\1'),
      TaxonKey   = as.numeric(str_replace(taxon_name_key, '^(.*)_(.*)$', '\\2')),   
      #Create Identifier for linking assessed stocks with country-level catches
      stock_id   = sprintf('%s_%d', taxon_name, fao_id))
  
  # separate out the region ids:
  
  # b_bmsy data
  b = SelectLayersData(layers, layer='fis_b_bmsy', narrow=T) %.%
    select(
      fao_id      = id_num,
      taxon_name  = category,
      year,
      b_bmsy      = val_num) %.%
    mutate(
      stock_id    = paste(taxon_name, fao_id, sep="_"))
  

  # Identifier taxa/fao region:
  
  # area data for saup to rgn conversion
  a = SelectLayersData(layers, layer='FAOregions', narrow=T) %.%
    select(
      rgn_id = id_num,
      fao_id = val_num)
  
  # ------------------------------------------------------------------------
  # STEP 1. Merge the species status data with catch data
  #     AssessedCAtches: only taxa with catch status data
  # -----------------------------------------------------------------------
  AssessedCatches <- join(b, c, 
                          by=c("stock_id", "year"), type="inner")
  
  # b,c by stock_id
  
  # include only taxa with species-level data
  AssessedCatches <- AssessedCatches[as.numeric(AssessedCatches$TaxonKey)>=600000, ]
  AssessedCatches$penalty <- 1

# DataCheck <- AssessedCatches %.%
#   filter(year %in% 2011) %.%
#   select(fao_id, taxon_name, year, b_bmsy, mean_catch) %.%
#   arrange(fao_id, b_bmsy)
# write.csv(DataCheck, "C:\\Users\\Melanie\\Desktop\\HS Explore\\FIS_bmsy.csv", row.names=FALSE)
  
  # ------------------------------------------------------------------------
  # STEP 2. Estimate status data for catch taxa without species status
  #     UnAssessedCatches: taxa with catch status data
  # -----------------------------------------------------------------------  
  UnAssessedCatches <- c[!(c$year %in% AssessedCatches$year &
                             c$stock_id %in% AssessedCatches$stock_id), ]
  
  # 2a.  Join UnAssessedCatches data to the b_bmsy summaries for each FAO/Year
  
  # Average status data for assessed stocks by FAO region for each year. 
  # This is used as the starting estimate for unassesed stocks
  # Here, the Median b_bmsy was chosen for TaxonKey >= 600000 
  # and Min b_bmsy for TaxonKey < 600000
  #  *************NOTE *****************************
  #  Using the minimum B/BMSY score as an starting point
  #  for the estimate of B/BMSY for unassessed taxa not
  #  identified to species level is very conservative.
  #  This is a parameter that can be changed.
  #  ***********************************************
  b_summary <- ddply(b, .(fao_id, year), summarize,
                     Medianb_bmsy=quantile(as.numeric(b_bmsy), probs=c(0.5)), 
                     Minb_bmsy=min(as.numeric(b_bmsy)))
  
  UnAssessedCatches <- join(UnAssessedCatches, b_summary, by=c("fao_id", "year"),
                            type="left", match="all")
  # 2b.  Create a penalty variable based on taxa level:
  UnAssessedCatches$TaxonPenaltyCode <- substring(UnAssessedCatches$TaxonKey,1,1)
  
  # 2c. Create a penalty table for taxa not identified to species level
  #  *************NOTE *****************************
  #  In some cases, it may make sense to alter the 
  #  penalty for not identifying fisheries catch data to
  #  species level.
  #  ***********************************************
  penaltyTable <- data.frame(TaxonPenaltyCode=1:6, 
                             penalty=c(0.01, 0.25, 0.5, 0.8, 0.9, 1))
  # 2d.Merge with data
  UnAssessedCatches <- join(UnAssessedCatches, penaltyTable, by="TaxonPenaltyCode")

#  DataCheck <- UnAssessedCatches %.%
#   filter(year %in% 2011) %.%
#   mutate(b_bmsy = Medianb_bmsy*penalty) %.%
#   select(fao_id, taxon_name, year, b_bmsy, mean_catch) %.%
#   arrange(fao_id, b_bmsy)
# write.csv(DataCheck, "C:\\Users\\Melanie\\Desktop\\HS Explore\\FIS_bmsy_Unassessed_full.csv", row.names=FALSE)


  # ------------------------------------------------------------------------
  # STEP 3. Calculate score for all taxa based on status (b/bmsy) and taxa
  # -----------------------------------------------------------------------
  
  #  *************NOTE *****************************
  #  These values can be altered
  #  ***********************************************
  alpha <- 0.5
  beta <- 0.25
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
  UnAssessedCatchesT6 <- subset(UnAssessedCatches, penalty==1)
  UnAssessedCatchesT6$score <- score(UnAssessedCatchesT6, "Medianb_bmsy")
  
## was originally using: Minb_bmsy, changed to median
  UnAssessedCatches <- subset(UnAssessedCatches, penalty!=1)
  UnAssessedCatches$score <- score(UnAssessedCatches, "Medianb_bmsy")
  
  AllScores <- rbind(AssessedCatches[,c("taxon_name", "TaxonKey", "year", "fao_id", "saup_id", "mean_catch","score")],
                  UnAssessedCatchesT6[,c("taxon_name", "TaxonKey", "year", "fao_id", "saup_id", "mean_catch","score")],
                  UnAssessedCatches[,c("taxon_name", "TaxonKey", "year", "fao_id", "saup_id", "mean_catch","score")])
    
  # ------------------------------------------------------------------------
  # STEP 4. Calculate status for each saup_id region
  # -----------------------------------------------------------------------
  
  # 4a. To calculate the weight (i.e, the relative catch of each stock per saup_id),
  # the mean catch of taxon i is divided by the   
  # sum of mean catch of all species in region r, which is calculated as: 
  
  smc <- ddply(.data = AllScores, .(year, fao_id), summarize, 
               SumCatch = sum(mean_catch))   
  AllScores<-join(AllScores,smc,by=c("year","fao_id"))  
  AllScores$wprop<-AllScores$mean_catch/AllScores$SumCatch 
  
  
  #  4b. The "score" and "weight" values per taxon per SAUP region are used to  
  #    calculate a geometric weighted mean across taxa for each saup_id region
  geomMean <- ddply(.data = AllScores, .(fao_id, year), summarize, Status = prod(score^wprop)) 
  
  # ------------------------------------------------------------------------
  # STEP 5. Convert FAO regions to OHI regions  
  # -----------------------------------------------------------------------
    
  # Join region names/ids to Geom data
  StatusData <- geomMean %.% 
    mutate(fao_id = as.integer(fao_id)) %.%
    left_join(a, by='fao_id') %.%
    select(rgn_id = rgn_id, year, Status)
  
  # 2013 status is based on 2011 data (most recent data)
  status = StatusData %.%
    filter(year==status_year) %.%
    mutate(
      score     = round(Status*100),
      dimension = 'status') %.%
    select(region_id=rgn_id, dimension, score)
    
    
  # ------------------------------------------------------------------------
  # STEP 6. Calculate trend  
  # -----------------------------------------------------------------------
  # NOTE: Status is rounded to 2 digits before trend is 
  # calculated in order to match OHI 2013 results (is this what we want to do?)
  trend = ddply(StatusData, .(rgn_id), function(x){
    mdl = lm(Status ~ year, data=x)
    data.frame(
      score     = round(coef(mdl)[['year']] * 5, 2),
      dimension = 'trend')}) %.%
    select(region_id=rgn_id, dimension, score)
  # %.% semi_join(status, by='rgn_id')
  
# hack: Arctic region has no status (due to no catch data for 2011, and basically the past 5 years)
# given this, trend should be zero (It's not because 2007 had a catch of 1)
trend$score[trend$region_id == "260"] <- NA

  # assemble dimensions
  scores = rbind(status, trend) %.% mutate(goal='FIS')
  return(scores)  
}


FP = function(layers, scores){
    
  # scores
  s = scores %.%
    filter(goal %in% c('FIS') & dimension %in% c('status','trend','future','score')) %.%
    # NOTE: resilience and pressure skipped for supra-goals
    mutate(goal = 'FP')
  
  # return all scores
  return(rbind(scores, s))
}



ICO = function(layers){
  
  # scores
  scores = SelectLayersData(
    layers, 
    layers=c(
      'ico_spp_extinction_status' = 'status',
      'ico_spp_popn_trend'        = 'trend'), narrow=T) %.%
    select(
      region_id = id_num,
      dimension = layer,
      score     = val_num) %.%
    mutate(
      goal      = 'ICO',
      score     = ifelse(dimension=='status', score, score))
  
  return(scores) 
  
}

SP = function(scores){
  
  # scores
  s = scores %.%
    filter(goal %in% c('ICO') & dimension %in% c('status','trend','future','score')) %.%
    mutate(goal = 'SP')
  
  # return all scores
  return(rbind(scores, s))
}



SPP = function(layers){

  # scores
  scores = SelectLayersData(layers, layers=c('spp_status'='status','spp_trend'='trend'), narrow=T) %.%
    select(
      region_id = id_num,
      dimension = layer,
      score     = val_num) %.%
    mutate(
      goal      = 'SPP',
      score     = ifelse(dimension=='status', score, score))
  
  return(scores) 
}

BD = function(scores){
  
  # scores
  s = scores %.%
    filter(goal %in% c('SPP') & dimension %in% c('status','trend','future','score')) %.%
    mutate(goal = 'BD')
  
  # return all scores
  return(rbind(scores, s))
}



PreGlobalScores = function(layers, conf, scores){
    
  # get regions
  rgns = SelectLayersData(layers, layers=conf$config$layer_region_labels, narrow=T)
  
  # limit to just desired regions and global (region_id==0)
  scores = subset(scores, region_id %in% c(rgns[,'id_num'], 0))
  
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
