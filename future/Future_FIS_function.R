### Paths to data: ohiprep:
# ohiprep:globalprep/SAUP_FIS/v2015/data	fnk_fis_b_bmsy_lyr.csv
# ohiprep:globalprep/SAUP_FIS/v2015/data	mean_catch.csv


FIS = function(layers, status_year){
  #catch data
  c = SelectLayersData(layers, layers='fis_meancatch', narrow = TRUE) %>%
    select(
      fao_ohi_id    = id_chr,
      taxon_name_key = category,
      year,
      catch          = val_num)  
  # b_bmsy data
  b = SelectLayersData(layers, layer='fis_b_bmsy', narrow = TRUE) %>%
    select(
      fao_ohi_id         = id_chr,
      TaxonKey      = category,
      year,
      bmsy           = val_num)
  
  
  # separate out the region ids:
  c <- separate(c, fao_ohi_id, c("fao_id", "rgn_id"))
  c <- separate(c, taxon_name_key, c("TaxonName", "TaxonKey"), sep="_")
  c <- c %>%
    mutate(catch = as.numeric(catch)) %>%
    mutate(year = as.numeric(as.character(year))) %>%
    mutate(fao_id = as.numeric(as.character(fao_id))) %>%
    mutate(rgn_id = as.numeric(as.character(rgn_id))) %>%
    mutate(TaxonKey = as.numeric(as.character(TaxonKey))) %>%
    mutate(stock_id = paste(TaxonKey, fao_id, rgn_id, sep="_")) %>% #ID to link to b/bmsy of assessed stocks
    mutate(stock_year = paste(stock_id, year, sep="_"))
  
  # Identifier taxa/fao region:
  b <- separate(b, fao_ohi_id, c("fao_id", "rgn_id"))
  b <- b %>%
    mutate(bmsy = as.numeric(bmsy)) %>%
    mutate(fao_id = as.numeric(as.character(fao_id))) %>%
    mutate(rgn_id = as.numeric(as.character(rgn_id))) %>%
    mutate(year = as.numeric(as.character(year))) %>%
    mutate(stock_id = paste(TaxonKey, fao_id, rgn_id, sep="_")) %>%
    mutate(stock_year = paste(stock_id, year, sep="_"))
  
  # ------------------------------------------------------------------------
  # STEP 1. Calculate scores for Bbmsy values
  # -----------------------------------------------------------------------
  #  *************NOTE *****************************
  #  These values can be altered
  #  ***********************************************
  alpha <- 0.5
  beta <- 0.25
  lowerBuffer <- 0.95
  upperBuffer <- 1.05
  
  b$score = ifelse(b$bmsy < lowerBuffer, b$bmsy,
                   ifelse (b$bmsy >= lowerBuffer & b$bmsy <= upperBuffer, 1, NA))
  b$score = ifelse(!is.na(b$score), b$score,  
                   ifelse(1 - alpha*(b$bmsy - upperBuffer) > beta,
                          1 - alpha*(b$bmsy - upperBuffer), 
                          beta))
  
  # ------------------------------------------------------------------------
  # STEP 1. Merge the species status (b/bmsy) data with catch data
  #     AssessedCatches: only taxa with catch status data
  # -----------------------------------------------------------------------
  AssessedCatches <- b %>%
    left_join(c, by=c('fao_id', 'rgn_id', 'TaxonKey', 'stock_id', 'stock_year', 'year')) %>%
    select(stock_year, TaxonName, TaxonKey, year, fao_id, rgn_id, catch, score)
  
  # ------------------------------------------------------------------------
  # STEP 2. Estimate status data for catch taxa without species status
  #     UnAssessedCatches: taxa with catch status data
  # -----------------------------------------------------------------------  
  UnAssessedCatches <- c %>%
    filter(!(stock_year %in% AssessedCatches$stock_year))
  
  # 2a.  Join UnAssessedCatches data to the b_bmsy summaries for each FAO/rgn/year
  # Average status data for assessed stocks by FAO/region for each year. 
  # This is used as the starting estimate for unassesed stocks
  
  b_summary <- b %>%
    group_by(fao_id, rgn_id, year) %>%
    summarize(Median_score = quantile(score, probs=c(0.5))) %>%
    ungroup() 
  
  UnAssessedCatches <- UnAssessedCatches %>%
    left_join(b_summary, by=c('fao_id', 'rgn_id', 'year')) %>%
    filter(!is.na(Median_score))
  
  
  
  # 2b. Create a penalty table for taxa not identified to species level
  #  *************NOTE *****************************
  #  In some cases, it may make sense to alter the 
  #  penalty for not identifying fisheries catch data to
  #  species level.
  #  ***********************************************
  
  penaltyTable <- data.frame(TaxonPenaltyCode=1:6, 
                             penalty=c(0.01, 0.25, 0.5, 0.8, 0.9, 1))
  
  UnAssessedCatches <- UnAssessedCatches %>%
    mutate(TaxonPenaltyCode = as.numeric(substring(TaxonKey, 1, 1))) %>%
    left_join(penaltyTable, by='TaxonPenaltyCode')
  
  # ------------------------------------------------------------------------
  # STEP 3. Calculate score for all taxa based on status (b/bmsy) and taxa
  # -----------------------------------------------------------------------
  UnAssessedCatches <- UnAssessedCatches %>%
    mutate(score = Median_score * penalty) 
  
  gap_fill <- bind_rows(UnAssessedCatches, AssessedCatches) %>%
    mutate(gap_fill = ifelse(is.na(penalty), "none", "median")) %>%
    mutate(penalty = ifelse(is.na(penalty), 1, penalty)) %>%
    select(fao_id, rgn_id, TaxonName, TaxonKey, year, catch, Median_score, penalty, score, gap_fill) %>%
    filter(year == status_year)
  write.csv(gap_fill, 'temp/FIS_summary.csv', row.names=FALSE)
  
  # checking different regions
  # gap_fill <- read.csv('eez2013/temp/FIS_summary.csv')
  # 
  #   tmp <- gap_fill %>%
  #     filter(rgn_id==208) %>%
  #     arrange(catch)
  #   data.frame(tmp)
  #   tmp$catch[length(tmp$catch)]/sum(tmp$catch)
  
  UnAssessedCatches <- UnAssessedCatches  %>%
    select(stock_year, TaxonName, TaxonKey, year, fao_id, rgn_id, catch, score)
  
  AllScores <- rbind(AssessedCatches, UnAssessedCatches)
  
  # ------------------------------------------------------------------------
  # STEP 4. Calculate status for each region
  # -----------------------------------------------------------------------
  
  # 4a. To calculate the weight (i.e, the relative catch of each stock per saup_id),
  # the mean catch of taxon i is divided by the   
  # sum of mean catch of all species in region/year 
  
  AllScores <- AllScores %>%
    group_by(year, rgn_id) %>%
    mutate(SumCatch = sum(catch)) %>%
    ungroup() %>%
    mutate(wprop = catch/SumCatch)
  
  
  #  4b. The "score" and "weight" values per taxon per SAUP region are used to  
  #    calculate a geometric weighted mean across taxa for each saup_id region
  
  StatusData <- AllScores %>%
    group_by(rgn_id, year) %>%
    summarize(status = prod(score^wprop)) %>%
    ungroup()
  
  # ------------------------------------------------------------------------
  # STEP 5. Get yearly status and trend  
  # -----------------------------------------------------------------------
  
  # 2015 status is based on 2010 data (most recent data)
  status <-  StatusData %>%
    filter(year==status_year) %>%
    mutate(
      score     = round(status*100, 1),
      dimension = 'status') %>%
    select(region_id=rgn_id, score, dimension)
  
  # ## save status scores:
  #   regions <- read.csv('../ohiprep/src/LookupTables/eez_rgn_2013master.csv') %>%
  #     select(region_id = rgn_id_2013, rgn_name = rgn_nam_2013)
  
  #  To save a file with status scores and country names  
  #   status_save <- status %>%
  #     left_join(regions) %>%
  #     arrange(-score)
  #   write.csv(status_save, "FIS_status.csv", row.names=FALSE)  
  
  trend_years <- status_year:(status_year-4)
  
  trend <- StatusData %>%
    filter(year==trend_years) %>%
    group_by(rgn_id) %>%
    do(mdl = lm(status ~ year, data=.)) %>%
    summarize(region_id = rgn_id,
              score = round(coef(mdl)['year'] * 5, 2),
              dimension = 'trend') %>%
    ungroup()
  
  
  # assemble dimensions
  scores <- rbind(status, trend) %>% 
    mutate(goal='FIS') %>%
    filter(region_id != 255)
  scores <- data.frame(scores)
  
  return(scores)
}