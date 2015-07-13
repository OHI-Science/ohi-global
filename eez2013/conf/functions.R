#
Setup = function(){
  
# this code bombs the shinyapps.io deploy, so commenting out and relying on package prefixes to guide install zoo::, psych::
#   extra.packages.required = c('zoo','psych') # zoo for MAR(), NP(); psych for geometric.mean
#   
#   # install packages if needed
#   for (p in extra.packages.required){
#     if (!suppressWarnings(library(p, character.only = TRUE, logical.return = TRUE))){
#       cat(sprintf('\n\nInstalling %s...\n', p))
#       install.packages(p)
#       require(p, character.only = TRUE)
#     }
#   }
  
  # csv comparison function, made global
  csv_compare <<- function(o, step, prefix=sprintf('temp/%s_MAR', basename(getwd()))){
    
    dir_temp = basename(dirname(prefix))
    if (!file.exists(dir_temp)) dir.create(dir_temp, recursive = TRUE)
    
    csv = sprintf('%s_%s_B.csv', prefix, step)
    if (!file.exists(csv)){
      cat(sprintf('DEBUG: writing %s.\n', csv))
      write.csv(o, csv, row.names = FALSE, na='')     
    }
    x = read.csv(csv, check.names = FALSE)
    
    # custom modifications
    if (step=="1-rky"){x = x %>% arrange(rgn_id, species)}
    if (step=="2-rky-smooth"){x = x %>% arrange(rgn_id, species)}
    if (step=='3-m-melt'){ x$year = factor(x$year, levels=levels(o$year))
                           x = x %>%
                             arrange(rgn_id, species)} # [1] "Component “year”: 'current' is not a factor"
    if (step=="4-m-within"){x = x %>% arrange(rgn_id, species)}
    if (step=="5-m-merge"){x = x %>% arrange(rgn_id, species, species_code)}
    if (step=='7-ref95pct-quantile'){ x = setNames(as.numeric(x), '95%') }
    

    eq = all.equal(o, x)
    if (class(eq) == 'character'){
      csv = sprintf('%s_%s_A.csv', prefix, step)
      cat(sprintf('DEBUG: NOT EQUAL! writing %s.\n', csv))
      print(eq)
      write.csv(o, csv, row.names = FALSE, na='') 
    }
    return(x)
  }
  
}

FIS = function(layers, status_year){
  # layers used: fis_meancatch, fis_b_bmsy, fis_proparea_saup2rgn
      
  # catch data
  c = SelectLayersData(layers, layers='fis_meancatch', narrow = TRUE) %>%
    select(
      fao_saup_id    = id_chr,
      taxon_name_key = category,
      year,
      catch          = val_num)  
  
  # separate out the region ids:
  c$fao_id    <- as.numeric(sapply(strsplit(as.character(c$fao_saup_id), "_"), function(x)x[1]))
  c$saup_id   <- as.numeric(sapply(strsplit(as.character(c$fao_saup_id), "_"), function(x)x[2]))
  c$TaxonName <- sapply(strsplit(as.character(c$taxon_name_key), "_"), function(x)x[1])
  c$TaxonKey  <- as.numeric(sapply(strsplit(as.character(c$taxon_name_key), "_"), function(x)x[2]))
  c$catch     <- as.numeric(c$catch)
  c$year      <- as.numeric(as.character(c$year))
  #Create Identifier for linking assessed stocks with country-level catches
  c$stock_id <- paste(as.character(c$TaxonName),
                      as.character(c$fao_id), sep="_")
  
  # b_bmsy data
  b = SelectLayersData(layers, layer='fis_b_bmsy', narrow = TRUE) %>%
    select(
      fao_id         = id_num,
      TaxonName      = category,
      year,
      bmsy           = val_num)
  # Identifier taxa/fao region:
  b$stock_id <- paste(b$TaxonName, b$fao_id, sep="_")
  b$bmsy     <- as.numeric(b$bmsy)
  b$fao_id   <- as.numeric(as.character(b$fao_id))
  b$year     <- as.numeric(as.character(b$year))
    
  
  # area data for saup to rgn conversion
  a = layers$data[['fis_proparea_saup2rgn']] %>%
    select(saup_id, rgn_id, prop_area)
  a$prop_area <- as.numeric(a$prop_area)
  a$saup_id   <- as.numeric(as.character(a$saup_id))
  a$rgn_id    <- as.numeric(as.character(a$rgn_id))
  
  # ------------------------------------------------------------------------
  # STEP 1. Merge the species status data with catch data
  #     AssessedCAtches: only taxa with catch status data
  # -----------------------------------------------------------------------
  AssessedCatches <- join(b, c, 
                          by = c("stock_id", "year"), type="inner")
  
  # b,c by stock_id
  
  # include only taxa with species-level data
  AssessedCatches <- AssessedCatches[as.numeric(AssessedCatches$TaxonKey)>=600000, ]
  AssessedCatches$penalty <- 1
  
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
                     Medianb_bmsy=quantile(as.numeric(bmsy), probs=c(0.5)), 
                     Minb_bmsy=min(as.numeric(bmsy)))
  
  UnAssessedCatches <- join(UnAssessedCatches, b_summary, by = c("fao_id", "year"),
                            type="left", match="all")
  
#  UnAssessedCatches <- UnAssessedCatches[!(is.na(UnAssessedCatches$Medianb_bmsy)), ] #added 8/21/2014 due to changes in b/bmsy data created NAs here
  
#   ## Troubleshooting:
#   head(UnAssessedCatches[is.na(UnAssessedCatches$Medianb_bmsy), ])
#   tmp <- UnAssessedCatches[is.na(UnAssessedCatches$Medianb_bmsy), ]
#   unique(tmp$fao_id)
#   saups <- unique(tmp$saup_id)
#   unique(tmp$stock_id)
#   a[a$saup_id %in% saups,]
#   
#   ggplot(tmp, aes(x=year, y=catch, group=saup_id, color=saup_id)) +
#     geom_point() +
#     geom_line() +
#     facet_wrap( ~ stock_id, ncol=9, scale="free")
#   
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
  
  AssessedCatches$score <- score(data=AssessedCatches, variable="bmsy")
  
  # Median is used to calculate score for species with Taxon 6 coding
  UnAssessedCatchesT6 <- subset(UnAssessedCatches, penalty==1)
  UnAssessedCatchesT6$score <- score(UnAssessedCatchesT6, "Medianb_bmsy")
  
  UnAssessedCatches <- subset(UnAssessedCatches, penalty!=1)
  UnAssessedCatches$score <- score(UnAssessedCatches, "Medianb_bmsy")
  
  AllScores <- rbind(AssessedCatches[,c("TaxonName", "TaxonKey", "year", "fao_id", "saup_id", "catch","score")],
                  UnAssessedCatchesT6[,c("TaxonName", "TaxonKey", "year", "fao_id", "saup_id", "catch","score")],
                  UnAssessedCatches[,c("TaxonName", "TaxonKey", "year", "fao_id", "saup_id", "catch","score")])
    
  # ------------------------------------------------------------------------
  # STEP 4. Calculate status for each saup_id region
  # -----------------------------------------------------------------------
  
  # 4a. To calculate the weight (i.e, the relative catch of each stock per saup_id),
  # the mean catch of taxon i is divided by the   
  # sum of mean catch of all species in region r, which is calculated as: 
  
  smc <- ddply(.data = AllScores, .(year, saup_id), summarize, 
               SumCatch = sum(catch))   
  AllScores<-join(AllScores,smc,by = c("year","saup_id"))  
  AllScores$wprop<-AllScores$catch/AllScores$SumCatch 
  
  
  #  4b. The "score" and "weight" values per taxon per SAUP region are used to  
  #    calculate a geometric weighted mean across taxa for each saup_id region
  geomMean <- ddply(.data = AllScores, .(saup_id, year), summarize, status_saup = prod(score^wprop)) 
  
  # ------------------------------------------------------------------------
  # STEP 5. Convert status from saup spatial scale to OHI spatial scale  
  # -----------------------------------------------------------------------
  # In many cases the ohi reporting regions are comprised of multiple saup regions.
  # To correct for this, the proportion of each saup area of the total area of the 
  # OHI region was calculated. This was used to calculate Status from the Status_saup.
  # This type of adjustment is omitted if the data were collected at the same spatial 
  # scale as the collecting region.
  
  # Join region names/ids to Geom data
  geomMean <- join(a, geomMean, type="inner", by="saup_id") # merge km2 of shelf area with status results
  
  # weighted mean scores
  StatusData <- ddply(.data = geomMean, .(rgn_id, year), summarize, Status = sum(status_saup*prop_area))
  
  # 2013 status is based on 2011 data (most recent data)
  status = StatusData %>%
    filter(year==status_year) %>%
    mutate(
      score     = round(Status*100),
      dimension = 'status') %>%
    select(region_id=rgn_id, dimension, score)
    
  # ------------------------------------------------------------------------
  # STEP 6. Calculate trend  
  # -----------------------------------------------------------------------
  trend = ddply(StatusData, .(rgn_id), function(x){
    mdl = lm(Status ~ year, data=x)
    data.frame(
      score     = round(coef(mdl)[['year']] * 5, 2),
      dimension = 'trend')}) %>%
    select(region_id=rgn_id, dimension, score)
  # %>% semi_join(status, by = 'rgn_id')
  
  # assemble dimensions
  scores = rbind(status, trend) %>% mutate(goal='FIS')
  return(scores)  
}

MAR = function(layers, status_years){  
  # layers used: mar_harvest_tonnes, mar_harvest_species, mar_sustainability_score, mar_coastalpopn_inland25mi, mar_trend_years
  harvest_tonnes = rename(
    SelectLayersData(layers, layers='mar_harvest_tonnes', narrow = TRUE),
    c('id_num'='rgn_id', 'category'='species_code', 'year'='year', 'val_num'='tonnes'))
  harvest_species = rename(
    SelectLayersData(layers, layers='mar_harvest_species', narrow = TRUE),
    c('category'='species_code', 'val_chr'='species'))
  sustainability_score = rename(
    SelectLayersData(layers, layers='mar_sustainability_score', narrow = TRUE),
    c('id_num'='rgn_id', 'category'='species', 'val_num'='sust_coeff'))
  popn_inland25mi = rename(
    SelectLayersData(layers, layers='mar_coastalpopn_inland25mi', narrow = TRUE),
    c('id_num'='rgn_id', 'year'='year', 'val_num'='popsum'))
  trend_years = rename(
    SelectLayersData(layers, layers='mar_trend_years', narrow = TRUE),
    c('id_num'='rgn_id', 'val_chr'='trend_yrs'))
  
  rky = harvest_tonnes %>%
    merge(harvest_species     , all.x = TRUE, by = 'species_code') %>%
    merge(sustainability_score, all.x = TRUE, by = c('rgn_id', 'species')) %>%
    dcast(rgn_id + species + species_code + sust_coeff ~ year, value.var='tonnes', mean, na.rm = TRUE) %>%
    arrange(rgn_id, species)
    
 # x = csv_compare(rky, '1-rky')
  
  # smooth each species-country time-series using a running mean with 4-year window, excluding NAs from the 4-year mean calculation
  # TODO: simplify below with dplyr::group_by()
  yrs_smooth <- names(rky)[!names(rky) %in% c('rgn_id','species','species_code','sust_coeff')]
  rky_smooth = zoo::rollapply(t(rky[,yrs_smooth]), 4, mean, na.rm = TRUE, partial = TRUE) 
  rownames(rky_smooth) = as.character(yrs_smooth)
  rky_smooth = t(rky_smooth)
  rky = as.data.frame(cbind(rky[, c('rgn_id','species','species_code','sust_coeff')], rky_smooth)); head(rky)
#  x = csv_compare(rky, '2-rky-smooth')  # DEBUG
    
  # melt
  m = melt(rky,
           id=c('rgn_id', 'species', 'species_code', 'sust_coeff'),
           variable.name='year', value.name='sm_tonnes'); head(m)
#   m <- m %>%
#     arrange(rgn_id, species)
#   x = csv_compare(m, '3-m-melt')  # DEBUG
  # "Component “year”: 'current' is not a factor"
    
  # for each species-country-year, smooth mariculture harvest times the sustainability coefficient
  m = within(m, {
    sust_tonnes = sust_coeff * sm_tonnes
    year        = as.numeric(as.character(m$year))
  })
#   m <- m %>%
#     arrange(rgn_id, species)
#   x = csv_compare(m, '4-m-within')  # DEBUG
  
  # merge the MAR and coastal human population data   
  m = merge(m, popn_inland25mi, by = c('rgn_id','year'), all.x = TRUE)
#   m <- m %>%
#     arrange(rgn_id, species, species_code)
#   m_a = csv_compare(m, '5-m-merge')  # DEBUG
  
  # must first aggregate all weighted timeseries per region, before dividing by total population
#   ry = ddply(m, .(rgn_id, year, popsum), summarize, 
#              sust_tonnes_sum = sum(sust_tonnes),
#              mar_pop         = sum(sust_tonnes) / popsum[1]) # <-- PROBLEM using popsum[1] with ddply!!!
  
  # aggregate all weighted timeseries per region, and divide by coastal human population
  ry = m %>%
    group_by(rgn_id, year) %>%
    summarize(
      sust_tonnes_sum = sum(sust_tonnes)) %>%
    merge(
      popn_inland25mi, by = c('rgn_id','year'), all.x = TRUE) %>%
    mutate(
      mar_pop         = sust_tonnes_sum / popsum) %>%
    select(rgn_id, year, popsum, sust_tonnes_sum, mar_pop)
#  ry_b = csv_compare(ry, '6-ry-ddply')  # RIGHT
#   ry_a = ry
#   eq = all.equal(ry_a, ry_b)
#   if (class(eq) == 'character') browser()

  
  # get reference quantile based on argument years
  ref_95pct = quantile(subset(ry, year <= max(status_years), mar_pop, drop = TRUE), 0.95, na.rm = TRUE)
#  x = csv_compare(ref_95pct, '7-ref95pct-quantile')  # DEBUG

# identify reference rgn_id
ry_ref = ry %>% 
  filter(year <=max(status_years)) %>%
  arrange(mar_pop) %>%
  filter(mar_pop >= ref_95pct)
  cat(sprintf('95th percentile rgn_id for MAR ref pt is: %s\n', ry_ref$rgn_id[1])) # rgn_id 25 = Thailand
  
  ry = within(ry, {
    status = ifelse(mar_pop / ref_95pct > 1, 
                    1,
                    mar_pop / ref_95pct)})
  status <- subset(ry, year == max(status_years), c('rgn_id', 'status'))
  status$status <- round(status$status*100, 2)  
#   x = csv_compare(ry, '8-ry-within')  # DEBUG  
  
  # get list where trend is only to be calculated up to second-to-last-year
  # species where the last year of the time-series was 2010, and the same value was copied over to 2011
  # i.e. it was gapfilled using the previous year
  
  # get MAR trend
  ry = merge(ry, trend_years, all.x = TRUE)
  yr_max = max(status_years)
  trend = ddply(ry, .(rgn_id), function(x){  # x = subset(ry, rgn_id==5)
    yrs = ifelse(x$trend_yrs=='4_yr',
                 (yr_max-5):(yr_max-1), # 4_yr
                 (yr_max-5):(yr_max))   # 5_yr
    y = subset(x, year %in% yrs & !is.na(status))
    # added condition for aus repo since rgns 7 & 9 have no data
    if (nrow(y) > 1){
      trend = round(max(min(lm(status ~ year, data=y)$coefficients[['year']] * 5, 1), -1), 2)
    } else {
      trend = NA
    } 
    return(data.frame(trend)) 
  })
  
  # return scores
  scores = status %>%
    select(region_id = rgn_id,
           score     = status) %>%
    mutate(dimension='status') %>%
    rbind(
      trend %>%
        select(region_id = rgn_id,
               score     = trend) %>%
        mutate(dimension = 'trend')) %>%
    mutate(goal='MAR')
  
  return(scores)
  # NOTE: some differences to www2013 are due to 4_yr species only previously getting trend calculated to 4 years (instead of 5)
}

FP = function(layers, scores){
  # weights
  w = rename(SelectLayersData(layers, layers='fp_wildcaught_weight', narrow = TRUE),
             c('id_num'='region_id', 'val_num'='w_FIS')); head(w)
  
  # scores
  s = dcast(scores, region_id + dimension ~ goal, value.var='score', subset=.(goal %in% c('FIS','MAR') & !dimension %in% c('pressures','resilience'))); head(s)  
  
  # combine
  d = merge(s, w)
  d$w_MAR = 1 - d$w_FIS
  d$score = apply(d[,c('FIS','MAR','w_FIS', 'w_MAR')], 1, function(x){ weighted.mean(x[1:2], x[3:4], na.rm = TRUE) })
  d$goal = 'FP'
  
  # return all scores
  return(rbind(scores, d[,c('region_id','goal','dimension','score')]))
}


AO = function(layers, 
              year_max,
              year_min=max(min(layers_data$year, na.rm = TRUE), year_max - 10), 
              Sustainability=1.0){
  
  # cast data
  layers_data = SelectLayersData(layers, targets='AO')
  
  ry = rename(dcast(layers_data, id_num + year ~ layer, value.var='val_num', 
                    subset = .(layer %in% c('ao_need'))),
              c('id_num'='region_id', 'ao_need'='need')); head(ry); summary(ry)
  
  r = na.omit(rename(dcast(layers_data, id_num ~ layer, value.var='val_num', 
                           subset = .(layer %in% c('ao_access'))),
                     c('id_num'='region_id', 'ao_access'='access'))); head(r); summary(r)
  
  ry = merge(ry, r); head(r); summary(r); dim(r)
  
  # model
  ry = within(ry,{
    Du = (1.0 - need) * (1.0 - access)
    statusData = ((1.0 - Du) * Sustainability)
  })
  
  # status
  r.status <- ry %>%
    filter(year==year_max) %>%
    select(region_id, statusData) %>%
    mutate(status=statusData*100)
summary(r.status); dim(r.status)
  
  # trend
  r.trend = ddply(subset(ry, year >= year_min), .(region_id), function(x)
    {
      if (length(na.omit(x$statusData))>1) {
        # use only last valid 5 years worth of status data since year_min
        d = data.frame(statusData=x$statusData, year=x$year)[tail(which(!is.na(x$statusData)), 5),]
        trend = coef(lm(statusData ~ year, d))[['year']]*5
      } else {
        trend = NA
      }
      return(data.frame(trend=trend))
    })
  
  # return scores
  scores = r.status %>%
    select(region_id, score=status) %>%
    mutate(dimension='status') %>%
    rbind(
      r.trend %>%
        select(region_id, score=trend) %>%
        mutate(dimension='trend')) %>%
    mutate(goal='AO') # dlply(scores, .(dimension), summary)
  return(scores)  
}

NP <- function(scores, layers, year_max, debug = FALSE){
  ### Apr 2014: updated by @oharac to use dplyr, tidyr.
  # TODO: add smoothing a la PLoS 2013 manuscript # ??? CCO: done? is this the NP data_prep smoothing?

  ### new code version - load combined harvest variables
  r_cyanide    = layers$data[['np_cyanide']]
  r_blast      = layers$data[['np_blast']]  
  hab_extent   = layers$data[['hab_extent']]

  ### FIS status for fish oil exposure
  FIS_status   <-  scores %>% 
    filter(goal == 'FIS' & dimension == 'status') %>%
    select(rgn_id = region_id, score)  


  ###########################################################.
  ### Here I define five main sub-functions.  The main script that
  ### actually calls these functions is at the very end of the NP section.
  ###   np_rebuild_harvest
  ###   np_calc_exposure
  ###   np_calc_risk
  ###   np_calc_sustainability
  ###   np_calc_scores

  np_rebuild_harvest <- function(layers) {
    ### Reassembles NP harvest information from separate data layers:
    ### [rgn_name  rgn_id  product  year  tonnes  tonnes_rel  prod_weight]
    #########################################.

    ## load data from layers dataframe
      rgns         <- layers$data[['rgn_labels']]
      h_tonnes     <- layers$data[['np_harvest_tonnes']]
      h_tonnes_rel <- layers$data[['np_harvest_tonnes_relative']]
      h_w          <- layers$data[['np_harvest_product_weight']]
    
    # merge harvest in tonnes and usd
    np_harvest <- h_tonnes %>%
      full_join(
        h_tonnes_rel,
        by=c('rgn_id', 'product', 'year')) %>%
          left_join(
            h_w %>%
              select(rgn_id, product, prod_weight = weight),
            by=c('rgn_id', 'product')) %>%
          left_join(
            rgns %>%
              select(rgn_id, rgn_name=label),
            by='rgn_id') %>%
          select(
            rgn_name, rgn_id, product, year, 
            tonnes, tonnes_rel, prod_weight) %>%
          group_by(rgn_id, product)
        
    return(np_harvest)
  }


  np_calc_exposure <- function(np_harvest, hab_extent, FIS_status) {
    ### calculates NP exposure based on habitats (for corals, seaweeds, 
    ### ornamentals, shells, sponges) and FIS status scores (for fish oil).
    ### Returns the first input data frame with a new column for exposure:
    ### [rgn_id rgn_name product year tonnes tonnes_rel prod_weight exposure]
    #########################################.

    ### Determine Habitat Areas for Exposure
    ### extract habitats used
    hab_coral <- hab_extent %>%
      filter(habitat == 'coral') %>%
      select(rgn_id, km2)
    hab_rocky   <- hab_extent %>%
      filter(habitat == 'rocky_reef') %>%
      select(rgn_id, km2)
    
    ### area for products having single habitats for exposure
    area_single_hab <- bind_rows(
      # corals in coral reef
        np_harvest %>%
        filter(product == 'corals') %>%
        left_join(
          hab_coral %>%
            filter(km2 > 0) %>%
            select(rgn_id, km2), by = 'rgn_id'),
      ### seaweeds in rocky reef
      np_harvest %>%
        filter(product == 'seaweeds') %>%
        left_join(
          hab_rocky %>%
            filter(km2 > 0) %>%
            select(rgn_id, km2), by = 'rgn_id'))
    
    ### area for products in both coral and rocky reef habitats: shells, ornamentals, sponges
    area_dual_hab <- np_harvest %>%
      filter(product %in% c('shells', 'ornamentals','sponges')) %>%
      left_join(
        hab_coral %>%
          filter(km2 > 0) %>%
          select(rgn_id, coral_km2 = km2), 
        by = 'rgn_id') %>%
      left_join(
        hab_rocky %>%
          filter(km2 > 0) %>%
          select(rgn_id, rocky_km2 = km2), 
        by = 'rgn_id') %>%
      rowwise() %>%
      mutate(
        km2 = sum(c(rocky_km2, coral_km2), na.rm = TRUE)) %>%
      filter(km2 > 0)
    
    ### Determine Exposure
    ### exposure: combine areas, get tonnes / area, and rescale with log transform
    np_exp <- 
      bind_rows(
        area_single_hab,
        area_dual_hab %>%
          select(-rocky_km2, -coral_km2)) %>%
      mutate(
        expos_raw = ifelse(tonnes > 0 & km2 > 0, (tonnes / km2), 0)) %>%
      group_by(product) %>%
      mutate(
        expos_prod_max = (1 - .35)*max(expos_raw, na.rm = TRUE)) %>%
          ### Reduced max exposure:
          ###   .35 is the threshold for harvest peak used in status
      ungroup() %>%
      mutate(
        exposure = (log(expos_raw + 1) / log(expos_prod_max + 1)),
        exposure = ifelse(exposure > 1, 1, exposure)) %>%
      select(-km2, -expos_raw, -expos_prod_max)
        ### clean up columns
      
    ### add exposure for countries with (habitat extent == NA)
    np_exp <- np_exp %>% 
      group_by(product) %>%
      mutate(mean_exp = mean(exposure, na.rm = TRUE)) %>%
      mutate(exposure = ifelse(is.na(exposure), mean_exp, exposure)) %>%
      select(-mean_exp) %>%
      ungroup()
    
    ### add exposure for fish_oil
    np_exp <- np_exp %>% bind_rows(
        np_harvest %>%
          filter(product=='fish_oil') %>%
          left_join(
            FIS_status %>%
              mutate(exposure = score / 100) %>%
#              mutate(exposure = ifelse(is.na(exposure), 0, exposure)) %>% 
        ### ??? adding this ^^^ from below - now will filter only NAs in fish_oil exposure, not seaweeds and coral exposure
              select(rgn_id, exposure),
            by = 'rgn_id'))
    
    # ??? CCO: This assigns exposure to zero for ANY product with NA (fish oil, seaweeds, corals)
#     np_exp <- np_exp %>% 
 #      mutate(exposure = ifelse(is.na(exposure), 0, exposure))
    
    return(np_exp)
  }
  
  np_calc_risk <- function(np_exp, r_cyanide, r_blast) {
    ### calculates NP risk based on:
    ###   ornamentals:      risk = 1 if blast or cyanide fishing
    ###   corals:           risk = 1 for all cases
    ###   shells, sponges:  risk = 0 for all cases
    ###   others:           risk = NA?
    ### Returns a data frame of risk, by product, by region:
    ### 
    #########################################.

    ### Determine Risk
    
    ### risk for ornamentals set to 1 if blast or cyanide fishing present, based on Nature 2012 code
    ###  despite Nature 2012 Suppl saying Risk for ornamental fish is set to the "relative intensity of cyanide fishing"
    risk_orn <- r_cyanide %>%
      filter(!is.na(score) & score > 0) %>%
      select(rgn_id, cyanide = score) %>%  
      merge(
        r_blast %>%
          filter(!is.na(score) & score > 0) %>%
          select(rgn_id, blast = score),
        all = TRUE) %>%
      mutate(ornamentals = 1)
    
    ### risk as binary
    np_risk <- 
      ### fixed risk: corals (1), sponges (0) and shells (0)
      data.frame(
        rgn_id  = unique(np_harvest$rgn_id),
        corals  = 1,
        sponges = 0,
        shells  = 0) %>%  
      ### ornamentals
      left_join(
        risk_orn %>%
          select(rgn_id, ornamentals),
        by = 'rgn_id')  %>%
      mutate(
        ornamentals = ifelse(is.na(ornamentals), 0, ornamentals)) %>%
      gather(product, risk, -rgn_id)
    return(np_risk)
  }

  np_calc_sustainability <- function(np_exp, np_risk) {
    ### calculates NP sustainability coefficient for each natural product, based
    ### on (1 - mean(c(exposure, risk))).  Returns first input dataframe with
    ### new columns for sustainability coefficient, and sustainability-adjusted
    ### NP product_status:
    ### [rgn_id  rgn_name  product  year  prod_weight  sustainability  product_status]
    #########################################.
    
    ### join Exposure (with harvest) and Risk
    np_sust <- np_exp %>%
      left_join(
        np_risk,
        by = c('rgn_id', 'product')) %>%
      rowwise() %>% 
      mutate(sustainability = 1 - mean(c(exposure, risk), na.rm = TRUE))
    
    ### calculate rgn-product-year status
    np_sust <- np_sust %>% 
      mutate(product_status = tonnes_rel * sustainability) %>%
      filter(rgn_name != 'DISPUTED') %>%
      select(-tonnes, -tonnes_rel, -risk, -exposure)
    
    return(np_sust)
  }

  np_calc_scores <- function(np_sust, year_max) {
    ### Calculates NP status for all production years for each region, based 
    ### upon weighted mean of all products produced. 
    ### From this, reports the most recent year as the NP status.
    ### Calculates NP trend for each region, based upon slope of a linear
    ### model over the past six years inclusive (five one-year intervals).
    ### Returns data frame with status and trend by region:
    ### [goal   dimension   region_id   score]
    #########################################.
    
    ### Calculate status, trends
    ### aggregate across products to rgn-year status, weighting by usd_rel
    np_status_all <- np_sust %>%
      filter(!is.na(product_status) & !is.na(prod_weight)) %>%
        ### ??? CCO: guadeloupe & martinique have NA for ornamental prod_weight.  Is this a gap-filling error?
      select(rgn_name, rgn_id, year, product, product_status, prod_weight) %>%
      group_by(rgn_id, year) %>%
      summarize(status = weighted.mean(product_status, prod_weight)) %>%
      filter(!is.na(status)) %>% # 1/0 produces NaN
      ungroup()
  
    #   if (debug){
    #     ### write out data
    #     write.csv(np_risk, sprintf('temp/%s_NP_2-rgn-year-product_data.csv', basename(getwd())), row.names = FALSE, na='')
    #     write.csv(status_all, sprintf('temp/%s_NP_3-rgn-year_status.csv', basename(getwd())), row.names = FALSE, na='')
    #   }

    ### get current status
    np_status_current <- np_status_all %>%
      filter(year == year_max & !is.na(status)) %>%
      mutate(
        dimension = 'status',
        score     = round(status,4) * 100) %>%
      select(rgn_id, dimension, score)
    stopifnot(
      min(np_status_current$score, na.rm = TRUE) >= 0, 
      max(np_status_current$score, na.rm = TRUE) <= 100)
    
    ### trend based on 5 intervals (6 years of data)
    np_trend <- np_status_all %>%
      filter(year <= year_max & year > (year_max - 5) & !is.na(status)) %>%
      group_by(rgn_id) %>%
      do(mdl = lm(status ~ year, data=.)) %>%
      summarize(
        rgn_id    = rgn_id,
        dimension = 'trend',
        score     = max(-1, min(1, coef(mdl)[['year']] * 5)))
    stopifnot(min(np_trend$score) >= -1, max(np_trend$score) <= 1)
    
    ### return scores
    np_scores <- np_status_current %>%
      full_join(np_trend) %>%
      mutate(goal = 'NP') %>%
      select(goal, dimension, region_id=rgn_id, score) %>%
      arrange(goal, dimension, region_id)
    
    return(np_scores)
  }

  ##########################################.
  ### Natural Products main starts here:

  np_harvest <- np_rebuild_harvest(layers)  
  np_exp     <- np_calc_exposure(np_harvest, hab_extent, FIS_status) 
  np_risk    <- np_calc_risk(np_exp, r_cyanide, r_blast)
  np_sust    <- np_calc_sustainability(np_exp, np_risk)
  np_scores  <- np_calc_scores(np_sust, year_max) 
        
  return(np_scores)
}


  CS <- function(layers){
    # join layer data
    d <- 
      plyr::join_all(
        list(
          layers$data[['hab_extent']] %>%
            select(rgn_id, habitat, km2), 
          
          layers$data[['hab_health']] %>%
            select(rgn_id, habitat, health),
          
          layers$data[['hab_trend']] %>%
            select(rgn_id, habitat, trend)),
        
        by = c('rgn_id','habitat'), type='full') %>% 
      select(rgn_id, habitat, km2, health, trend) %>%
      mutate(habitat = as.character(habitat))
    
    # limit to CP habitats and add rank
    habitat.rank <- c('mangrove'         = 139,
                      'saltmarsh'        = 210,
                      'seagrass'         = 83)
    
    d <- d %>%
      filter(habitat %in% names(habitat.rank)) %>%
      mutate(
        rank = habitat.rank[habitat],
        extent = ifelse(km2==0, NA, km2))
    
    if (nrow(d) > 0){
      # status
      scores_CS <- d %>%
        filter(!is.na(rank) & !is.na(health) & !is.na(extent)) %>%
        group_by(rgn_id) %>%
        summarize(
          score = pmin(1, sum(rank * health * extent, na.rm=TRUE) / (sum(extent * rank, na.rm=TRUE)) ) * 100,
          dimension = 'status')
      
      # trend
      d_trend <- d %>%
        filter(!is.na(rank) & !is.na(trend) & !is.na(extent))
      if (nrow(d_trend) > 0 ){
        scores_CS <- rbind_list(
          scores_CS,
          d_trend %>%
            group_by(rgn_id) %>%
            summarize(
              score = sum(rank * trend * extent, na.rm=TRUE) / (sum(extent*rank, na.rm=TRUE)),
              dimension = 'trend'))
      }
      
      ### output data file for checking and data review
      scores_check <- spread(scores_CS, dimension, score) %>%
        select(rgn_id, status, trend_score=trend)
      
      d_check <- d %>%
        select(rgn_id, habitat, extent, health, trend, rank) %>%
        arrange(rgn_id, habitat) %>%
        left_join(scores_check, by="rgn_id") 
      write.csv(d_check, sprintf('temp/CS_data_%s.csv', scenario), row.names=FALSE)    
      ### end: output...   
      
      scores_CS <- scores_CS %>%
        mutate(
          goal = 'CS') %>%
        select(region_id=rgn_id, goal, dimension, score)
    } else {
      scores_CS <- data.frame(
        goal      = character(0),
        dimension = character(0),
        region_id = integer(0),
        score     = numeric())
    }
    
    # return scores
    return(scores_CS)
  }  



CP <- function(layers){
  # sum mangrove_offshore1km + mangrove_inland1km = mangrove to match with extent and trend
  m <- layers$data[['hab_extent']] %>%
    filter(habitat %in% c('mangrove_inland1km','mangrove_offshore1km')) %>%
    select(rgn_id, habitat, km2)
  
  if (nrow(m) > 0){
    m <- m %>%
      group_by(rgn_id) %>%
      summarize(km2 = sum(km2, na.rm = TRUE)) %>%
      mutate(habitat='mangrove') %>%
      ungroup() 
  }
  
  # join layer data
  d <- 
    plyr::join_all(
      list(
        layers$data[['hab_health']] %>%
          select(rgn_id, habitat, health),
        
        layers$data[['hab_trend']] %>%
          select(rgn_id, habitat, trend),
        
        # for habitat extent
        rbind_list(
          
          # do not use all mangrove
          layers$data[['hab_extent']] %>%
            filter(!habitat %in% c('mangrove','mangrove_inland1km','mangrove_offshore1km')) %>%
            select(rgn_id, habitat, km2),
          
          # just use inland1km and offshore1km
          m)),
      
      by = c('rgn_id','habitat'), type='full') %>% 
    select(rgn_id, habitat, km2, health, trend)
  
  # limit to CP habitats and add rank
  habitat.rank <- c('coral'            = 4,
                    'mangrove'         = 4,
                    'saltmarsh'        = 3,
                    'seagrass'         = 1,
                    'seaice_shoreline' = 4)
  
  d <- d %>%
    filter(habitat %in% names(habitat.rank)) %>%
    mutate(
      rank = habitat.rank[habitat],
      extent = ifelse(km2==0, NA, km2))
  
  if (nrow(d) > 0){
    # status
    scores_CP <- d %>%
      filter(!is.na(rank) & !is.na(health) & !is.na(extent)) %>%
      group_by(rgn_id) %>%
      summarize(
        score = pmin(1, sum(rank * health * extent, na.rm=TRUE) / (sum(extent * rank, na.rm=TRUE)) ) * 100,
        dimension = 'status')
    
    # trend
    d_trend <- d %>%
      filter(!is.na(rank) & !is.na(trend) & !is.na(extent))
    if (nrow(d_trend) > 0 ){
      scores_CP <- rbind_list(
        scores_CP,
        d_trend %>%
          group_by(rgn_id) %>%
          summarize(
            score = sum(rank * trend * extent, na.rm=TRUE) / (sum(extent*rank, na.rm=TRUE)),
            dimension = 'trend'))
    }
    
    ### output data file for checking and data review
    scores_check <- spread(scores_CP, dimension, score) %>%
      select(rgn_id, status, trend_score=trend)
    
    d_check <- d %>%
      select(rgn_id, habitat, extent, health, trend, rank) %>%
      arrange(rgn_id, habitat) %>%
      left_join(scores_check, by="rgn_id") 
    write.csv(d_check, sprintf('temp/CP_data_%s.csv', scenario), row.names=FALSE)    
    ### end: output...   
    
    scores_CP <- scores_CP %>%
      mutate(
        goal = 'CP') %>%
      select(region_id=rgn_id, goal, dimension, score)
  } else {
    scores_CP <- data.frame(
      goal      = character(0),
      dimension = character(0),
      region_id = integer(0),
      score     = numeric())
  }
  
  # return scores
  return(scores_CP)
}


TR = function(layers, year_max, debug = FALSE, pct_ref = 90) {
  ### Updated July 2015 - Casey O'Hara
  ### * adjusted model to use percent employment in tourism when available.
  ### * set up layers for travel warnings.
  
  #Inputs:
  # * U  = tr_unemployment.csv:     Percent unemployment (0-100%)
  # * S_score  = tr_sustainability.csv:   TTCI score, not normalized (1-7)
  # * Ed = tr_jobs_tourism.csv:     Number of jobs, direct employment in tourism
  # * Ep = tr_jobs_pct_tourism.csv: Percent of direct tourism jobs
  # * L  = tr_jobs_total.csv:       Total labor force
  # formula:
  #  E       = ifelse(is.na(Ep), Ed / (L - (L * U)), Ep)    # Ep is direct percentage of labor in tourism; if not available, calc the hard way
  #  S       = (S_score - 1) / (7 - 1)                      # S_score is raw score (from 1:7).  Subtract 1 and normalize.
  #  Xtr     = E * S 
  
  # get regions
  rgn_names = layers$data[[conf$config$layer_region_labels]] %>%
    select(rgn_id, rgn_name = label)   
  
  tr_data  <- layers$data[['tr_jobs_tourism']]     %>% 
    select(-layer) %>%
    full_join(layers$data[['tr_jobs_pct_tourism']] %>% 
                select(-layer),
              by = c('rgn_id','year'))             %>%
    full_join(layers$data[['tr_unemployment']]     %>% 
                select(-layer),
              by = c('rgn_id','year'))             %>%
    full_join(layers$data[['tr_jobs_total']]       %>% 
                select(-layer),
              by = c('rgn_id','year'))             %>%
    full_join(layers$data[['tr_sustainability']]   %>% 
                select(-layer),
              by = c('rgn_id'))                    %>%
    full_join(rgn_names, by = 'rgn_id')            %>%
    filter(year <= year_max)
  
  tr_model <- tr_data %>%
    mutate(
      E   = ifelse(is.na(Ep), Ed / (L - (L * U)), Ep),
      S   = (S_score - 1) / (7 - 1), # scale score from 1 to 7.
      Xtr = E * S ) %>%  
    filter(year <= year_max & year > year_max - 5) 
  # five data years, four intervals
  
  # regions with Travel Warnings at http://travel.state.gov/content/passports/english/alertswarnings.html
  rgn_travel_warnings <- layers$data[['tr_travelwarnings']] %>%
    select(rgn_name, multiplier) %>%
    left_join(rgn_names, by = 'rgn_name') %>%
    filter(!is.na(rgn_id))
  
  tr_model <- tr_model %>%
    left_join(rgn_travel_warnings %>% 
                select(-rgn_name), 
              by = 'rgn_id') %>%
    mutate(Xtr = ifelse(!is.na(multiplier), multiplier * Xtr, Xtr)) %>%
    select(-multiplier)
  
  ### Calculate status based on quantile reference (see function call for pct_ref)
  tr_model <- tr_model %>%
    select(rgn_id, rgn_name, year, Xtr) %>%
    left_join(tr_model %>%
                group_by(year) %>%
                summarize(Xtr_q = quantile(Xtr, probs = pct_ref/100, na.rm = TRUE)),
              by = 'year') %>%
    mutate(
      Xtr_rq  = ifelse(Xtr / Xtr_q > 1, 1, Xtr / Xtr_q)) # rescale to qth percentile, cap at 1
  
  
  # calculate trend
  tr_trend <- tr_model %>%
    filter(!is.na(Xtr_rq)) %>%
    arrange(year, rgn_id) %>%
    group_by(rgn_id) %>%
    do(mod = lm(Xtr_rq ~ year, data = .)) %>%
    do(data.frame(
      rgn_id    = .$rgn_id,
      dimension = 'trend',
      score     =  max(min(coef(.$mod)[['year']] * 5, 1), -1)))
  
  # get status (as last year's value)
  tr_status <- tr_model %>%
    arrange(year, rgn_id) %>%
    group_by(rgn_id) %>%
    summarize(
      dimension = 'status',
      score     = last(Xtr_rq) * 100)
  
  # bind status and trend by rows
  tr_score <- bind_rows(tr_status, tr_trend) %>%
    mutate(goal = 'TR')  
  
  if (conf$config$layer_region_labels=='rgn_global'){
    # assign NA for uninhabitated islands
    unpopulated = layers$data[['le_popn']] %>%
      group_by(rgn_id) %>%
      filter(count==0) %>%
      select(rgn_id)
    tr_score$score = ifelse(tr_score$rgn_id %in% unpopulated$rgn_id, NA, tr_score$score)  
    
    #     # replace North Korea value with 0
    #     tr_score$score[tr_score$rgn_id == 21] = 0
  }
  
  # return final scores
  scores = tr_score %>%
    select(region_id=rgn_id, goal, dimension, score)
  
  return(scores)
}
LIV_ECO = function(layers, subgoal, liv_workforcesize_year, eco_rev_adj_min_year){

  g.component = c('LIV'='livelihood','ECO'='economy')[[subgoal]]
  
  # get status_model
  status_model_long = SelectLayersData(
    layers, narrow = TRUE,
    layers=c('le_jobs_cur_base_value','le_jobs_ref_base_value','le_jobs_cur_adj_value','le_jobs_ref_adj_value',
             'le_rev_cur_base_value','le_rev_ref_base_value','le_rev_cur_adj_value','le_rev_ref_adj_value',
             'le_wage_cur_base_value','le_wage_ref_base_value','le_wage_cur_adj_value','le_wage_ref_adj_value'))
  status_model = status_model_long %>%
    select(cntry_key = id_chr, sector = category, val_num, layer) %>%
    mutate(metric = str_replace(layer, 'le_(jobs|rev|wage)_(.*)', '\\1'),
           field  = str_replace(layer, 'le_(jobs|rev|wage)_(.*)', '\\2')) %>% 
    dcast(metric + cntry_key + sector ~ field, value.var='val_num')
  
  # get gdp per capita, at ppp
  ppp = SelectLayersData(layers, layers='le_gdp_pc_ppp') %>%
    select(cntry_key=id_chr, year, usd=val_num)
  
  # country to region aggregation weight for livelihood
  workforce_adj = SelectLayersData(layers, layers='le_workforcesize_adj') %>%
    select(cntry_key=id_chr, year, jobs=val_num)
  
  # country to region aggregation weight for economy
  rev_adj = SelectLayersData(layers, layers='le_revenue_adj') %>%
    select(cntry_key=id_chr, year, usd=val_num)
  
  # compute the corrected relative value per metric per country, for JOBS
  status_jobs_rev = status_model %>%
    filter(ref_base_value != 0 & ref_adj_value != 0 & metric %in% c('jobs', 'rev')) %>%
    group_by(metric, cntry_key) %>%
    summarise(
      score    = (sum(cur_base_value, na.rm = TRUE) / sum(ref_base_value, na.rm = TRUE)) / (mean(cur_adj_value, na.rm = TRUE) / mean(ref_adj_value, na.rm = TRUE)),
      n_sector = n()) %>%
    arrange(metric, cntry_key)
  
  # compute the corrected relative value per metric per country, for WAGE
  # 0. extract w'_i = (w_c/w_r)/(W_c/W_r) for each sector i per country
  t0 = status_model %>%
    filter(metric=='wage' & ref_base_value != 0 & ref_adj_value != 0) %>%
    mutate(w_prime_i = (cur_base_value / ref_base_value) / (cur_adj_value / ref_adj_value)) %>%
    select(metric, cntry_key, sector, w_prime_i) %>%
    group_by(metric, cntry_key) %>%
    summarise(w_prime  = mean(w_prime_i, na.rm = TRUE),
              n_sector = n()) %>%
    arrange(metric, cntry_key)
  
  # 1. let w' = unweighted mean(w'_i) across all sector i per country
  # 2. multiple w' by the most recent purchasing power parity (PPP) value for the country  
  p = ppp %>%
    arrange(cntry_key, year) %>%
    group_by(cntry_key) %>%
    summarise(year     = last(year),
              ppp_last = last(usd)) %>%
    filter(!is.na(ppp_last)) %>%
    arrange(cntry_key)
  t2 = t0 %>%
    merge(p, by = 'cntry_key') %>%
    mutate(score = w_prime * ppp_last) %>%
    select(metric, cntry_key, score, n_sector) %>%
    arrange(metric, cntry_key)
  
  # 3. set the best country (PPP-adjusted average wage) equal to 1.0 and then rescale all countries to that max
  max_wage_score = max(t2$score, na.rm = TRUE)
  status_wage = t2 %>%
    mutate(score = score / max_wage_score)
  
  # combine the corrected relative values into a single status score
  status_model_combined = ungroup(status_jobs_rev) %>%
    rbind(status_wage)
  status_score = status_model_combined %>%
    # liv
    dcast(cntry_key ~ metric, value.var='score') %>%
    group_by(cntry_key) %>%
    mutate(
      value     = mean(c(jobs, wage), na.rm = TRUE),
      component = 'livelihood') %>%
    select(cntry_key, component, value) %>%
    ungroup() %>% 
    arrange(cntry_key, component, value) %>%
    # eco
    rbind(status_model_combined %>%
            filter(metric=='rev') %>%
            mutate(
              value     = score,
              component = 'economy') %>%
            select(cntry_key, component, value)) %>%
    # order
    filter(!is.na(value)) %>%
    arrange(cntry_key, component) %>%
    # clamp
    mutate(score = pmin(value, 1))
  
  # countries to regions
  cntry_rgn = layers$data[['cntry_rgn']] %>%
    select(rgn_id, cntry_key) %>%
    merge(
      SelectLayersData(layers, layers='rgn_labels') %>%
        select(rgn_id=id_num, rgn_name=val_chr),
      by = 'rgn_id', all.x = TRUE) %>%
    arrange(rgn_name, cntry_key) %>%
    select(rgn_id, rgn_name, cntry_key)

  if (conf$config$layer_region_labels=='rgn_global') {
    # update country to region lookups
    # TODO: use name_to_rgn
    suppressWarnings({ # Warning message: In rbind_list__impl(environment()) : Unequal factor levels: coercing to character
      cntry_rgn = cntry_rgn %>%
        mutate(
          cntry_key = revalue(cntry_key, c(
            'SCG'            = 'MNE',  # used to be Serbia (no coast) and Montenegro (has coast) in Nature 2012
            'Aruba'          = 'ABW',  # ABW and ANT EEZs got split...
            'Bonaire'        = 'ANT',
            'Curacao'        = 'ANT',
            'Sint Eustatius' = 'ANT',
            'Saba'           = 'ANT',
            'Brunei'         = 'BRN',  # Brunei new country in Malaysia
            'Malaysia'       = 'MYS'))) %>%
        rbind_list(
          data.frame(rgn_id=221, rgn_name='Northern Saint-Martin', cntry_key='BLM'),  # BLM is Saint Barthélemy, included within Northern Saint-Martin (MAF)
          data.frame(rgn_id=209, rgn_name=                'China', cntry_key='HKG'),  # add Hong Kong to China (CHN)
          data.frame(rgn_id=209, rgn_name=                'China', cntry_key='MAC'))  # add Macau to China (CHN)
    })
    cntry_landlocked = c('BDI','BOL','BWA','CHE','LUX','MKD','MWI','PRY','PSE','SCG','SVK','SWZ','TKM','UGA','ZMB')
    
    # remove landlocked countries and check for any country codes still not matched
    status_score = filter(status_score, !cntry_key %in% cntry_landlocked)
    if (sum(!status_score$cntry_key %in% cntry_rgn$cntry_key) != 0){
      stop(sprintf('LIV_ECO status missing country to region lookup for: %s.', paste(setdiff(status_score$cntry_key, cntry_rgn$cntry_key), collapse=', ')))
    }
  }

  # get weights, for 1) aggregating to regions and 2) georegionally gap filling
  weights = workforce_adj %>%
    filter(year==liv_workforcesize_year) %>%
    select(cntry_key, w=jobs) %>%
    mutate(component='livelihood') %>%
    rbind(
      rev_adj %>%
        select(cntry_key, year, w=usd) %>%
        filter(year >= eco_rev_adj_min_year) %>%
        arrange(cntry_key, year) %>%
        group_by(cntry_key) %>%
        summarize(w=last(w)) %>%
        mutate(component='economy'))
  
  # aggregate countries to regions by weights
  s_r = status_score %>%
    merge(cntry_rgn, by = 'cntry_key', all.x = TRUE) %>%
    merge(weights, by = c('cntry_key','component'), all.x = TRUE) %>%
    select(component, rgn_id, rgn_name, cntry_key, score, w) %>%
    arrange(component, rgn_name, cntry_key) %>%
    group_by(component, rgn_id, rgn_name) %>%
    summarize(cntry_w     = paste(cntry_key[!is.na(w)], collapse=','),
              cntry_w_na  = paste(cntry_key[is.na(w)], collapse=','),
              n           = n(),
              n_w_na      = sum(is.na(w)),
              score_w_avg = weighted.mean(score, w),
              score_avg   = mean(score),
              w_sum       = sum(w, na.rm = TRUE)) %>%
    mutate(score = ifelse(!is.na(score_w_avg), score_w_avg, score_avg)) %>%
    ungroup()
  #print(filter(s_r, n>1) %>% as.data.frame())
  # 2013:
  #    component rgn_id                                            rgn_name           cntry_w cntry_w_na n n_w_na score_w_avg score_avg        w_sum     score
  # 1    economy    116 Puerto Rico and Virgin Islands of the United States           PRI,VIR            2      0   0.8764356 0.4977907 1.012454e+11 0.8764356
  # 2    economy    140                           Guadeloupe and Martinique           GLP,MTQ            2      0   0.3550632 0.3572914 1.876348e+10 0.3550632
  # 3    economy    163                                       United States USA,Alaska,Hawaii            3      0   0.9982232 0.9437773 1.499130e+13 0.9982232
  # 4    economy    209                                               China               CHN        HKG 2      1          NA 0.9925451 7.603540e+12 0.9925451
  # 5    economy    224                                               Chile CHL,Easter Island            2      0   1.0000000 1.0000000 2.485850e+11 1.0000000
  # 6 livelihood    116 Puerto Rico and Virgin Islands of the United States           PRI,VIR            2      0   0.5212928 0.5484682 1.508586e+06 0.5212928
  # 7 livelihood    140                           Guadeloupe and Martinique                      GLP,MTQ 2      2          NA 0.9650846 0.000000e+00 0.9650846
  # 8 livelihood    209                                               China           CHN,HKG            2      0   0.7381191 0.8684414 7.868545e+08 0.7381191
  #s_r = select(s_r, component, region_id=rgn_id, rgn_name, score, w=w_sum) %>% head()
  
  # setup georegions gapfill by region
  georegions = SelectLayersData(layers, layers='rgn_georegions') %>%
    select(rgn_id=id_num, level=category, georgn_id=val_num) %>%
    dcast(rgn_id ~ level, value.var='georgn_id')

  data = s_r %>%
    filter(component==g.component) %>%
    as.data.frame() %>%
    select(rgn_id, score, w_sum)
  
  # georegional gap fill ----
  if (conf$config$layer_region_labels=='rgn_global') {
    
    # georegional gapfill, and output gapfill_georegions attributes
    if (!file.exists('temp')) dir.create('temp', recursive = TRUE)
    csv = sprintf('temp/eez2013_%s-status-gapfill-georegions.csv', subgoal)
    s_r_g = gapfill_georegions(
      data              = data,
      fld_id            = 'rgn_id',
      fld_value         = 'score',
      fld_weight        = 'w_sum',
      georegions        = georegions,    
      ratio_weights     = FALSE,
      georegion_labels  = NULL,
      r0_to_NA          = TRUE, 
      attributes_csv    = csv)
  } else {
    s_r_g = data
  }
  
  status = s_r_g %>% 
    select(region_id=rgn_id, score) %>%    
    mutate(
      goal      = subgoal,
      dimension = 'status',
      score     = score * 100) %>%
    arrange(region_id)
     
  # trend layers ----
  le_unemployment     = layers$data[['le_unemployment']]
  le_gdp              = layers$data[['le_gdp']]
  le_jobs_sector_year = layers$data[['le_jobs_sector_year']]
  le_rev_sector_year  = layers$data[['le_rev_sector_year']]
  le_wage_sector_year = layers$data[['le_wage_sector_year']]
  
  suppressWarnings({ # Warning message: In rbind_list__impl(environment()) : Unequal factor levels: coercing to character
    #browser()
    
    # adjustments
    adjustments = rbind_list(
      le_unemployment %>% 
        mutate(
          metric='jobs',
          value = 100 - percent) %>%
        select(metric, cntry_key, year, value),
      le_gdp %>% 
        mutate(
          metric='rev') %>%
        select(metric, cntry_key, year, value=usd))
    
    # metric-country-sector-year
    mcsy = rbind_list(
      le_jobs_sector_year %>%
        mutate(metric='jobs'),
      le_rev_sector_year %>%
        mutate(metric='rev'),
      le_wage_sector_year %>%
        mutate(metric='wage')) %>%
      select(metric, cntry_key, sector, year, value)

  })
  
  # merge metric-country-sector-year with adjustments
  mcsy = mcsy %>% 
    select(metric, cntry_key, sector, year, base_value=value) %>%
    left_join(
      adjustments %>% 
        select(metric, cntry_key, year, adj_value=value),
      by = c('metric','cntry_key','year')) %>%
    mutate(
      adj_value = ifelse(metric=='wage', 1, adj_value),
      value = base_value / adj_value) %>% 
    arrange(metric, cntry_key, year)
  
  # trend per metric-country-sector, based on 5 intervals (6 years of data) 
  mcs = 
    mcsy %>%
      # for clip-n-ship where cntry_key is one value, drops factor to integer so adding this bit
      mutate(
        cntry_key = as.character(cntry_key)) %>%
      filter(!is.na(value)) %>%
      group_by(metric, cntry_key, sector) %>%
      do(mdl = lm(value ~ year, data=.)) %>%
      summarize(
        metric    = metric,
        cntry_key = cntry_key,
        sector    = sector,
        trend     = max(-1, min(1, coef(mdl)[['year']] * 5))) %>%
    # get sums for weight
    left_join(
      mcsy %>%
        filter(!is.na(value)) %>%
        group_by(metric, cntry_key, sector) %>%
        summarize(
          value_sum = sum(value)),
      by = c('metric','cntry_key','sector'))
  
  # trend per metric-country 
  mc = rbind_list(
    # wage: simple average of sectors
    mcs %>%
      group_by(metric, cntry_key) %>%
      filter(metric=='wage') %>%
      summarize(
        trend = mean(trend)),
    # jobs and rev: weighted average by total jobs or rev per sector
    mcs %>%
      group_by(metric, cntry_key) %>%
      filter(metric %in% c('jobs','rev')) %>%
      summarize(
        trend = weighted.mean(trend, value_sum)))

  # trend per goal-country
  gc = rbind_list(
    # LIV: avg(jobs, wage)
    mc %>%
      group_by(cntry_key) %>%
      filter(metric %in% c('jobs','wage') & !is.na(trend)) %>%
      summarize(
        score     = mean(trend),
        component = 'livelihood'),
    # ECO: rev
    mc %>%
      filter(metric %in% c('rev')) %>%
      mutate(
        component = 'economy',
        score     = trend)) %>%
    select(component, cntry_key, score)
  
  # remove landlocked countries and check for any country codes still not matched
  if (conf$config$layer_region_labels=='rgn_global') {
    gc = filter(gc, !cntry_key %in% cntry_landlocked)
    if (sum(!gc$cntry_key %in% cntry_rgn$cntry_key) != 0){
      stop(sprintf('LIV_ECO trend missing country to region lookup for: %s.', paste(setdiff(gc$cntry_key, cntry_rgn$cntry_key), collapse=', ')))
    }
  }
  
  # aggregate countries to regions by weights
  # TODO: migrate to using name_to_rgn()
  gr = gc %>%
    merge(cntry_rgn, by = 'cntry_key', all.x = TRUE) %>%
    merge(weights, by = c('cntry_key','component'), all.x = TRUE) %>%
    select(component, rgn_id, rgn_name, cntry_key, score, w) %>%
    arrange(component, rgn_name, cntry_key) %>%
    group_by(component, rgn_id, rgn_name) %>%
    summarize(cntry_w     = paste(cntry_key[!is.na(w)], collapse=','),
              cntry_w_na  = paste(cntry_key[is.na(w)], collapse=','),
              n           = n(),
              n_w_na      = sum(is.na(w)),
              score_w_avg = weighted.mean(score, w),
              score_avg   = mean(score),
              w_sum       = sum(w, na.rm = TRUE)) %>%
    mutate(score = ifelse(!is.na(score_w_avg), score_w_avg, score_avg)) %>%
    ungroup() %>%
    filter(!is.na(rgn_id))

  data = gr %>%
    filter(component==g.component) %>%
    as.data.frame() %>%
    select(rgn_id, score, w_sum)

  # georegional gap fill ----
  if (conf$config$layer_region_labels=='rgn_global') {
    
    # georegional gapfill, and output gapfill_georegions attributes
    if (!file.exists('temp')) dir.create('temp', recursive = TRUE)
    csv = sprintf('temp/eez2013_%s-trend-gapfill-georegions.csv', subgoal)
    rg = gapfill_georegions(
      data              = data,
      fld_id            = 'rgn_id',
      fld_value         = 'score',
      fld_weight        = 'w_sum',
      georegions        = georegions,    
      ratio_weights     = FALSE,
      georegion_labels  = NULL,
      r0_to_NA          = TRUE, 
      attributes_csv    = csv)
    
  } else {
    rg = data
  }
  
  trend = rg %>%
    select(region_id=rgn_id, score) %>%    
    mutate(
      goal      = subgoal,
      dimension = 'trend',
      score     = score) %>%
    arrange(region_id)
  
  scores = rbind(status, trend)
  return(scores)
}

LE = function(scores, layers, eez2012 = FALSE){
  
  if (eez2012){
    # replacing 2012 scores for ECO and LIV with 2013 data (email Feb 28, Ben H.)
    # ECO: Eritrea (just this one country)
    # LIV: Eritrea, Anguilla, Bermuda, Egypt, Ghana, Indonesia, Iceland, Saint Kitts, 
    #      Sri Lanka, Brunei, Malaysia, Trinidad & Tobago, and Taiwan
    
    # replacement data and region names
    scores_2013 <- read.csv('../eez2013/scores.csv')  
    rgns = SelectLayersData(layers, layers='rgn_labels', narrow = TRUE) %>%
      select(region_id=id_num, label=val_chr) %>%
      arrange(label)
    
    # ECO
    ECO_rgn_id_replace = subset(rgns, label=='Eritrea', 'region_id', drop = TRUE)
    scores = scores %>%
      filter(!(goal=='ECO' & dimension=='score' & region_id==ECO_rgn_id_replace)) %>%
      rbind(
        scores_2013 %>%
          filter(goal=='ECO' & dimension=='score' & region_id==ECO_rgn_id_replace))
    
    # LIV
    LIV_rgns_label_replace = c('Eritrea','Anguilla','Bermuda','Egypt','Ghana','Indonesia','Iceland','Saint Kitts and Nevis','Sri Lanka','Brunei','Malaysia','Trinidad and Tobago','Taiwan')
    LIV_rgns_id_replace = subset(rgns, label %in% LIV_rgns_label_replace, 'region_id', drop = TRUE)
    stopifnot(length(LIV_rgns_label_replace)==length(LIV_rgns_id_replace))
    scores = scores %>%
      filter(!(goal=='LIV' & dimension=='score' & region_id %in% LIV_rgns_id_replace)) %>%
      rbind(
        scores_2013 %>%
          filter(goal=='LIV' & dimension=='score' & region_id %in% LIV_rgns_id_replace))
  }
  
  # calculate LE scores
  scores.LE = scores %>% 
    filter(goal %in% c('LIV','ECO') & dimension %in% c('status','trend','score','future')) %>%
    dcast(region_id + dimension ~ goal, value.var='score') %>%
    mutate(score = rowMeans(cbind(ECO, LIV), na.rm = TRUE)) %>%
    select(region_id, dimension, score) %>%
    mutate(goal  = 'LE')

  # rbind to all scores
  scores = scores %>%
    rbind(scores.LE)  
  
  # LIV, ECO and LE: nullify unpopulated regions and those of the Southern Ocean Islands
  r_s_islands   = subset(SelectLayersData(layers, layers='rgn_georegions', narrow = TRUE), 
                         category=='r2' & val_num==999, id_num, drop = TRUE)
  r_unpopulated = subset(ddply(SelectLayersData(layers, layers='le_popn', narrow = TRUE), .(id_num), summarize, 
                               count = val_num[which.max(year)]),
                         is.na(count) | count==0, id_num, drop = TRUE)
  scores[with(scores, 
              goal %in% c('LIV','ECO','LE') & 
                !dimension %in% c('pressures','resilience') & 
                region_id %in% union(r_s_islands, r_unpopulated)),
         'score'] = NA
  
  # return scores
  return(scores)  
}

ICO = function(layers){

  layers_data = SelectLayersData(layers, layers=c('ico_spp_extinction_status', 'ico_spp_popn_trend'))  
 
   rk <- layers_data %>%
    select(region_id = id_num, sciname = category, iucn_cat=val_chr, layer)
  
  # lookup for weights status
  w.risk_category = data.frame(iucn_cat = c('LC', 'NT', 'VU', 'EN', 'CR', 'EX'),
                               risk_score = c(1,  0.8,   0.6,  0.4,  0.2,  0))

  # lookup for population trend
  w.popn_trend = data.frame(iucn_cat = c('decreasing', 'stable', 'increasing'),
                            trend_score = c(-0.5, 0, 0.5))
  
  ####### status
  # STEP 1: take mean of subpopulation scores
  r.status_spp <- rk %>%
    filter(layer == 'ico_spp_extinction_status') %>%
    left_join(w.risk_category) %>%
    group_by(region_id, sciname) %>%
    summarize(spp_mean = mean(risk_score, na.rm=TRUE) * 100) %>%
    ungroup()
  # STEP 2: take mean of populations within regions
  r.status <- r.status_spp %>%
    group_by(region_id) %>%
    summarize(score = mean(spp_mean, na.rm=TRUE))
  
  ####### trend
  # STEP 1: take mean of subpopulation scores
  r.trend_spp <- rk %>%
    filter(layer == 'ico_spp_popn_trend') %>%
    left_join(w.popn_trend) %>%
    group_by(region_id, sciname) %>%
    summarize(spp_mean = mean(trend_score, na.rm=TRUE)) %>%
    ungroup()
  # STEP 2: take mean of populations within regions
  r.trend <- r.trend_spp %>%
    group_by(region_id) %>%
    summarize(score = mean(spp_mean, na.rm=TRUE))
  
  # return scores
  s.status = cbind(r.status, data.frame('dimension'='status'))
  s.trend  = cbind(r.trend , data.frame('dimension'='trend' ))
  scores = cbind(rbind(s.status, s.trend), data.frame('goal'='ICO'))
  return(scores)  
  
}

LSP = function(layers, ref_pct_cmpa=30, ref_pct_cp=30, status_year){
   trend_years = (status_year-4):status_year
  
  # select data ----
  r = SelectLayersData(layers, layers=c('rgn_area_inland1km', 'rgn_area_offshore3nm'))  #total offshore/inland areas
  ry = SelectLayersData(layers, layers=c('lsp_prot_area_offshore3nm', 'lsp_prot_area_inland1km')) #total protected areas                      

r <- r %>%
  select(region_id = id_num, val_num, layer) %>%
  spread(layer, val_num) %>%
  dplyr::rename(area_inland1km = rgn_area_inland1km,
                area_offshore3nm = rgn_area_offshore3nm)

ry <- ry %>%
  select(region_id = id_num, year, val_num, layer) %>%
  spread(layer, val_num) %>%
  dplyr::rename(cmpa = lsp_prot_area_offshore3nm,
                cp = lsp_prot_area_inland1km)
  
  # fill in time series for all regions and generate cumulative sum
r.yrs <- expand.grid(region_id = unique(ry$region_id),
                         year = unique(ry$year)) %>%
  left_join(ry, by=c('region_id', 'year')) %>%
  arrange(region_id, year) %>%
  mutate(cp= ifelse(is.na(cp), 0, cp),
         cmpa = ifelse(is.na(cmpa), 0, cmpa)) %>%
  group_by(region_id) %>%
  mutate(cp_cumsum    = cumsum(cp),
         cmpa_cumsum  = cumsum(cmpa)) %>%
  ungroup() %>%
 mutate(pa_cumsum     = cp_cumsum + cmpa_cumsum)
  
  # get percent of total area that is protected for inland1km (cp) and offshore3nm (cmpa) per year
  # and calculate status score
r.yrs = r.yrs %>%
  full_join(r) %>%
  mutate(pct_cp    = pmin(cp_cumsum   / area_inland1km   * 100, 100),
         pct_cmpa  = pmin(cmpa_cumsum / area_offshore3nm * 100, 100),
         prop_protected    = ( pmin(pct_cp / ref_pct_cp, 1) + pmin(pct_cmpa / ref_pct_cmpa, 1) ) / 2) %>%
  filter(!is.na(prop_protected))

# extract status based on specified year

  r.status = r.yrs %>%
    filter(year==status_year) %>%
    select(region_id, status=prop_protected) %>%
    mutate(status=status*100) 
head(r.status)
  
  # calculate trend
  r.trend =   r.yrs %>%
  filter(year %in% trend_years) %>%
  group_by(region_id) %>%
  do(mdl = lm(prop_protected ~ year, data=.)) %>%
  summarize(
    region_id = region_id,
    trend = min(1, max(0, 5*coef(mdl)['year']))) %>% # set boundaries so trend does not go below 0 or above 1
   ungroup()
  
  # return scores
  scores = rbind.fill(
    within(r.status, {
      goal      = 'LSP'
      dimension = 'status'
      score     = status}),
    within(r.trend, {
      goal      = 'LSP'
      dimension = 'trend'
      score     = trend}))
  return(scores[,c('region_id','goal','dimension','score')])    
}

SP = function(scores){
  
  d = within(
    dcast(
      scores, 
      region_id + dimension ~ goal, value.var='score', 
      subset=.(goal %in% c('ICO','LSP') & !dimension %in% c('pressures','resilience')))
    , {
      goal = 'SP'
      score = rowMeans(cbind(ICO, LSP), na.rm = TRUE)})
  
  
  # return all scores
  return(rbind(scores, d[,c('region_id','goal','dimension','score')]))
}


CW = function(layers){
  
  # layers
  lyrs = c('po_pathogens' = 'a',
           'po_nutrients' = 'u',
           'po_chemicals' = 'l',
           'po_trash'     = 'd',
           'cw_pesticide_trend'   = 'pest_trend',
           'cw_fertilizer_trend'  = 'fert_trend',
           'cw_coastalpopn_trend' = 'popn_trend',
           'cw_pathogen_trend'    = 'path_trend')
  
  # cast data
  d = SelectLayersData(layers, layers=names(lyrs))  
  r = rename(dcast(d, id_num ~ layer, value.var='val_num', subset = .(layer %in% names(lyrs))),
              c('id_num'='region_id', lyrs)); head(r); summary(r)
  
  # invert pressures
  r$a = 1 - r$a
  r$u = 1 - r$u
  r$l = 1 - r$l
  r$d = 1 - r$d
  
  # invert trends for CW
  r$popn_trend = -1 * r$popn_trend
  r$path_trend = -1 * r$path_trend
  r$pest_trend = -1 * r$pest_trend
  r$fert_trend = -1 * r$fert_trend
  
  # status
  r$status = psych::geometric.mean(t(r[,c('a','u','l','d')]), na.rm = TRUE) * 100
  
  # trend
  r$trend = rowMeans(r[,c('pest_trend','fert_trend','popn_trend','path_trend')], na.rm = TRUE)
  
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
    
  # get layer data
  d = 
    join_all(
      list(
        
        layers$data[['hab_health']] %>%
          select(rgn_id, habitat, health),
        
        layers$data[['hab_trend']] %>%
          select(rgn_id, habitat, trend),
        
        layers$data[['hab_extent']] %>%
          select(rgn_id, habitat, extent=km2)),
        
      by = c('rgn_id','habitat'), type='full') %>% 
    select(rgn_id, habitat, extent, health, trend)

  # limit to habitats used for HAB, create extent presence as weight
  d = d %>%
    filter(habitat %in% c('coral','mangrove','saltmarsh','seaice_edge','seagrass','soft_bottom')) %>%    
    mutate(
      w  = ifelse(!is.na(extent) & extent > 0, 1, NA)) %>%
    filter(!is.na(w)) %>%
    group_by(rgn_id)
  
  # calculate scores
  scores_HAB = rbind_list(
    # status
    d %>% 
      filter(!is.na(health)) %>%
      summarize(      
        score = pmin(1, sum(w * health) / sum(w)) * 100,
        dimension = 'status'),
    # trend
    d %>% 
      filter(!is.na(trend)) %>%      
      summarize(      
        score =  sum(w * trend) / sum(w),
        dimension = 'trend')) %>%
    mutate(
      goal = 'HAB') %>%
    select(region_id=rgn_id, goal, dimension, score)
  
  # return scores
  return(scores_HAB)  
}


SPP = function(layers){

  # scores
  scores = cbind(rename(SelectLayersData(layers, layers=c('spp_status'='status','spp_trend'='trend'), narrow = TRUE),
                      c(id_num='region_id', layer='dimension', val_num='score')), 
               data.frame('goal'='SPP'))
  scores = mutate(scores, score=ifelse(dimension=='status', score*100, score))
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
      score = rowMeans(cbind(HAB, SPP), na.rm = TRUE)})
  
  # return all scores
  return(rbind(scores, d[,c('region_id','goal','dimension','score')]))
}

PreGlobalScores = function(layers, conf, scores){
    
  # get regions
  rgns = SelectLayersData(layers, layers=conf$config$layer_region_labels, narrow = TRUE)
  
  # limit to just desired regions and global (region_id==0)
  scores = subset(scores, region_id %in% c(rgns[,'id_num'], 0))
  
  # apply NA to Antarctica
  id_ant = subset(rgns, val_chr=='Antarctica', id_num, drop = TRUE)
  scores[scores$region_id==id_ant, 'score'] = NA
    
  return(scores)
}

FinalizeScores = function(layers, conf, scores){
  
  # get regions
  rgns = SelectLayersData(layers, layers=conf$config$layer_region_labels, narrow = TRUE)
    
  # add NAs to missing combos (region_id, goal, dimension)
  d = expand.grid(list(score_NA  = NA,
                       region_id = c(rgns[,'id_num'], 0),
                       dimension = c('pressures','resilience','status','trend','future','score'), 
                       goal      = c(conf$goals$goal, 'Index')), stringsAsFactors = FALSE); head(d)
  d = subset(d, 
             !(dimension %in% c('pressures','resilience','trend') & region_id==0) & 
             !(dimension %in% c('pressures','resilience','status','trend') & goal=='Index'))
  scores = merge(scores, d, all = TRUE)[,c('goal','dimension','region_id','score')]
      
  # order
  scores = arrange(scores, goal, dimension, region_id)
  
  # round scores
  scores$score = round(scores$score, 2)
    
  return(scores)
}
