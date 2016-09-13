Setup = function(){
  if(file.exists('eez2013/temp/referencePoints.csv')){file.remove('temp/referencePoints.csv')}
  referencePoints <- data.frame(goal=as.character(), 
                                method = as.character(), 
                                reference_point = as.character())
  write.csv(referencePoints, 'temp/referencePoints.csv', row.names=FALSE)
}

FIS = function(layers, status_year){
 
  
  #catch data
  c = SelectLayersData(layers, layers='fis_meancatch', narrow = TRUE) %>%
    select(
      rgn_id    = id_num,
      stock_id_taxonkey = category,
      year,
      catch          = val_num)  
  # b_bmsy data
  b = SelectLayersData(layers, layer='fis_b_bmsy', narrow = TRUE) %>%
    select(
      rgn_id         = id_num,
      stock_id      = category,
      year,
      bmsy           = val_num)

# The following stocks are fished in multiple regions and have high b/bmsy values
# Due to the underfishing penalty, this actually penalizes the regions that have the highest
# proportion of catch of these stocks.  The following corrects this problem:
#  filter(b, stock_id %in% c('Katsuwonus_pelamis-71', 'Clupea_harengus-27', 'Trachurus_capensis-47'))

high_bmsy <- c('Katsuwonus_pelamis-71', 'Clupea_harengus-27', 'Trachurus_capensis-47', 'Sardinella_aurita-34', 'Scomberomorus_cavalla-31')
    
b <- b %>%
  mutate(bmsy = ifelse(stock_id %in% high_bmsy, 1, bmsy))
  
  
    # separate out the stock_id and taxonkey:
  c <- c %>% 
    mutate(stock_id_taxonkey = as.character(stock_id_taxonkey)) %>%
    mutate(taxon_key = str_sub(stock_id_taxonkey, -6, -1)) %>%
    mutate(stock_id = substr(stock_id_taxonkey, 1, nchar(stock_id_taxonkey)-7)) %>% 
    mutate(catch = as.numeric(catch)) %>%
    mutate(year = as.numeric(as.character(year))) %>%
    mutate(rgn_id = as.numeric(as.character(rgn_id))) %>%
    mutate(taxon_key = as.numeric(as.character(taxon_key))) %>%
    select(rgn_id, year, stock_id, taxon_key, catch)
  
  # general formatting:
  b <- b %>%
    mutate(bmsy = as.numeric(bmsy)) %>%
    mutate(rgn_id = as.numeric(as.character(rgn_id))) %>%
    mutate(year = as.numeric(as.character(year))) %>%
    mutate(stock_id = as.character(stock_id))  
  
  
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
  # STEP 1. Merge the b/bmsy data with catch data
  # -----------------------------------------------------------------------
  data_fis <- c %>%
    left_join(b, by=c('rgn_id', 'stock_id', 'year')) %>%
    select(rgn_id, stock_id, year, taxon_key, catch, bmsy, score) 
  

  # ------------------------------------------------------------------------
  # STEP 2. Estimate scores for taxa without b/bmsy values
  # Median score of other fish in the region is the starting point
  # Then a penalty is applied based on the level the taxa are reported at
  # -----------------------------------------------------------------------  
  
  ## this takes the median score within each region
  data_fis_gf <- data_fis %>%
    group_by(rgn_id, year) %>%
    mutate(Median_score = quantile(score, probs=c(0.5), na.rm=TRUE)) %>%
    ungroup() 
  
  ## this takes the median score across all regions (when no stocks have scores within a region)
  data_fis_gf <- data_fis_gf %>%
    group_by(year) %>%
    mutate(Median_score_global = quantile(score, probs=c(0.5), na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(Median_score = ifelse(is.na(Median_score), Median_score_global, Median_score)) %>%
    select(-Median_score_global)
  
  #  *************NOTE *****************************
  #  In some cases, it may make sense to alter the 
  #  penalty for not identifying fisheries catch data to
  #  species level.
  #  ***********************************************
  
  penaltyTable <- data.frame(TaxonPenaltyCode=1:6, 
                             penalty=c(0.1, 0.25, 0.5, 0.8, 0.9, 1))
  
  data_fis_gf <- data_fis_gf %>%
    mutate(TaxonPenaltyCode = as.numeric(substring(taxon_key, 1, 1))) %>%
    left_join(penaltyTable, by='TaxonPenaltyCode') %>%
    mutate(score_gf = Median_score * penalty) %>%
    mutate(score_gapfilled = ifelse(is.na(score), "Median gapfilled", "none")) %>%
    mutate(score = ifelse(is.na(score), score_gf, score))
  

  gap_fill_data <- data_fis_gf %>%
    mutate(gap_fill = ifelse(is.na(penalty), "none", "median")) %>%
    select(rgn_id, stock_id, taxon_key, year, catch, score, gap_fill) %>%
    filter(year == status_year)
  write.csv(gap_fill_data, 'temp/FIS_summary_gf.csv', row.names=FALSE)
  
  status_data <- data_fis_gf %>%
    select(rgn_id, stock_id, year, catch, score)
  
  
  # ------------------------------------------------------------------------
  # STEP 4. Calculate status for each region
  # -----------------------------------------------------------------------
  
  # 4a. To calculate the weight (i.e, the relative catch of each stock per region),
  # the mean catch of taxon i is divided by the   
  # sum of mean catch of all species in region/year 
  
  status_data <- status_data %>%
    group_by(year, rgn_id) %>%
    mutate(SumCatch = sum(catch)) %>%
    ungroup() %>%
    mutate(wprop = catch/SumCatch)
  
  status_data <- status_data %>%
    group_by(rgn_id, year) %>%
    summarize(status = prod(score^wprop)) %>%
    ungroup()
  
  # ------------------------------------------------------------------------
  # STEP 5. Get yearly status and trend  
  # -----------------------------------------------------------------------
  
  status <-  status_data %>%
    filter(year==status_year) %>%
    mutate(
      score     = round(status*100, 1),
      dimension = 'status') %>%
    select(region_id=rgn_id, score, dimension)
  
  trend_years <- status_year:(status_year-4)
  
  trend <- status_data %>%
    filter(year %in% trend_years) %>%
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

MAR = function(layers, status_year){  
  browser()
   # layers used: mar_harvest_tonnes, mar_harvest_species, mar_sustainability_score, mar_coastalpopn_inland25mi, mar_trend_years
  harvest_tonnes <- SelectLayersData(layers, layers='mar_harvest_tonnes', narrow = TRUE) %>%
    select(rgn_id=id_num, species_code=category, year, tonnes=val_num)
  
  sustainability_score <- SelectLayersData(layers, layers='mar_sustainability_score', narrow = TRUE) %>%
    select(rgn_id=id_num, species_code=category, sust_coeff=val_num)
  
  popn_inland25mi <- SelectLayersData(layers, layers='mar_coastalpopn_inland25mi', narrow = TRUE) %>%
    select(rgn_id=id_num, year, popsum=val_num)
  
  
  rky <-  harvest_tonnes %>%
    left_join(sustainability_score, by = c('rgn_id', 'species_code')) 
  
  # fill in gaps with no data
  rky <- spread(rky, year, tonnes)
  rky <- gather(rky, "year", "tonnes", 4:dim(rky)[2])
  
  
  # 4-year rolling mean of data
  m <- rky %>%
    mutate(year = as.numeric(as.character(year))) %>%
    group_by(rgn_id, species_code, sust_coeff) %>%
    arrange(rgn_id, species_code, year) %>%
    mutate(sm_tonnes = zoo::rollapply(tonnes, 4, mean, na.rm=TRUE, partial=TRUE)) %>%
    ungroup()
  
  # smoothed mariculture harvest * sustainability coefficient
  m <- m %>%
    mutate(sust_tonnes = sust_coeff * sm_tonnes)
  
  
  # aggregate all weighted timeseries per region, and divide by coastal human population
  ry = m %>%
    group_by(rgn_id, year) %>%
    summarize(sust_tonnes_sum = sum(sust_tonnes, na.rm=TRUE)) %>%  #na.rm = TRUE assumes that NA values are 0
    left_join(popn_inland25mi, by = c('rgn_id','year')) %>%
    mutate(mar_pop = sust_tonnes_sum / popsum) %>%
    ungroup()
  
  
  # get reference quantile based on argument years
  ref_95pct_data <- ry %>%
    filter(year <= status_year) 
  
  ref_95pct <- quantile(ref_95pct_data$mar_pop, 0.95, na.rm=TRUE)
  
  # identify reference rgn_id
  ry_ref = ref_95pct_data %>%
    arrange(mar_pop) %>%
    filter(mar_pop >= ref_95pct)
  message(sprintf('95th percentile for MAR ref pt is: %s\n', ref_95pct)) # rgn_id 25 = Thailand
  message(sprintf('95th percentile rgn_id for MAR ref pt is: %s\n', ry_ref$rgn_id[1])) # rgn_id 25 = Thailand
  
  rp <- read.csv('temp/referencePoints.csv', stringsAsFactors=FALSE) %>%
    rbind(data.frame(goal = "MAR", method = "spatial 95th quantile", 
                     reference_point = paste0("region id: ", ry_ref$rgn_id[1], ' value: ', ref_95pct)))
  write.csv(rp, 'temp/referencePoints.csv', row.names=FALSE)
  
  
  ry = ry %>%
    mutate(status = ifelse(mar_pop / ref_95pct > 1,
                           1, 
                           mar_pop / ref_95pct)) 
  status <- ry %>%
    filter(year == status_year) %>%
    select(rgn_id, status) %>%
    mutate(status = round(status*100, 2))
  
  
  # get MAR trend
  trend = ry %>%
    group_by(rgn_id) %>%
    do(mdl = lm(status ~ year, data=., subset=year %in% (status_year-4):(status_year))) %>%
    summarize(rgn_id, trend = coef(mdl)['year'] * 5) %>%
    ungroup()
  
  trend <- trend %>%
    mutate(trend = ifelse(trend>1, 1, trend)) %>%
    mutate(trend = ifelse(trend<(-1), (-1), trend)) %>%
    mutate(trend = round(trend, 2))
  
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
}


FP = function(layers, scores){
  
  # weights
  w <-  SelectLayersData(layers, layers='fp_wildcaught_weight', narrow = TRUE) %>%
    select(region_id = id_num, w_FIS = val_num); head(w)
  
  # scores
  s <- scores %>%
    filter(goal %in% c('FIS', 'MAR')) %>%
    filter(!(dimension %in% c('pressures', 'resilience'))) %>%
    left_join(w, by="region_id")  %>%
    mutate(w_MAR = 1 - w_FIS) %>%
    mutate(weight = ifelse(goal == "FIS", w_FIS, w_MAR)) 
  
  
  ## Some warning messages due to potential mismatches in data: 
  # NA score but there is a weight
  tmp <- filter(s, goal=='FIS' & is.na(score) & (!is.na(w_FIS) & w_FIS!=0) & dimension == "score")
  if(dim(tmp)[1]>0){
    warning(paste0("Check: these regions have a FIS weight but no score: ", 
                   paste(as.character(tmp$region_id), collapse = ", ")))}
  
  tmp <- filter(s, goal=='MAR' & is.na(score) & (!is.na(w_MAR) & w_MAR!=0) & dimension == "score")
  if(dim(tmp)[1]>0){
    warning(paste0("Check: these regions have a MAR weight but no score: ", 
                   paste(as.character(tmp$region_id), collapse = ", ")))}
  
  # score, but the weight is NA or 0
  tmp <- filter(s, goal=='FIS' & (!is.na(score) & score > 0) & (is.na(w_FIS) | w_FIS==0) & dimension == "score" & region_id !=0)
  if(dim(tmp)[1]>0){
    warning(paste0("Check: these regions have a FIS score but no weight: ", 
                   paste(as.character(tmp$region_id), collapse = ", ")))}
  
  tmp <- filter(s, goal=='MAR' & (!is.na(score) & score > 0) & (is.na(w_MAR) | w_MAR==0) & dimension == "score" & region_id !=0)
  if(dim(tmp)[1]>0){
    warning(paste0("Check: these regions have a MAR score but no weight: ", 
                   paste(as.character(tmp$region_id), collapse = ", ")))}
  
  s <- s  %>%
    group_by(region_id, dimension) %>%
    summarize(score = weighted.mean(score, weight, na.rm=TRUE)) %>%
    mutate(goal = "FP") %>%
    ungroup() %>%
    select(region_id, goal, dimension, score) %>%
    data.frame()
  
  # return all scores
  return(rbind(scores, s))
}


AO = function(layers, 
              status_year, 
              Sustainability=1.0){
  
  # cast data
  layers_data = SelectLayersData(layers, targets='AO')
  
  year_min=max(min(layers_data$year, na.rm = TRUE), status_year - 10)
  
  r <- layers_data %>%
    filter(layer == 'ao_access') %>%
    select(region_id=id_num, access=val_num)
  r <- na.omit(r)
  
  ry <- layers_data %>%
    filter(layer == 'ao_need') %>%
    select(region_id = id_num, year, need=val_num) %>%
    left_join(r, by="region_id")
  
  
  # model
  
  ry <- ry %>%
    mutate(Du = (1 - need) * (1 - access)) %>%
    mutate(statusData = (1 - Du) * Sustainability)
  
  # status
  r.status <- ry %>%
    filter(year==status_year) %>%
    select(region_id, statusData) %>%
    mutate(status=statusData*100)
  summary(r.status); dim(r.status)
  
  # trend
  r.trend <- ry %>%
    filter(year >= year_min) %>%
    filter(!is.na(statusData)) %>%
    group_by(region_id) %>%
    arrange(year) %>%
    top_n(5, year) %>%
    ungroup()
  
  
  r.trend <- r.trend %>%
    group_by(region_id) %>%
    do(mdl = lm(statusData ~ year, data=.)) %>%
    summarize( region_id = region_id, 
               trend = coef(mdl)['year']*5) %>%
    ungroup()
  
  ## reference points
  rp <- read.csv('temp/referencePoints.csv', stringsAsFactors=FALSE) %>%
    rbind(data.frame(goal = "AO", method = "??", 
                     reference_point = NA))
  write.csv(rp, 'temp/referencePoints.csv', row.names=FALSE)
  
  
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

NP <- function(scores, layers, status_year, debug = FALSE){

  ### new code version - load combined harvest variables
  r_cyanide    = layers$data[['np_cyanide']] # cyanide & blast used to calculate risk variable
  r_blast      = layers$data[['np_blast']]   
  hab_extent   = layers$data[['hab_extent']] # used to calculate exposure variable
  
  ### FIS status for fish oil sustainability
  #FIS_status <- read.csv('scores.csv')%>%
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
    ### ornamentals, shells, sponges).
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
      ungroup() %>%
      mutate(
        exposure = (log(expos_raw + 1) / log(expos_prod_max + 1)),
        exposure = ifelse(exposure > 1, 1, exposure)) %>%
      select(-km2, -expos_raw, -expos_prod_max)
    ### clean up columns
    
    gap_fill <- np_exp %>%
      mutate(gap_fill = ifelse(is.na(exposure), "prod_average", 0)) %>%
      select(rgn_id, product, year, gap_fill)
    write.csv(gap_fill, 'temp/NP_exposure_gapfill.csv', row.names=FALSE)
    
    ### add exposure for countries with (habitat extent == NA)
    np_exp <- np_exp %>% 
      group_by(product) %>%
      mutate(mean_exp = mean(exposure, na.rm = TRUE)) %>%
      mutate(exposure = ifelse(is.na(exposure), mean_exp, exposure)) %>%
      select(-mean_exp) %>%
      ungroup() %>%
      mutate(product = as.character(product))
    

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
      gather(product, risk, -rgn_id) %>%
      mutate(product = as.character(product))
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
    
    ### add in fish_oil sustainability based on FIS scores calculated above:
    ### add fish_oil (no exposure calculated, sustainability is based on FIS score only, and not exposure/risk components)
    fish_oil_sust <-   FIS_status %>%
      mutate(sustainability = score / 100) %>%
      mutate(sustainability = ifelse(is.na(sustainability), 0, sustainability)) %>% 
      select(rgn_id, sustainability)
      
    np_sus_fis_oil <- np_harvest %>%
      filter(product=='fish_oil') %>%
      mutate(exposure = NA) %>%
      mutate(risk = NA) %>%
      left_join(fish_oil_sust, by='rgn_id')
    
    np_exp <- np_sust %>% 
      bind_rows(np_sus_fis_oil)
      

    ### calculate rgn-product-year status
    np_sust <- np_sust %>% 
      mutate(product_status = tonnes_rel * sustainability) %>%
      filter(rgn_name != 'DISPUTED') %>%
      select(-tonnes, -tonnes_rel, -risk, -exposure) %>%
      ungroup()
    
    return(np_sust)
  }
  
  np_calc_scores <- function(np_sust, status_year) {
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
    
    ### get current status
    np_status_current <- np_status_all %>%
      filter(year == status_year & !is.na(status)) %>%
      mutate(
        dimension = 'status',
        score     = round(status,4) * 100) %>%
      select(rgn_id, dimension, score)
    stopifnot(
      min(np_status_current$score, na.rm = TRUE) >= 0, 
      max(np_status_current$score, na.rm = TRUE) <= 100)
    
    ### trend 
    np_trend <- np_status_all %>%
      filter(year <= status_year & year > (status_year - 5) & !is.na(status)) %>%
      group_by(rgn_id) %>%
      do(mdl = lm(status ~ year, data=.)) %>%
      summarize(
        rgn_id    = rgn_id,
        dimension = 'trend',
        score     = max(-1, min(1, coef(mdl)[['year']] * 5)))
    stopifnot(min(np_trend$score) >= -1, max(np_trend$score) <= 1)
    
    
    ### return scores
    np_scores <- np_status_current %>%
      full_join(np_trend, by=c('rgn_id', 'dimension', 'score')) %>%
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
  np_scores  <- np_calc_scores(np_sust, status_year) 
  
  ## reference points
  rp <- read.csv('temp/referencePoints.csv', stringsAsFactors=FALSE) %>%
    rbind(data.frame(goal = "NP", method = "Harvest peak within region times 0.65 buffer", 
                     reference_point = "varies for each region"))
  write.csv(rp, 'temp/referencePoints.csv', row.names=FALSE)
  
  
  return(np_scores)
}


CS <- function(layers){
  
  ## read in layers
  extent <- layers$data[['hab_extent']] %>%
    select(rgn_id, habitat, km2) %>%
    mutate(habitat = as.character(habitat))
  
  health <-  layers$data[['hab_health']] %>%
    select(rgn_id, habitat, health) %>%
    mutate(habitat = as.character(habitat))
  
  trend <-layers$data[['hab_trend']] %>%
    select(rgn_id, habitat, trend) %>%
    mutate(habitat = as.character(habitat))
  
  ## join layer data
  d <-  extent %>%
    full_join(health, by=c("rgn_id", "habitat")) %>%
    full_join(trend, by=c("rgn_id", "habitat"))
  
  ## set ranks for each habitat
  habitat.rank <- c('mangrove'         = 139,
                    'saltmarsh'        = 210,
                    'seagrass'         = 83)
  
  ## limit to CS habitats and add rank
  d <- d %>%
    filter(habitat %in% names(habitat.rank)) %>%
    mutate(
      rank = habitat.rank[habitat],
      extent = ifelse(km2==0, NA, km2))
  
  ## output file to temp folder that describes how much each habitat
  ## contributes to the score based on rank and extent
  ## this output is for the dataplayground website
  dp <- d %>%
    mutate(weighted_cont = rank*extent) %>%
    filter(!is.na(weighted_cont)) %>%
    group_by(rgn_id) %>%
    mutate(prop_score = weighted_cont/sum(weighted_cont)) %>%
    mutate(prop_score = round(prop_score, 3)) %>%
    select(rgn_id, habitat, prop_score)
  write.csv(dp, 'temp/cs_hab_contributions.csv', row.names=FALSE)
  
  ## status and trend models; ensure at least one habitat-region has extent (km2) > 0, otherwise set NA.
  if (sum(d$km2, na.rm=TRUE) > 0){
    # status
    scores_CS <- d %>%
      filter(!is.na(rank) & !is.na(health) & !is.na(extent)) %>%
      group_by(rgn_id) %>%
      summarize(
        score = pmin(1, sum(rank * health * extent, na.rm=TRUE) / (sum(extent * rank, na.rm=TRUE)) ) * 100,
        dimension = 'status') %>%
      ungroup()
    
    # trend
    d_trend <- d %>%
      filter(!is.na(rank) & !is.na(trend) & !is.na(extent))
    if (nrow(d_trend) > 0 ){
      scores_CS <- dplyr::bind_rows(
        scores_CS,
        d_trend %>%
          group_by(rgn_id) %>%
          summarize(
            score = sum(rank * trend * extent, na.rm=TRUE) / (sum(extent*rank, na.rm=TRUE)),
            dimension = 'trend')) %>%
        ungroup()
    } else { # if no trend score, assign NA
      scores_CS <- dplyr::bind_rows(
        scores_CS,
        d %>%
          group_by(rgn_id) %>%
          summarize(
            score = NA,
            dimension = 'trend'))
    }
    
    ### output data file for checking and data review
    scores_check <- spread(scores_CS, dimension, score) %>%
      select(rgn_id, status, trend_score=trend)
    
    d_check <- d %>%
      select(rgn_id, habitat, extent, health, trend, rank) %>%
      arrange(rgn_id, habitat) %>%
      left_join(scores_check, by="rgn_id") 
    write.csv(d_check, sprintf('temp/cs_data_%s.csv', scenario), row.names=FALSE)    
    ### end: output...   
    
    scores_CS <- scores_CS %>%
      mutate(
        goal = 'CS') %>%
      select(region_id=rgn_id, goal, dimension, score)
    
  } else { ## else -- if sum(d$km2) is not greater than 0
    
     ## set status and trend to NA for all regions
      message('CS status and trend are NA, consider removing goal if no CS habitats in assessment area')

      rgns <-layers$data[['rgn_labels']]
      scores_CS <- bind_rows(
        rgns %>%
        mutate(goal      = 'CS',
               dimension = 'status',
               score     = NA),
      rgns %>%
        mutate(goal      = 'CS',
               dimension = 'trend',
               score     = NA)) %>%
        select(goal, dimension, region_id = rgn_id, score)
  
  } ## end -- if (sum(d$km2) > 0)
  
  ## reference points
  rp <- read.csv('temp/referencePoints.csv', stringsAsFactors=FALSE) %>%
    rbind(data.frame(goal = "CS", method = "Health/condition variable based on current vs. historic extent", 
                     reference_point = "varies for each region/habitat"))
  write.csv(rp, 'temp/referencePoints.csv', row.names=FALSE)
  
  
  # return scores
  return(scores_CS)
}  



CP <- function(layers){
  
  ## read in layers
  extent <- layers$data[['hab_extent']] %>%
    select(rgn_id, habitat, km2) %>%
    mutate(habitat = as.character(habitat))
  
  health <-  layers$data[['hab_health']] %>%
    select(rgn_id, habitat, health) %>%
    mutate(habitat = as.character(habitat))
  
  trend <-layers$data[['hab_trend']] %>%
    select(rgn_id, habitat, trend) %>%
    mutate(habitat = as.character(habitat))
  
  
  ## sum mangrove_offshore + mangrove_inland1km = mangrove to match with extent and trend
  mangrove_extent <- extent %>%
    filter(habitat %in% c('mangrove_inland1km','mangrove_offshore')) 
  
  if (nrow(mangrove_extent) > 0){
    mangrove_extent <- mangrove_extent %>%
      group_by(rgn_id) %>%
      summarize(km2 = sum(km2, na.rm = TRUE)) %>%
      mutate(habitat='mangrove') %>%
      ungroup() 
  }
  
  extent <- extent %>%
    filter(!habitat %in% c('mangrove','mangrove_inland1km','mangrove_offshore')) %>%  #do not use all mangrove
    rbind(mangrove_extent)  #just the inland 1km and offshore
  
  ## join layer data
  d <-  extent %>%
    full_join(health, by=c("rgn_id", "habitat")) %>%
    full_join(trend, by=c("rgn_id", "habitat"))
  
  ## set ranks for each habitat
  habitat.rank <- c('coral'            = 4,
                    'mangrove'         = 4,
                    'saltmarsh'        = 3,
                    'seagrass'         = 1,
                    'seaice_shoreline' = 4)
  
  ## limit to CP habitats and add rank
  d <- d %>%
    filter(habitat %in% names(habitat.rank)) %>%
    mutate(
      rank = habitat.rank[habitat],
      extent = ifelse(km2==0, NA, km2))
  
  ## output file to temp folder that describes how much each habitat
  ## contributes to the score based on rank and extent
  ## this output is for the dataplayground website
  dp <- d %>%
    mutate(weighted_cont = rank*extent) %>%
    filter(!is.na(weighted_cont)) %>%
    group_by(rgn_id) %>%
    mutate(prop_score = weighted_cont/sum(weighted_cont)) %>%
    mutate(prop_score = round(prop_score, 3)) %>%
    select(rgn_id, habitat, prop_score)
  write.csv(dp, 'temp/cp_hab_contributions.csv', row.names=FALSE)
  
  ## status and trend models; ensure at least one habitat-region has extent (km2) > 0, otherwise set NA.
  if (sum(d$km2, na.rm=TRUE) > 0){
    # status
    scores_CP <- d %>%
      filter(!is.na(rank) & !is.na(health) & !is.na(extent)) %>%
      group_by(rgn_id) %>%
      summarize(score = pmin(1, sum(rank * health * extent, na.rm=TRUE) / 
                               (sum(extent * rank, na.rm=TRUE)) ) * 100) %>%
      mutate(dimension = 'status') %>%
      ungroup()
    
    # trend
    d_trend <- d %>%
      filter(!is.na(rank) & !is.na(trend) & !is.na(extent))
    
    if (nrow(d_trend) > 0 ){
      scores_CP <- dplyr::bind_rows(
        scores_CP,
        d_trend %>%
          group_by(rgn_id) %>%
          summarize(
            score = sum(rank * trend * extent, na.rm=TRUE) / (sum(extent*rank, na.rm=TRUE)),
            dimension = 'trend'))
    } else { # if no trend score, assign NA
      scores_CP <- dplyr::bind_rows(
        scores_CP,
        d %>%
          group_by(rgn_id) %>%
          summarize(
            score = NA,
            dimension = 'trend'))
    }
    
    ### output data file for checking and data review
    scores_check <- spread(scores_CP, dimension, score) %>%
      select(rgn_id, status, trend_score=trend)
    
    d_check <- d %>%
      select(rgn_id, habitat, extent, health, trend, rank) %>%
      arrange(rgn_id, habitat) %>%
      left_join(scores_check, by="rgn_id") 
    write.csv(d_check, sprintf('temp/cp_data_%s.csv', scenario), row.names=FALSE)    
    
    ## finalize scores_CP
    scores_CP <- scores_CP %>%
      mutate(
        goal = 'CP') %>%
      select(region_id=rgn_id, goal, dimension, score)
  
  } else { ## else -- if sum(d$km2) is not greater than 0
    
    ## set status and trend to NA for all regions
    message('CP status and trend are NA, consider removing goal if no CP habitats in assessment area')
    
    rgns <-layers$data[['rgn_labels']]
    scores_CP <- bind_rows(
      rgns %>%
        mutate(goal      = 'CP',
               dimension = 'status',
               score     = NA),
      rgns %>%
        mutate(goal      = 'CP',
               dimension = 'trend',
               score     = NA)) %>%
      select(goal, dimension, region_id = rgn_id, score)
    
  } ## end -- if (sum(d$km2) > 0)
  
  ## reference points
  rp <- read.csv('temp/referencePoints.csv', stringsAsFactors=FALSE) %>%
    rbind(data.frame(goal = "CP", method = "Health/condition variable based on current vs. historic extent", 
                     reference_point = "varies for each region/habitat"))
  write.csv(rp, 'temp/referencePoints.csv', row.names=FALSE)
  
  
  # return scores
  return(scores_CP)
  
}


TR = function(layers, status_year, pct_ref = 90) {
  
   ## formula:
  ##  E   = Ep                         # Ep: % of direct tourism jobs. tr_jobs_pct_tourism.csv
  ##  S   = (S_score - 1) / (7 - 1)    # S_score: raw TTCI score, not normalized (1-7). tr_sustainability.csv
  ##  Xtr = E * S

  ## read in layers
  tr_data  <- full_join(
    layers$data[['tr_jobs_pct_tourism']] %>%
      select(-layer),
    layers$data[['tr_sustainability']] %>%
      select(-layer),
    by = c('rgn_id'))%>%
    filter(year <= status_year)
  
  tr_model <- tr_data %>%
    mutate(
      E   = Ep,
      S   = (S_score - 1) / (7 - 1), # scale score from 1 to 7.
      Xtr = E * S ) %>%  
    filter(year <= status_year & year > status_year - 5) 
  # five data years for trend calcs

  # regions with Travel Warnings 
  ### adjust the travel warning years...these always reflect the current year
  ### but the other datasets will lag
  if (exists('scenarios')) { ## if global scenarios
    scenario_year <- as.numeric(substring(scenario, 4,7))
    offset_years <- scenario_year - status_year
    
    ## read in layers for regions with Travel Warnings
    rgn_travel_warnings <- layers$data[['tr_travelwarnings']] %>%
      select(rgn_id, year, multiplier) %>%
      mutate(year = year - offset_years)

    ## incorporate Travel Warnings
    tr_model <- tr_model %>%
      left_join(rgn_travel_warnings, by = c('rgn_id', 'year')) %>%
      mutate(Xtr = ifelse(!is.na(multiplier), multiplier * Xtr, Xtr)) %>%
      select(-multiplier)
    
  } ## end if (exists('scenarios'))
  
  ### Calculate status based on quantile reference (see function call for pct_ref)
  tr_model <- tr_model %>%
    select(rgn_id, year, Xtr) %>%
    left_join(tr_model %>%
                group_by(year) %>%
                summarize(Xtr_q = quantile(Xtr, probs = pct_ref/100, na.rm = TRUE)),
              by = 'year') %>%
    mutate(
      Xtr_rq  = ifelse(Xtr / Xtr_q > 1, 1, Xtr / Xtr_q)) # rescale to qth percentile, cap at 1
  
  ## reference points
  ref_point <- tr_model %>%
    filter(year == status_year) %>%
    select(Xtr_q) %>%
    unique()
  rp <- read.csv('temp/referencePoints.csv', stringsAsFactors=FALSE) %>%
    rbind(data.frame(goal = "TR", method = paste0('spatial: ', pct_ref, "th quantile"), 
                     reference_point = ref_point$Xtr_q))
  write.csv(rp, 'temp/referencePoints.csv', row.names=FALSE)
  
  
  
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
    reshape2::dcast(metric + cntry_key + sector ~ field, value.var='val_num')
  
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
    reshape2::dcast(cntry_key ~ metric, value.var='score') %>%
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
          cntry_key = plyr::revalue(cntry_key, c(
            'SCG'            = 'MNE',  # used to be Serbia (no coast) and Montenegro (has coast) in Nature 2012
            'Aruba'          = 'ABW',  # ABW and ANT EEZs got split...
            'Bonaire'        = 'ANT',
            'Curacao'        = 'ANT',
            'Sint Eustatius' = 'ANT',
            'Saba'           = 'ANT',
            'Brunei'         = 'BRN',  # Brunei new country in Malaysia
            'Malaysia'       = 'MYS'))) %>%
        dplyr::bind_rows(
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
    reshape2::dcast(rgn_id ~ level, value.var='georgn_id')
  
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
    adjustments = dplyr::bind_rows(
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
    mcsy = dplyr::bind_rows(
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
  mc = dplyr::bind_rows(
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
  gc = dplyr::bind_rows(
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
    reshape2::dcast(region_id + dimension ~ goal, value.var='score') %>%
    mutate(score = rowMeans(cbind(ECO, LIV), na.rm = TRUE)) %>%
    select(region_id, dimension, score) %>%
    mutate(goal  = 'LE')
  
  # rbind to all scores
  scores = scores %>%
    rbind(scores.LE)  
  
  # LIV, ECO and LE: nullify unpopulated regions and those of the Southern Ocean Islands
  r_s_islands   = subset(SelectLayersData(layers, layers='rgn_georegions', narrow = TRUE), 
                         category=='r2' & val_num==999, id_num, drop = TRUE)
  r_unpopulated = subset(plyr::ddply(SelectLayersData(layers, layers='le_popn', narrow = TRUE), plyr::.(id_num), summarize, 
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

ICO = function(layers, status_year){
  
  layers_data = SelectLayersData(layers, layers=c('ico_spp_iucn_status'))  
  
  rk <- layers_data %>%
    select(region_id = id_num, sciname = category, iucn_cat=val_chr, year, layer) %>%
    mutate(iucn_cat = as.character(iucn_cat))
  
  # lookup for weights status
  #  LC <- "LOWER RISK/LEAST CONCERN (LR/LC)"                         
  #  NT <- "LOWER RISK/NEAR THREATENED (LR/NT)"                       
  #  T  <- "THREATENED (T)" treat as "EN"
  #  VU <- "VULNERABLE (V)"                                           
  #  EN <- "ENDANGERED (E)"                                           
  #  LR/CD <- "LOWER RISK/CONSERVATION DEPENDENT (LR/CD)" treat as between VU and NT
  #  CR <- "VERY RARE AND BELIEVED TO BE DECREASING IN NUMBERS"       
  #  DD <- "INSUFFICIENTLY KNOWN (K)"                                 
  #  DD <- "INDETERMINATE (I)"                                        
  #  DD <- "STATUS INADEQUATELY KNOWN-SURVEY REQUIRED OR DATA SOUGHT" 
  w.risk_category = data.frame(iucn_cat = c('LC', 'NT', 'CD', 'VU', 'EN', 'CR', 'EX', 'DD'),
                               risk_score = c(0,  0.2,  0.3,  0.4,  0.6,  0.8,  1, NA)) %>%
                    mutate(status_score = 1-risk_score) %>%
    mutate(iucn_cat = as.character(iucn_cat))
  
  ####### status
  # STEP 1: take mean of subpopulation scores
  r.status_spp <- rk %>%
    left_join(w.risk_category, by = 'iucn_cat') %>%
    group_by(region_id, sciname, year) %>%
    summarize(spp_mean = mean(status_score, na.rm=TRUE)) %>%
    ungroup()
  
  # STEP 2: take mean of populations within regions
  r.status <- r.status_spp %>%
    group_by(region_id, year) %>%
    summarize(score = mean(spp_mean, na.rm=TRUE)) %>%
    ungroup() 
  
  ####### trend
  trend_years <- c(status_year:(status_year - 9)) # trend based on 10 years of data, due to infrequency of IUCN assessments 
  r.trend <- r.status %>%
    filter(year %in% trend_years) %>%
    group_by(region_id) %>%
    do(mdl = lm(score ~ year, data=.)) %>%
    summarize(region_id = region_id,
              score = coef(mdl)['year'] * 5) %>%
    ungroup() %>%
    mutate(dimension = "trend")
  
  ####### status
  r.status <- r.status %>%
    filter(year == status_year) %>%
    mutate(score = score * 100) %>%
    mutate(dimension = "status") %>%
    select(region_id, score, dimension)

  ## reference points
  rp <- read.csv('temp/referencePoints.csv', stringsAsFactors=FALSE) %>%
    rbind(data.frame(goal = "ICO", method = "scaled IUCN risk categories", 
                     reference_point = NA))
  write.csv(rp, 'temp/referencePoints.csv', row.names=FALSE)
  
  
  # return scores
  scores <-  rbind(r.status, r.trend) %>%
    mutate('goal'='ICO') %>%
    select(goal, dimension, region_id, score) %>%
    data.frame()
  
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
    select(region_id, area_inland1km = rgn_area_inland1km,
           area_offshore3nm = rgn_area_offshore3nm)
  
  ry <- ry %>%
    select(region_id = id_num, year, val_num, layer) %>%
    spread(layer, val_num) %>%
    select(region_id, year, cmpa = lsp_prot_area_offshore3nm,
           cp = lsp_prot_area_inland1km)
  
  # fill in time series for all regions 

r.yrs <- expand.grid(region_id = unique(ry$region_id),
                         year = unique(ry$year)) %>%
  left_join(ry, by=c('region_id', 'year')) %>%
  arrange(region_id, year) %>%
  mutate(cp= ifelse(is.na(cp), 0, cp),
         cmpa = ifelse(is.na(cmpa), 0, cmpa)) %>%
 mutate(pa     = cp + cmpa)
  
  # get percent of total area that is protected for inland1km (cp) and offshore3nm (cmpa) per year
  # and calculate status score
r.yrs = r.yrs %>%
  full_join(r, by="region_id") %>%
  mutate(pct_cp    = pmin(cp   / area_inland1km   * 100, 100),
         pct_cmpa  = pmin(cmpa / area_offshore3nm * 100, 100),
         prop_protected    = ( pmin(pct_cp / ref_pct_cp, 1) + pmin(pct_cmpa / ref_pct_cmpa, 1) ) / 2) %>%
  filter(!is.na(prop_protected))

# extract status based on specified year
  r.status = r.yrs %>%
    filter(year==status_year) %>%
    select(region_id, status=prop_protected) %>%
    mutate(status=status*100) 
  
  # calculate trend
  r.trend =   r.yrs %>%
    filter(year %in% trend_years) %>%
    group_by(region_id) %>%
    do(mdl = lm(prop_protected ~ year, data=.)) %>%
    summarize(
      region_id = region_id,
      trend = min(1, max(0, 5*coef(mdl)['year']))) %>% # set boundaries so trend does not go below 0 or above 1
    ungroup()
  
  ## reference points
  rp <- read.csv('temp/referencePoints.csv', stringsAsFactors=FALSE) %>%
    rbind(data.frame(goal = "LSP", method = paste0(ref_pct_cmpa, "% marine protected area; ", 
                                                   ref_pct_cp, "% coastal protected area"), 
                     reference_point = "varies by area of region's eez and 1 km inland"))
  write.csv(rp, 'temp/referencePoints.csv', row.names=FALSE)
  
  
  # return scores
  scores = bind_rows(
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
  
  ## to calculate the four SP dimesions, average those dimensions for ICO and LSP
  s <- scores %>%
    filter(goal %in% c('ICO','LSP'),
           dimension %in% c('status', 'trend', 'future', 'score')) %>%
    group_by(region_id, dimension) %>%
    summarize(score = mean(score, na.rm=TRUE)) %>%
    ungroup() %>%
    arrange(region_id) %>%
    mutate(goal = "SP") %>%
    select(region_id, goal, dimension, score) %>%
    data.frame()
  
  # return all scores
  return(rbind(scores, s))
}


CW = function(layers){
  
  # layers
  lyrs <- c('po_pathogens', 'po_nutrients_3nm', 'po_chemicals_3nm', 'po_trash',
            'cw_chemical_trend', 'cw_nutrient_trend', 'cw_trash_trend', 'cw_pathogen_trend')
  
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
  
  
  # return scores
  scores = rbind(d_pressures, d_trends) %>%
    mutate(goal = "CW") %>%
    select(region_id, goal, dimension, score) %>%
    data.frame()
  
  ## reference points
  rp <- read.csv('temp/referencePoints.csv', stringsAsFactors=FALSE) %>%
    rbind(data.frame(goal = "CW", method = "spatial: pressures scaled from 0-1 at raster level", 
                     reference_point = NA))
  write.csv(rp, 'temp/referencePoints.csv', row.names=FALSE)
  
  return(scores)  
}


HAB = function(layers){
  
  ## get the data:
  health <-  layers$data[['hab_health']] %>%
    select(rgn_id, habitat, health) %>%
    mutate(habitat = as.character(habitat))
  
  trend <-  layers$data[['hab_trend']] %>%
    select(rgn_id, habitat, trend) %>%
    mutate(habitat = as.character(habitat))
  
  extent <- layers$data[['hab_extent']] %>%
    select(rgn_id, habitat, extent=km2) %>%
    mutate(habitat = as.character(habitat))
  
  # join and limit to HAB habitats
  d <- health %>%
    full_join(trend, by = c('rgn_id', 'habitat')) %>%
    full_join(extent, by = c('rgn_id', 'habitat')) %>%
    filter(habitat %in% c('coral','mangrove','saltmarsh','seaice_edge','seagrass','soft_bottom')) %>%    
    mutate(w  = ifelse(!is.na(extent) & extent > 0, 1, NA)) %>%
    filter(!is.na(w))
  
  if(sum(d$w %in% 1 & is.na(d$trend)) > 0){
    warning("Some regions/habitats have extent data, but no trend data.  Consider estimating these values.")
  }
  
  if(sum(d$w %in% 1 & is.na(d$health)) > 0){
    warning("Some regions/habitats have extent data, but no health data.  Consider estimating these values.")
  }  
  
  
  ## calculate scores 
  status <- d %>%
    group_by(rgn_id) %>%
    filter(!is.na(health)) %>%
    summarize(      
      score = pmin(1, sum(health) / sum(w)) * 100,
      dimension = 'status') %>%
    ungroup()
  
  trend <- d %>%
    group_by(rgn_id) %>%
    filter(!is.na(trend)) %>%      
    summarize(      
      score =  sum(trend) / sum(w),
      dimension = 'trend')  %>%
    ungroup()
  
  scores_HAB <- rbind(status, trend) %>%
    mutate(goal = "HAB") %>%
    select(region_id=rgn_id, goal, dimension, score)
  
  ## reference points
  rp <- read.csv('temp/referencePoints.csv', stringsAsFactors=FALSE) %>%
    rbind(data.frame(goal = "HAB", method = "Health/condition variable based on current vs. historic extent", 
                     reference_point = "varies for each region/habitat"))
  write.csv(rp, 'temp/referencePoints.csv', row.names=FALSE)
  
  # return scores
  return(scores_HAB)  
}


SPP = function(layers){
  scores <-   SelectLayersData(layers, layers=c('spp_status'='status','spp_trend'='trend'), narrow = TRUE) %>%
    select(region_id = id_num, dimension = layer, score = val_num) %>%
    mutate(goal = 'SPP') %>%
    mutate(score = ifelse(dimension == 'status', score*100, score))
  
  ## reference points
  rp <- read.csv('temp/referencePoints.csv', stringsAsFactors=FALSE) %>%
    rbind(data.frame(goal = "SPP", method = "Average of IUCN risk categories, scaled to historic extinction", 
                     reference_point = NA))
  write.csv(rp, 'temp/referencePoints.csv', row.names=FALSE)
  
  
  return(scores) 
}

BD = function(scores){
  d <- scores %>%
    filter(goal %in% c('HAB', 'SPP')) %>%
    filter(!(dimension %in% c('pressures', 'resilience'))) %>%
    group_by(region_id, dimension) %>%
    summarize(score = mean(score, na.rm=TRUE)) %>%
    mutate(goal = 'BD') %>%
    data.frame()
  
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
