Setup = function(){
  if(file.exists('temp/reference_pts.csv'))
  {file.remove('temp/reference_pts.csv')}
  
  ref_pts <- data.frame(year   = as.integer(),
                        goal   = as.character(),
                        method = as.character(),
                        reference_point = as.character())
  write_csv(ref_pts, 'temp/reference_pts.csv')
  
}

# general function to calculate trend
# select the status_layer that is used as the baseline year
trend_calc2 <- function(status_data, status_layer=NA, trend_years=trend_years){   
  # status_data = ry
  # status_layer = "ao_access"
  # trend_years = trend_years  # want to use "data years" rather than scenario years
  # because scenario years may be repeated
  
  if(!is.na(status_layer)){
    names(status_data)[which(names(status_data) == paste0(status_layer, "_year"))] <- "year"
  }
  
  if(sum(grepl("rgn_id", names(status_data))>0)){
    names(status_data)[which(names(status_data)=="rgn_id")] <- "region_id"
  }
  
  status_data <- status_data %>%
    select(region_id, year, status) %>%
    filter(year %in% trend_years) %>%
    unique()
  
  adj_trend_year <- min(trend_years)
  
  r.trend = status_data %>%
    group_by(region_id) %>%
    do(mdl = lm(status ~ year, data=.),
       adjust_trend = .$status[.$year == adj_trend_year]) %>%
    summarize(region_id, score = ifelse(coef(mdl)['year']==0, 0, coef(mdl)['year']/adjust_trend * 5)) %>%
    ungroup() %>%
    mutate(score = ifelse(score>1, 1, score)) %>%
    mutate(score = ifelse(score<(-1), (-1), score)) %>%
    mutate(score = round(score, 4)) %>%
    mutate(dimension = "trend") %>%
    select(region_id, score, dimension)
  
  return(r.trend)
}    

trend_calc <- function(status_data, trend_years=trend_years){   
  # status_data = ry
  # status_layer = "ao_access"
  # trend_years = trend_years  # want to use "data years" rather than scenario years
  # because scenario years may be repeated
  
  
  if(sum(grepl("rgn_id", names(status_data))>0)){
    names(status_data)[which(names(status_data)=="rgn_id")] <- "region_id"
  }
  
  if(sum(grepl("scenario_year", names(status_data)) > 0)) {
    names(status_data)[which(names(status_data) == "scenario_year")] <- "year"
  }
  
  status_data <- status_data %>%
    select(region_id, year, status) %>%
    filter(year %in% trend_years) %>%
    unique()
  
  adj_trend_year <- min(trend_years)
  
  r.trend = status_data %>%
    group_by(region_id) %>%
    do(mdl = lm(status ~ year, data=.),
       adjust_trend = .$status[.$year == adj_trend_year]) %>%
    summarize(region_id, score = ifelse(coef(mdl)['year']==0, 0, coef(mdl)['year']/adjust_trend * 5)) %>%
    ungroup() %>%
    mutate(score = ifelse(score>1, 1, score)) %>%
    mutate(score = ifelse(score<(-1), (-1), score)) %>%
    mutate(score = round(score, 4)) %>%
    mutate(dimension = "trend") %>%
    select(region_id, score, dimension)
  
  return(r.trend)
}    


# function to link data and scenario years based on 
# conf/scenario_data_years.csv information

get_data_year <- function(layer_nm, layers=layers) { #layer_nm="le_wage_cur_base_value"
  
  all_years <- conf$scenario_data_years %>%
    mutate(scenario_year= as.numeric(scenario_year),
           data_year = as.numeric(data_year)) %>%
    filter(layer_name %in% layer_nm) %>%
    select(layer_name, scenario_year, year=data_year)
  
  
  layer_vals <- layers$data[[layer_nm]]
  
  layers_years <- all_years %>%
    left_join(layer_vals, by="year") %>%
    select(-layer)
  
  names(layers_years)[which(names(layers_years)=="year")] <- paste0(layer_nm, "_year")  
  
  return(layers_years)
}


# useful function for compiling multiple data layers 
# only works when the variable names are the same across datasets
# (e.g., coral, seagrass, and mangroves)
SelectData2 <- function(layer_names){  
  data <- data.frame()
  for(e in layer_names){ # e="le_jobs_cur_base_value"
    data_new <- get_data_year(layer_nm=e, layers=layers) 
    names(data_new)[which(names(data_new) == paste0(e, "_year"))] <- "data_year"
    data <- rbind(data, data_new)
  }
  return(data)
}


write_ref_pts <- function(goal, method, ref_pt) {
  
  ref_pts <- read.csv("temp/reference_pts.csv")  %>%
    rbind(data.frame(year   = layers$data$scenario_year,
                     goal   = goal,
                     method = method,
                     reference_point = ref_pt))
  write_csv(ref_pts, "temp/reference_pts.csv")
  
}



FIS = function(layers){
  
  scen_year <- layers$data$scenario_year
  
  data_year <- conf$scenario_data_years %>%
    filter(layer_name == "fis_meancatch") %>%
    filter(scenario_year == scen_year) %>%
    .$data_year
  
  data_year <- as.numeric(data_year)
  
  #catch data
  c <- layers$data$fis_meancatch %>%
    select(rgn_id, year, stock_id_taxonkey, catch = mean_catch)
  
  # b_bmsy data
  b = layers$data$fis_b_bmsy %>%
    select(rgn_id, stock_id, year, bbmsy)
  
  # The following stocks are fished in multiple regions and have high b/bmsy values
  # Due to the underfishing penalty, this actually penalizes the regions that have the highest
  # proportion of catch of these stocks.  The following corrects this problem:
  #  filter(b, stock_id %in% c('Katsuwonus_pelamis-71', 'Clupea_harengus-27', 'Trachurus_capensis-47'))
  
  high_bmsy <- c('Katsuwonus_pelamis-71', 'Clupea_harengus-27', 'Trachurus_capensis-47', 'Sardinella_aurita-34', 'Scomberomorus_cavalla-31')
  
  b <- b %>%
    mutate(bbmsy = ifelse(stock_id %in% high_bmsy, 1, bbmsy))
  
  
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
    mutate(bbmsy = as.numeric(bbmsy)) %>%
    mutate(rgn_id = as.numeric(as.character(rgn_id))) %>%
    mutate(year = as.numeric(as.character(year))) %>%
    mutate(stock_id = as.character(stock_id))  
  
  
  ####
  # STEP 1. Calculate scores for Bbmsy values
  ####
  #  *************NOTE *****************************
  #  These values can be altered
  #  ***********************************************
  alpha <- 0.5
  beta <- 0.25
  lowerBuffer <- 0.95
  upperBuffer <- 1.05
  
  b$score = ifelse(b$bbmsy < lowerBuffer, b$bbmsy,
                   ifelse (b$bbmsy >= lowerBuffer & b$bbmsy <= upperBuffer, 1, NA))
  b$score = ifelse(!is.na(b$score), b$score,  
                   ifelse(1 - alpha*(b$bbmsy - upperBuffer) > beta,
                          1 - alpha*(b$bbmsy - upperBuffer), 
                          beta))
  
  
  ####
  # STEP 1. Merge the b/bmsy data with catch data
  ####
  data_fis <- c %>%
    left_join(b, by=c('rgn_id', 'stock_id', 'year')) %>%
    select(rgn_id, stock_id, year, taxon_key, catch, bbmsy, score) 
  
  
  ###
  # STEP 2. Estimate scores for taxa without b/bmsy values
  # Median score of other fish in the region is the starting point
  # Then a penalty is applied based on the level the taxa are reported at
  ###
  
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
    filter(year == data_year)
  write.csv(gap_fill_data, 'temp/FIS_summary_gf.csv', row.names=FALSE)
  
  status_data <- data_fis_gf %>%
    select(region_id=rgn_id, stock_id, year, catch, score)
  
  
  ###
  # STEP 4. Calculate status for each region
  ###
  
  # 4a. To calculate the weight (i.e, the relative catch of each stock per region),
  # the mean catch of taxon i is divided by the   
  # sum of mean catch of all species in region/year 
  
  status_data <- status_data %>%
    group_by(year, region_id) %>%
    mutate(SumCatch = sum(catch)) %>%
    ungroup() %>%
    mutate(wprop = catch/SumCatch)
  
  status_data <- status_data %>%
    group_by(region_id, year) %>%
    summarize(status = prod(score^wprop)) %>%
    ungroup()
  
  ###
  # STEP 5. Get yearly status and trend  
  ###
  
  status <-  status_data %>%
    filter(year==data_year) %>%
    mutate(
      score     = round(status*100, 1),
      dimension = 'status') %>%
    select(region_id, score, dimension)
  
  
  # calculate trend  
  
  trend_years <- (data_year-4):(data_year)
  
  trend <- trend_calc2(status_data=status_data, 
                      status_layer="fis_meancatch", trend_years=trend_years)
  
  ## reference points
  write_ref_pts(goal   = "FIS",
                method = "Functional relationship (B/Bmsy)",
                ref_pt = NA)
  
  # assemble dimensions
  scores <- rbind(status, trend) %>% 
    mutate(goal='FIS') %>%
    filter(region_id != 255)
  scores <- data.frame(scores)
  
  return(scores)
}

MAR = function(layers){

    scen_year <- layers$data$scenario_year
  
  mar_data_year <- conf$scenario_data_years %>%
    filter(layer_name=="mar_harvest_tonnes") %>%
    filter(scenario_year == scen_year) %>%
    .$data_year
  
  harvest_tonnes <- layers$data$mar_harvest_tonnes %>%
    select(-layer)
  
  sustainability_score <- layers$data$mar_sustainability_score %>%
    select(rgn_id, taxa_code, sust_coeff)
  
  popn_inland25mi <- layers$data$mar_coastalpopn_inland25mi %>%
    select(rgn_id, year, popsum) %>%
    mutate(popsum = popsum + 1)  # so 0 values do not cause errors when logged
  
  
  rky <-  harvest_tonnes %>%
    left_join(sustainability_score, by = c('rgn_id', 'taxa_code'))%>%
    select(rgn_id, year, taxa_code, tonnes, sust_coeff)
  
  # fill in gaps with no data
  rky <- spread(rky, year, tonnes)
  rky <- gather(rky, "year", "tonnes", -(1:3)) %>%
    mutate(year = as.numeric(year))
  
  # 4-year rolling mean of data
  m <- rky %>%
    group_by(rgn_id, taxa_code, sust_coeff) %>%
    arrange(rgn_id, taxa_code, year) %>%
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

  ref_95pct <- quantile(ry$mar_pop, 0.95, na.rm=TRUE)
  
  # reference information
  ry_ref = ry %>%
    arrange(mar_pop) %>%
    filter(mar_pop >= ref_95pct)
  
  write_ref_pts(goal = "MAR", method = "spatial 95th quantile",
                ref_pt=paste0("region id: ", ry_ref$rgn_id[1], ' value: ', ref_95pct))
  
  ry = ry %>%
    mutate(status = ifelse(mar_pop / ref_95pct > 1,
                           1, 
                           mar_pop / ref_95pct)) 
  status <- ry %>%
    filter(year == mar_data_year) %>%
    mutate(dimension = "status") %>%
    select(region_id=rgn_id, score=status, dimension) %>%
    mutate(score = round(score*100, 2))
  
  
  # calculate trend  
  
  trend_years <- (mar_data_year-4):(mar_data_year)
  
  trend <- trend_calc(status_data=ry, trend_years = trend_years)
  
  
  # return scores
  scores = rbind(status, trend) %>%
    mutate(goal='MAR')
  
  return(scores)
}


FP = function(layers, scores){
  
  scen_year <- layers$data$scenario_year
  
  w <- get_data_year(layer_nm = "fp_wildcaught_weight", layers=layers)%>%
    filter(scenario_year == scen_year) %>%
    select(region_id = rgn_id, w_fis)
  
  # scores
  s <- scores %>%
    filter(goal %in% c('FIS', 'MAR')) %>%
    filter(!(dimension %in% c('pressures', 'resilience'))) %>%
    left_join(w, by="region_id")  %>%
    mutate(w_mar = 1 - w_fis) %>%
    mutate(weight = ifelse(goal == "FIS", w_fis, w_mar)) 
  
  
  ## Some warning messages due to potential mismatches in data: 
  # NA score but there is a weight
  tmp <- filter(s, goal=='FIS' & is.na(score) & (!is.na(w_fis) & w_fis!=0) & dimension == "score")
  if(dim(tmp)[1]>0){
    warning(paste0("Check: these regions have a FIS weight but no score: ", 
                   paste(as.character(tmp$region_id), collapse = ", ")))}
  
  tmp <- filter(s, goal=='MAR' & is.na(score) & (!is.na(w_mar) & w_fis!=0) & dimension == "score")
  if(dim(tmp)[1]>0){
    warning(paste0("Check: these regions have a MAR weight but no score: ", 
                   paste(as.character(tmp$region_id), collapse = ", ")))}
  
  # score, but the weight is NA or 0
  tmp <- filter(s, goal=='FIS' & (!is.na(score) & score > 0) & (is.na(w_fis) | w_fis==0) & dimension == "score" & region_id !=0)
  if(dim(tmp)[1]>0){
    warning(paste0("Check: these regions have a FIS score but no weight: ", 
                   paste(as.character(tmp$region_id), collapse = ", ")))}
  
  tmp <- filter(s, goal=='MAR' & (!is.na(score) & score > 0) & (is.na(w_mar) | w_mar==0) & dimension == "score" & region_id !=0)
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


AO = function(layers){
  
  Sustainability=1.0
  
  scen_year <- layers$data$scenario_year
  
  r <- get_data_year(layer_nm="ao_access", layers=layers) %>%
    rename(region_id = rgn_id, access = value)
  
  r <- na.omit(r)
  
  ry <- get_data_year(layer_nm = "ao_need", layers=layers) %>%
    rename(region_id = rgn_id, need=value) %>%
    left_join(r, by=c("region_id", "scenario_year"))
  
  # model
  ry <- ry %>%
    mutate(Du = (1 - need) * (1 - access)) %>%
    mutate(status = (1 - Du) * Sustainability)
  
  # status
  r.status <- ry %>%
    filter(scenario_year==scen_year) %>%
    select(region_id, status) %>%
    mutate(status=status*100) %>%
    select(region_id, score=status) %>%
    mutate(dimension='status') 
  
  # trend (in this case the trend years are based on need years)
  recent_trend_year <- ry %>%
    select(scenario_year, ao_need_year) %>%
    unique() 
  
  recent_trend_year <- recent_trend_year$ao_need_year[recent_trend_year$scenario_year==scen_year]
  
  trend_years <- (recent_trend_year-4):(recent_trend_year)
  
  r.trend <- trend_calc2(status_data=ry, status_layer="ao_need", trend_years=trend_years)
  
  ## reference points
  write_ref_pts(goal   = "AO",
                method = "XXXXXXXX",
                ref_pt = NA)
  
  # return scores
  scores = rbind(r.status, r.trend) %>%
    mutate(goal='AO') 
  
  return(scores)  
}

NP <- function(scores, layers){
  
  scen_year <- layers$data$scenario_year
  
  data_year <- conf$scenario_data_years %>%
    filter(layer_name == "np_harvest_tonnes") %>%
    filter(scenario_year == scen_year) %>%
    .$data_year
  
  data_year <- as.numeric(data_year)
  
  
  r_cyanide    = layers$data[['np_cyanide']] # cyanide & blast used to calculate risk variable
  r_blast      = layers$data[['np_blast']]   
  hab_rocky    = layers$data[['hab_rockyreef_extent']]
  hab_coral    = layers$data[['hab_coral_extent']] # habitat data used to calculate exposure variable
  
  ### FIS status for fish oil sustainability
  # FIS_status <- read.csv('scores.csv')%>%  ## this is for troubleshooting
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
        tonnes, tonnes_rel, prod_weight)
    
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
    hab_coral <- hab_coral %>%
      select(rgn_id, km2)
    hab_rocky   <- hab_rocky %>%
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
      left_join(fish_oil_sust, by='rgn_id') %>%
      mutate(product = as.character(product)) 
    
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
      select(rgn_name, rgn_id, year, product, product_status, prod_weight) %>%
      group_by(rgn_id, year) %>%
      summarize(status = weighted.mean(product_status, prod_weight)) %>%
      filter(!is.na(status)) %>% # 1/0 produces NaN
      ungroup()
    
    ### get current status
    np_status_current <- np_status_all %>%
      filter(year == data_year & !is.na(status)) %>%
      mutate(
        dimension = 'status',
        score     = round(status,4) * 100) %>%
      select(region_id = rgn_id, dimension, score)
    stopifnot(
      min(np_status_current$score, na.rm = TRUE) >= 0, 
      max(np_status_current$score, na.rm = TRUE) <= 100)
    
    ### trend 
    
    trend_years <- (data_year-4):(data_year)
    
    np_trend <- trend_calc2(status_data=np_status_all, trend_years = trend_years)
    
    ### return scores
    np_scores <- np_status_current %>%
      full_join(np_trend, by=c('region_id', 'dimension', 'score')) %>%
      mutate(goal = 'NP') %>%
      select(goal, dimension, region_id, score) %>%
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
  write_ref_pts(goal = "NP", method = "Harvest peak within region times 0.65 buffer", 
                ref_pt = "varies for each region")
  
  return(np_scores)
}


CS <- function(layers){
  scen_year <- layers$data$scenario_year
  
  # layers for carbon storage
  extent_lyrs <- c('hab_mangrove_extent', 'hab_seagrass_extent', 'hab_saltmarsh_extent')
  health_lyrs <- c('hab_mangrove_health', 'hab_seagrass_health', 'hab_saltmarsh_health')
  trend_lyrs <- c('hab_mangrove_trend', 'hab_seagrass_trend', 'hab_saltmarsh_trend')
  
  # get data together:  
  extent <- SelectData2(extent_lyrs) %>%
    filter(!(habitat %in% c("mangrove_inland1km", "mangrove_offshore"))) %>%
    filter(scenario_year == scen_year) %>%
    select(region_id = rgn_id, habitat, extent=km2) %>%
    mutate(habitat = as.character(habitat))
  
  health <- SelectData2(health_lyrs) %>%
    filter(!(habitat %in% c("mangrove_inland1km", "mangrove_offshore"))) %>%
    filter(scenario_year == scen_year) %>%
    select(region_id = rgn_id, habitat, health) %>%
    mutate(habitat = as.character(habitat))
  
  trend <- SelectData2(trend_lyrs)%>%
    filter(!(habitat %in% c("mangrove_inland1km", "mangrove_offshore"))) %>%
    filter(scenario_year == scen_year) %>%
    select(region_id = rgn_id, habitat, trend) %>%
    mutate(habitat = as.character(habitat))
  
  ## join layer data
  d <-  extent %>%
    full_join(health, by=c("region_id", "habitat")) %>%
    full_join(trend, by=c("region_id", "habitat"))
  
  ## set ranks for each habitat
  habitat.rank <- c('mangrove'         = 139,
                    'saltmarsh'        = 210,
                    'seagrass'         = 83)
  
  ## limit to CS habitats and add rank
  d <- d %>%
    mutate(
      rank = habitat.rank[habitat],
      extent = ifelse(extent==0, NA, extent))
  
  # status
  status <- d %>%
    filter(!is.na(rank) & !is.na(health) & !is.na(extent)) %>%
    group_by(region_id) %>%
    summarize(
      score = pmin(1, sum(rank * health * extent, na.rm=TRUE) / (sum(extent * rank, na.rm=TRUE)) ) * 100,
      dimension = 'status') %>%
    ungroup()
  
  # trend
  
  trend <- d %>%
    filter(!is.na(rank) & !is.na(trend) & !is.na(extent)) %>%
    group_by(region_id) %>%
    summarize(
      score = sum(rank * trend * extent, na.rm=TRUE) / (sum(extent*rank, na.rm=TRUE)),
      dimension = 'trend') %>%
    ungroup()
  
  
  scores_CS <- rbind(status, trend)  %>%
    mutate(goal = 'CS') %>%
    select(goal, dimension, region_id, score)
  
  ## reference points
  write_ref_pts(goal = "CS", 
                method= "Health/condition variable based on current vs. historic extent", 
                ref_pt = "varies for each region/habitat")
  
  ## create weights file for pressures/resilience calculations  
  weights <- extent %>%  
    filter(extent > 0) %>%
    mutate(rank = habitat.rank[habitat]) %>%
    mutate(extent_rank = extent*rank) %>%
    mutate(layer = "element_wts_cs_km2_x_storage") %>%
    select(rgn_id=region_id, habitat, extent_rank, layer)
  
  layers$data$element_wts_cs_km2_x_storage <- weights
  
  
  # return scores
  return(scores_CS)
}  



CP <- function(layers){
  ## read in layers
  scen_year <- layers$data$scenario_year
  
  # layers for coastal protection
  extent_lyrs <- c('hab_mangrove_extent', 'hab_seagrass_extent', 'hab_saltmarsh_extent', 'hab_coral_extent', 'hab_seaice_extent')
  health_lyrs <- c('hab_mangrove_health', 'hab_seagrass_health', 'hab_saltmarsh_health', 'hab_coral_health', 'hab_seaice_health')
  trend_lyrs <- c('hab_mangrove_trend', 'hab_seagrass_trend', 'hab_saltmarsh_trend', 'hab_coral_trend', 'hab_seaice_trend')
  
  # get data together:  
  extent <- SelectData2(extent_lyrs) %>%
    filter(!(habitat %in% "seaice_edge")) %>%
    filter(scenario_year == scen_year) %>%
    select(region_id = rgn_id, habitat, extent=km2) %>%
    mutate(habitat = as.character(habitat))
  
  health <- SelectData2(health_lyrs) %>%
    filter(!(habitat %in% "seaice_edge")) %>%
    filter(scenario_year == scen_year) %>%
    select(region_id = rgn_id, habitat, health) %>%
    mutate(habitat = as.character(habitat))
  
  trend <- SelectData2(trend_lyrs) %>%
    filter(!(habitat %in% "seaice_edge")) %>%
    filter(scenario_year == scen_year) %>%
    select(region_id = rgn_id, habitat, trend) %>%
    mutate(habitat = as.character(habitat))
  
  ## sum mangrove_offshore + mangrove_inland1km = mangrove to match with extent and trend
  mangrove_extent <- extent %>%
    filter(habitat %in% c('mangrove_inland1km','mangrove_offshore')) 
  
  if (nrow(mangrove_extent) > 0){
    mangrove_extent <- mangrove_extent %>%
      group_by(region_id) %>%
      summarize(extent = sum(extent, na.rm = TRUE)) %>%
      mutate(habitat='mangrove') %>%
      ungroup() 
  }
  
  extent <- extent %>%
    filter(!habitat %in% c('mangrove','mangrove_inland1km','mangrove_offshore')) %>%  #do not use all mangrove
    rbind(mangrove_extent)  #just the inland 1km and offshore
  
  ## join layer data
  d <-  extent %>%
    full_join(health, by=c("region_id", "habitat")) %>%
    full_join(trend, by=c("region_id", "habitat"))
  
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
      extent = ifelse(extent==0, NA, extent))
  
  
  # status
  scores_CP <- d %>%
    filter(!is.na(rank) & !is.na(health) & !is.na(extent)) %>%
    group_by(region_id) %>%
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
        group_by(region_id) %>%
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
  
  ## finalize scores_CP
  scores_CP <- scores_CP %>%
    mutate(
      goal = 'CP') %>%
    select(region_id, goal, dimension, score)
  
  ## reference points
  write_ref_pts(goal = "CP", 
                method= "Health/condition variable based on current vs. historic extent", 
                ref_pt = "varies for each region/habitat")
  
  ## create weights file for pressures/resilience calculations  
  
  weights <- extent %>%
    filter(extent > 0) %>%
    mutate(rank = habitat.rank[habitat]) %>%
    mutate(extent_rank = extent*rank) %>%
    mutate(layer = "element_wts_cp_km2_x_protection") %>%
    select(rgn_id=region_id, habitat, extent_rank, layer)
  
  layers$data$element_wts_cp_km2_x_protection <- weights
  
  # return scores
  return(scores_CP)
  
}

TR = function(layers) {
  
  ## formula:
  ##  E   = Ep                         # Ep: % of direct tourism jobs. tr_jobs_pct_tourism.csv
  ##  S   = (S_score - 1) / (7 - 1)    # S_score: raw TTCI score, not normalized (1-7). tr_sustainability.csv
  ##  Xtr = E * S
  
  pct_ref = 90
  
  scen_year <- layers$data$scenario_year
  
  
  ## read in layers
  
  tourism <- get_data_year(layer_nm = "tr_jobs_pct_tourism", layers=layers) %>%
    select(-layer_name)
  sustain <- get_data_year(layer_nm = "tr_sustainability", layers=layers) %>%
    select(-layer_name)
  
  tr_data  <- full_join(tourism, sustain, by = c('rgn_id', 'scenario_year'))
  
  tr_model <- tr_data %>%
    mutate(
      E   = Ep,
      S   = (S_score - 1) / (7 - 1), # scale score from 1 to 7.
      Xtr = E * S )
  
  
  # regions with Travel Warnings
  rgn_travel_warnings <- get_data_year(layer_nm = "tr_travelwarnings", layers=layers) %>%
    select(-layer_name)
  
  ## incorporate Travel Warnings
  tr_model <- tr_model %>%
    left_join(rgn_travel_warnings, by = c('rgn_id', 'scenario_year')) %>%
    mutate(Xtr = ifelse(!is.na(multiplier), multiplier * Xtr, Xtr)) %>%
    select(-multiplier)
  
  
  ### Calculate status based on quantile reference (see function call for pct_ref)
  tr_model <- tr_model %>%
    group_by(scenario_year) %>%
    mutate(Xtr_q = quantile(Xtr, probs = pct_ref/100, na.rm = TRUE)) %>%
    mutate(status  = ifelse(Xtr / Xtr_q > 1, 1, Xtr / Xtr_q)) %>% # rescale to qth percentile, cap at 1
    ungroup()
  
  
  ## reference points
  ref_point <- tr_model %>%
    filter(scenario_year == scen_year) %>%
    select(Xtr_q) %>%
    unique() %>%
    data.frame() %>%
    .$Xtr_q
  
  write_ref_pts(goal = "TR",
                method= paste0('spatial: ', as.character(pct_ref), "th quantile"),
                ref_pt = as.character(ref_point))
  
  # get status
  tr_status <- tr_model %>%
    filter(scenario_year == scen_year) %>%
    select(region_id = rgn_id, score = status) %>%
    mutate(score = score*100) %>%
    mutate(dimension = 'status')
  
  
  # calculate trend (rooted in jobs data)
  data_year <- conf$scenario_data_years %>%
    filter(scenario_year == scen_year) %>%
    filter(layer_name == "tr_jobs_pct_tourism") %>%
    .$data_year
  
  
  trend_data <- tr_model %>%
    filter(!is.na(status))
  
  trend_years <- (data_year-4):(data_year)
  
  tr_trend <- trend_calc2(status_data = trend_data, status_layer = "tr_jobs_pct_tourism", trend_years=trend_years)
  
  
  # bind status and trend by rows
  tr_score <- bind_rows(tr_status, tr_trend) %>%
    mutate(goal = 'TR')
  
  
  # assign NA for uninhabitated islands
  if (conf$config$layer_region_labels=='rgn_global'){
    unpopulated = layers$data[['le_popn']] %>%
      group_by(rgn_id) %>%
      filter(count==0) %>%
      select(region_id = rgn_id)
    tr_score$score = ifelse(tr_score$region_id %in% unpopulated$region_id, NA, tr_score$score)
  }
  
  # return final scores
  scores = tr_score %>%
    select(region_id, goal, dimension, score)
  
  return(scores)
}

LIV_ECO = function(layers, subgoal){
  
  scen_year <- layers$data$scenario_year
  
  g.component = c('LIV'='livelihood','ECO'='economy')[[subgoal]]
  
  ## year data
  liv_workforcesize_year <- conf$scenario_data_years %>%
    filter(layer_name == "le_workforcesize_adj",
           scenario_year == scen_year) %>%
    .$data_year
  
  eco_rev_adj_min_year <- conf$scenario_data_years %>%  # values above this are selected and
    filter(layer_name == "le_revenue_adj",              # the most recent value is selected among these
           scenario_year == scen_year) %>%
    .$data_year
  
  # get data
  
  le_layers <- c('le_jobs_cur_base_value','le_jobs_ref_base_value','le_jobs_cur_adj_value','le_jobs_ref_adj_value',
                 'le_rev_cur_base_value','le_rev_ref_base_value','le_rev_cur_adj_value','le_rev_ref_adj_value',
                 'le_wage_cur_base_value','le_wage_ref_base_value','le_wage_cur_adj_value','le_wage_ref_adj_value')
  
  status_model_long <- SelectData2(layer_names=le_layers)
  
  
  status_model = status_model_long %>%
    filter(scenario_year == scen_year) %>%
    mutate(metric = str_replace(layer_name, 'le_(jobs|rev|wage)_(.*)', '\\1'),
           field  = str_replace(layer_name, 'le_(jobs|rev|wage)_(.*)', '\\2')) %>%
    select(metric, field, cntry_key, sector, value) %>%
    spread(field, value)
  
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
  # e.g., for US score: avg. USA main, Alaska, Hawaii weighted by rev, workforce, etc.
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
  
  ## trend layers for different scenario years include different years of data (<= 6 years for trend analysis)
  ## This will be revised when we update LE
  a_year <- ifelse(scen_year==2012, 2012, 2013)
  
  le_unemployment     = layers$data[['le_unemployment']]
  le_gdp              = layers$data[['le_gdp']]
  le_jobs_sector_year = layers$data[['le_jobs_sector_year']] %>%
    filter(analysis_year == a_year) %>%
    select(-analysis_year)
  le_rev_sector_year  = layers$data[['le_rev_sector_year']] %>%
    filter(analysis_year == a_year) %>%
    select(-analysis_year)
  le_wage_sector_year = layers$data[['le_wage_sector_year']] %>%
    filter(analysis_year == a_year) %>%
    select(-analysis_year)
  
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

LE = function(scores, layers){
  
  scores.LE = scores %>% 
    filter(goal %in% c('LIV','ECO') & dimension %in% c('status','trend','score','future')) %>%
    spread(goal, score) %>%
    mutate(score = rowMeans(cbind(ECO, LIV), na.rm = TRUE)) %>%
    select(region_id, dimension, score) %>%
    mutate(goal  = 'LE')
  
  # rbind to all scores
  scores = scores %>%
    rbind(scores.LE)  
  
  # LIV, ECO and LE: nullify unpopulated regions and those of the Southern Ocean Islands
  r_s_islands   <-   layers$data$rgn_georegions %>%
    filter(level=="r2" & georgn_id==999) %>%
    .$rgn_id
  
  r_unpopulated <- layers$data$le_popn %>%
    group_by(rgn_id) %>%
    arrange(rgn_id, year) %>%
    summarize(count=last(count)) %>%
    filter(count==0) %>%
    .$rgn_id
  
  NA_rgns <- union(r_s_islands, r_unpopulated) 
  
  scores <- scores %>%
    mutate(score = ifelse(goal %in% c('LIV', 'ECO', 'LE') 
                          & !(dimension %in% c('pressures', 'resilience')) & 
                            region_id %in% NA_rgns, 
                          NA, score))
  # scores[with(scores, 
  #            goal %in% c('LIV','ECO','LE') & 
  #              !dimension %in% c('pressures','resilience') & 
  #              region_id %in% union(r_s_islands, r_unpopulated)),
  #       'score'] = NA
  
  # return scores
  return(scores)  
}

ICO = function(layers){

    scen_year <- layers$data$scenario_year
  
  rk <- get_data_year(layer_nm="ico_spp_iucn_status", layers=layers) %>%
    select(region_id = rgn_id, sciname, iucn_cat=category, scenario_year, ico_spp_iucn_status_year) %>%
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
    group_by(region_id, sciname, scenario_year, ico_spp_iucn_status_year) %>%
    summarize(spp_mean = mean(status_score, na.rm=TRUE)) %>%
    ungroup()
  
  # STEP 2: take mean of populations within regions
  r.status <- r.status_spp %>%
    group_by(region_id, scenario_year, ico_spp_iucn_status_year) %>%
    summarize(status = mean(spp_mean, na.rm=TRUE)) %>%
    ungroup() 
  
  ####### trend
  
  data_year <- conf$scenario_data_years %>%
    filter(scenario_year == scen_year) %>%
    filter(layer_name == "ico_spp_iucn_status") %>%
    .$data_year
  
  
  trend_years <- (data_year-9):(data_year)
  
  r.trend <- trend_calc(status_data = r.status, trend_years=trend_years)
  
  
  ####### status
  r.status <- r.status %>%
    filter(scenario_year == scen_year) %>%
    mutate(score = status * 100) %>%
    mutate(dimension = "status") %>%
    select(region_id, score, dimension)
  
  ## reference points
  write_ref_pts(goal = "ICO", method = "scaled IUCN risk categories",
                ref_pt = NA)
  
  # return scores
  scores <-  rbind(r.status, r.trend) %>%
    mutate('goal'='ICO') %>%
    select(goal, dimension, region_id, score) %>%
    data.frame()
  
  ## Gapfill Oecussi Ambeno (rgn 237) with East Timor (rgn 231) data
  ## Oecussi Ambeno is an enclave within East Timor, so the data should be very similar
  go <- filter(scores, region_id == 231) %>%
    mutate(region_id = 237)
  scores <- rbind(scores, go)
  
  
  ## gapfill missing regions with average scores/trends of regions that share same UN geopolitical region 
  un_regions <- georegions %>%
    select(region_id=rgn_id, r2)
  # ID missing regions:
  regions <- SelectLayersData(layers, layers=c('rgn_global')) %>%
    select(region_id = id_num)
  regions_NA <- setdiff(regions$region_id, scores$region_id)
  scores_NA <- data.frame(goal="ICO", 
                          dimension=rep(c("status", "trend"), 
                                        each=length(regions_NA)), 
                          region_id=regions_NA, score=NA)
  
  scores <- scores %>%
    rbind(scores_NA) %>%
    mutate(region_id = as.numeric(region_id)) %>%
    left_join(un_regions, by="region_id") %>%
    group_by(dimension, r2) %>%
    mutate(score_gf = mean(score, na.rm=TRUE)) %>%
    arrange(dimension, region_id) %>%
    data.frame()
  
  # save gapfilling records
  scores_gf <- scores %>%
    mutate(gapfilled = ifelse(is.na(score) & !is.na(score_gf), "1", "0")) %>%
    mutate(method = ifelse(is.na(score) & !is.na(score_gf), "UN geopolitical avg. (r2)", NA)) %>%
    select(goal, dimension, region_id, gapfilled, method) 
  write.csv(scores_gf, "temp/ICO_status_trend_gf.csv", row.names=FALSE)  
  
  scores <- scores %>%
    mutate(score2 = ifelse(is.na(score), score_gf, score)) %>%
    select(goal, dimension, region_id, score=score2) %>%
    data.frame()
  
  return(scores)  
  
}

LSP = function(layers){
 
  scen_year <- layers$data$scenario_year
  
  ref_pct_cmpa=30
  ref_pct_cp=30
  
  # select data
  r <-  rbind(layers$data$rgn_area_inland1km, layers$data$rgn_area_offshore3nm) %>% #total offshore/inland areas
    select(region_id = rgn_id, area, layer) %>%
    spread(layer, area) %>%
    select(region_id, area_inland1km = rgn_area_inland1km,
           area_offshore3nm = rgn_area_offshore3nm)
  
  ry_offshore <-  layers$data$lsp_prot_area_offshore3nm %>%
    select(region_id = rgn_id, year, cmpa = a_prot_3nm)
  ry_inland <- layers$data$lsp_prot_area_inland1km %>%
    select(region_id = rgn_id, year, cp = a_prot_1km)
  
  ry <- full_join(ry_offshore, ry_inland, by=c("region_id", "year"))
  
  # fill in time series for all regions 
  r.yrs <- expand.grid(region_id = unique(ry$region_id),
                       year = unique(ry$year)) %>%
    left_join(ry, by=c('region_id', 'year')) %>%
    arrange(region_id, year) %>%
    mutate(cp= ifelse(is.na(cp), 0, cp),
           cmpa = ifelse(is.na(cmpa), 0, cmpa)) %>%
    mutate(pa     = cp + cmpa) %>%
    rename(data_year = year)
  
  ## associate data_years with scenario years for these data
  year_data <- conf$scenario_data_years %>%
    filter(layer_name == "lsp_prot_area_offshore3nm") %>%
    select(scenario_year, data_year)
  
  r.yrs <- left_join(r.yrs, year_data, by="data_year") %>%
    filter(!is.na(scenario_year))
  
  # get percent of total area that is protected for inland1km (cp) and offshore3nm (cmpa) per year
  # and calculate status score
  r.yrs <- r.yrs %>%
    full_join(r, by="region_id") %>%
    mutate(pct_cp    = pmin(cp   / area_inland1km   * 100, 100),
           pct_cmpa  = pmin(cmpa / area_offshore3nm * 100, 100),
           status    = ( pmin(pct_cp / ref_pct_cp, 1) + pmin(pct_cmpa / ref_pct_cmpa, 1) ) / 2) %>%
    filter(!is.na(status))
  
  # extract status based on specified year
  
  r.status = r.yrs %>%
    filter(scenario_year==scen_year) %>%
    mutate(score=status*100) %>%
    select(region_id, score) %>%
    mutate(dimension = "status")
  
  # calculate trend
  
  
  trend_years <- (scen_year-4):(scen_year)
  
  r.trend <- trend_calc(status_data = r.yrs, trend_years=trend_years)
  
  
  ## reference points
  write_ref_pts(goal = "LSP", method = paste0(ref_pct_cmpa, "% marine protected area; ",
                                              ref_pct_cp, "% coastal protected area"),
                ref_pt = "varies by area of region's eez and 1 km inland")
  
  # return scores
  scores = bind_rows(r.status, r.trend) %>%
    mutate(goal = "LSP")
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
  
  scen_year <- layers$data$scenario_year
  
  ### function to calculate geometric mean:  
  geometric.mean2 <- function (x, na.rm = TRUE) {
    if (is.null(nrow(x))) {
      exp(mean(log(x), na.rm = TRUE))
    }
    else {
      exp(apply(log(x), 2, mean, na.rm = na.rm))
    }
  }
  
  
  # layers
  trend_lyrs <- c('cw_chemical_trend', 'cw_nutrient_trend', 'cw_trash_trend', 'cw_pathogen_trend')
  prs_lyrs <- c('po_pathogens', 'po_nutrients_3nm', 'po_chemicals_3nm', 'po_trash')
  
  # get data together:  
  prs_data <- SelectData2(prs_lyrs) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    select(region_id = rgn_id, value=pressure_score)
  
  d_pressures <- prs_data %>%
    mutate(pressure = 1 - value) %>%  # invert pressures
    group_by(region_id) %>%
    summarize(score = geometric.mean2(pressure, na.rm=TRUE)) %>% # take geometric mean
    mutate(score = score * 100) %>%
    mutate(dimension = "status") %>%
    ungroup()
  
  
  # get trend data together:  
  trend_data <- SelectData2(trend_lyrs) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    select(region_id = rgn_id, value=trend)
  
  d_trends <- trend_data %>%
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
  write_ref_pts(goal   = "CW",
                method = "spatial: pressures scaled from 0-1 at raster level",
                ref_pt = NA)
  
  return(scores)  
}


HAB = function(layers){
  
  scen_year <- layers$data$scenario_year
  
  
  extent_lyrs <- c('hab_mangrove_extent', 'hab_seagrass_extent', 'hab_saltmarsh_extent', 'hab_coral_extent', 'hab_seaice_extent',
                   'hab_softbottom_extent')
  health_lyrs <- c('hab_mangrove_health', 'hab_seagrass_health', 'hab_saltmarsh_health', 'hab_coral_health', 'hab_seaice_health',
                   'hab_softbottom_health')
  trend_lyrs <- c('hab_mangrove_trend', 'hab_seagrass_trend', 'hab_saltmarsh_trend', 'hab_coral_trend', 'hab_seaice_trend',
                  'hab_softbottom_trend')
  
  # get data together:  
  extent <- SelectData2(extent_lyrs) %>%
    filter(scenario_year == scen_year) %>%
    select(region_id = rgn_id, habitat, extent=km2) %>%
    mutate(habitat = as.character(habitat))
  
  health <- SelectData2(health_lyrs) %>%
    filter(scenario_year == scen_year) %>%
    select(region_id = rgn_id, habitat, health) %>%
    mutate(habitat = as.character(habitat))
  
  trend <- SelectData2(trend_lyrs) %>%
    filter(scenario_year == scen_year) %>%
    select(region_id = rgn_id, habitat, trend) %>%
    mutate(habitat = as.character(habitat))
  
  
  # join and limit to HAB habitats
  d <- health %>%
    full_join(trend, by = c('region_id', 'habitat')) %>%
    full_join(extent, by = c('region_id', 'habitat')) %>%
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
    group_by(region_id) %>%
    filter(!is.na(health)) %>%
    summarize(      
      score = pmin(1, sum(health) / sum(w)) * 100,
      dimension = 'status') %>%
    ungroup()
  
  trend <- d %>%
    group_by(region_id) %>%
    filter(!is.na(trend)) %>%      
    summarize(      
      score =  sum(trend) / sum(w),
      dimension = 'trend')  %>%
    ungroup()
  
  scores_HAB <- rbind(status, trend) %>%
    mutate(goal = "HAB") %>%
    select(region_id, goal, dimension, score)
  
  ## reference points
  write_ref_pts(goal = "HAB", 
                method= "Health/condition variable based on current vs. historic extent", 
                ref_pt = "varies for each region/habitat")
  
  ## create weights file for pressures/resilience calculations  
  
  weights<- extent %>%
    filter(habitat %in% c('seagrass',
                          'saltmarsh',
                          'mangrove',
                          'coral',
                          'seaice_edge',
                          'soft_bottom')) %>%
    filter(extent > 0) %>%
    mutate(boolean = 1) %>%
    mutate(layer = "element_wts_hab_pres_abs") %>%
    select(rgn_id=region_id, habitat, boolean, layer)
  
  layers$data$element_wts_hab_pres_abs <- weights
  
  
  # return scores
  return(scores_HAB)  
}


SPP = function(layers){
  
  scores <- rbind(layers$data$spp_status, layers$data$spp_trend) %>%
    mutate(goal = 'SPP') %>%
    mutate(dimension = ifelse(layer=="spp_status", "status", "trend")) %>%
    mutate(score = ifelse(dimension == 'status', score*100, score)) %>%
    select(region_id=rgn_id, goal, dimension, score)
  
  ## reference points
  write_ref_pts(goal = "SPP", method = "Average of IUCN risk categories, scaled to historic extinction",
                ref_pt = NA)
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
  name_region_labels <- conf$config$layer_region_labels
  rgns <- layers$data[[name_region_labels]] %>%
    select(id_num = rgn_id, val_chr=label)
  
  # limit to just desired regions and global (region_id==0)
  scores = subset(scores, region_id %in% c(rgns[,'id_num'], 0))
  
  # apply NA to Antarctica
  id_ant = subset(rgns, val_chr=='Antarctica', id_num, drop = TRUE)
  scores[scores$region_id==id_ant, 'score'] = NA
  
  return(scores)
}

FinalizeScores = function(layers, conf, scores){
  
  # get regions
  name_region_labels <- conf$config$layer_region_labels
  rgns <- layers$data[[name_region_labels]] %>%
    select(id_num = rgn_id, val_chr=label)
  
  
  # add NAs to missing combos (region_id, goal, dimension)
  d = expand.grid(list(score_NA  = NA,
                       region_id = c(rgns[,'id_num'], 0),
                       dimension = c('pressures','resilience','status','trend','future','score'), 
                       goal      = c(conf$goals$goal, 'Index')), stringsAsFactors = FALSE); head(d)
  d = subset(d, 
             !(dimension %in% c('pressures','resilience','trend') & region_id==0) & 
               !(dimension %in% c('pressures','resilience','trend', 'status') & goal=='Index'))
  scores = merge(scores, d, all = TRUE)[,c('goal','dimension','region_id','score')]
  
  # order
  scores = arrange(scores, goal, dimension, region_id)
  
  # round scores
  scores$score = round(scores$score, 2)
  
  return(scores)
}
