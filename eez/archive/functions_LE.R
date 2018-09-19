LIV_ECO <- function(layers, subgoal) {
  scen_year <- layers$data$scenario_year
  
  g.component <- c('LIV' = 'livelihood', 'ECO' = 'economy')[[subgoal]]
  
  ## year data
  liv_workforcesize_year <- conf$scenario_data_years %>%
    filter(layer_name == "le_workforcesize_adj",
           scenario_year == scen_year) %>%
    .$data_year
  
  eco_rev_adj_min_year <-
    conf$scenario_data_years %>%  # values above this are selected and
    filter(layer_name == "le_revenue_adj",              # the most recent value is selected among these
           scenario_year == scen_year) %>%
    .$data_year
  
  # get data
  
  le_layers <-
    c(
      'le_jobs_cur_base_value',
      'le_jobs_ref_base_value',
      'le_jobs_cur_adj_value',
      'le_jobs_ref_adj_value',
      'le_rev_cur_base_value',
      'le_rev_ref_base_value',
      'le_rev_cur_adj_value',
      'le_rev_ref_adj_value',
      'le_wage_cur_base_value',
      'le_wage_ref_base_value',
      'le_wage_cur_adj_value',
      'le_wage_ref_adj_value'
    )
  
  status_model_long <- AlignManyDataYears(layer_names = le_layers)
  
  
  status_model <- status_model_long %>%
    filter(scenario_year == scen_year) %>%
    mutate(
      metric = str_replace(layer_name, 'le_(jobs|rev|wage)_(.*)', '\\1'),
      field  = str_replace(layer_name, 'le_(jobs|rev|wage)_(.*)', '\\2')
    ) %>%
    select(metric, field, cntry_key, sector, value) %>%
    spread(field, value)
  
  # get gdp per capita, at ppp
  ppp <- SelectLayersData(layers, layers = 'le_gdp_pc_ppp') %>%
    select(cntry_key = id_chr, year, usd = val_num)
  
  # country to region aggregation weight for livelihood
  workforce_adj <-
    SelectLayersData(layers, layers = 'le_workforcesize_adj') %>%
    select(cntry_key = id_chr, year, jobs = val_num)
  
  # country to region aggregation weight for economy
  rev_adj <- SelectLayersData(layers, layers = 'le_revenue_adj') %>%
    select(cntry_key = id_chr, year, usd = val_num)
  
  # compute the corrected relative value per metric per country, for JOBS
  status_jobs_rev <- status_model %>%
    filter(ref_base_value != 0 &
             ref_adj_value != 0 & metric %in% c('jobs', 'rev')) %>%
    group_by(metric, cntry_key) %>%
    summarise(score    = (
      sum(cur_base_value, na.rm = TRUE) / sum(ref_base_value, na.rm = TRUE)
    ) / (
      mean(cur_adj_value, na.rm = TRUE) / mean(ref_adj_value, na.rm = TRUE)
    ),
    n_sector = n()) %>%
    arrange(metric, cntry_key)
  
  # compute the corrected relative value per metric per country, for WAGE
  # 0. extract w'_i = (w_c/w_r)/(W_c/W_r) for each sector i per country
  t0 <- status_model %>%
    filter(metric == 'wage' &
             ref_base_value != 0 & ref_adj_value != 0) %>%
    mutate(w_prime_i = (cur_base_value / ref_base_value) / (cur_adj_value / ref_adj_value)) %>%
    select(metric, cntry_key, sector, w_prime_i) %>%
    group_by(metric, cntry_key) %>%
    summarise(w_prime  = mean(w_prime_i, na.rm = TRUE),
              n_sector = n()) %>%
    arrange(metric, cntry_key)
  
  # 1. let w' = unweighted mean(w'_i) across all sector i per country
  # 2. multiple w' by the most recent purchasing power parity (PPP) value for the country
  p <- ppp %>%
    arrange(cntry_key, year) %>%
    group_by(cntry_key) %>%
    summarise(year     = last(year),
              ppp_last = last(usd)) %>%
    filter(!is.na(ppp_last)) %>%
    arrange(cntry_key)
  t2 <- t0 %>%
    merge(p, by = 'cntry_key') %>%
    mutate(score = w_prime * ppp_last) %>%
    select(metric, cntry_key, score, n_sector) %>%
    arrange(metric, cntry_key)
  
  # 3. set the best country (PPP-adjusted average wage) equal to 1.0 and then rescale all countries to that max
  max_wage_score <- max(t2$score, na.rm = TRUE)
  status_wage <- t2 %>%
    mutate(score = score / max_wage_score)
  
  # combine the corrected relative values into a single status score
  status_model_combined <- ungroup(status_jobs_rev) %>%
    rbind(status_wage)
  
  status_score <- status_model_combined %>%
    # liv
    reshape2::dcast(cntry_key ~ metric, value.var = 'score') %>%
    group_by(cntry_key) %>%
    mutate(value     = mean(c(jobs, wage), na.rm = TRUE),
           component = 'livelihood') %>%
    select(cntry_key, component, value) %>%
    ungroup() %>%
    arrange(cntry_key, component, value) %>%
    # eco
    rbind(
      status_model_combined %>%
        filter(metric == 'rev') %>%
        mutate(value     = score,
               component = 'economy') %>%
        select(cntry_key, component, value)
    ) %>%
    # order
    filter(!is.na(value)) %>%
    arrange(cntry_key, component) %>%
    # clamp
    mutate(score = pmin(value, 1))
  
  # countries to regions
  cntry_rgn <- layers$data$cntry_rgn %>%
    select(rgn_id, cntry_key) %>%
    merge(
      SelectLayersData(layers, layers = 'rgn_labels') %>%
        select(rgn_id = id_num, rgn_name = val_chr),
      by = 'rgn_id',
      all.x = TRUE
    ) %>%
    arrange(rgn_name, cntry_key) %>%
    select(rgn_id, rgn_name, cntry_key)
  
  if (conf$config$layer_region_labels == 'rgn_global') {
    # update country to region lookups
    # TODO: use name_to_rgn
    suppressWarnings({
      # Warning message: In rbind_list__impl(environment()) : Unequal factor levels: coercing to character
      cntry_rgn <- cntry_rgn %>%
        mutate(cntry_key = plyr::revalue(
          cntry_key,
          c(
            'SCG'            = 'MNE',
            # used to be Serbia (no coast) and Montenegro (has coast) in Nature 2012
            'Aruba'          = 'ABW',
            # ABW and ANT EEZs got split...
            'Bonaire'        = 'ANT',
            'Curacao'        = 'ANT',
            'Sint Eustatius' = 'ANT',
            'Saba'           = 'ANT',
            'Brunei'         = 'BRN',
            # Brunei new country in Malaysia
            'Malaysia'       = 'MYS'
          )
        )) %>%
        dplyr::bind_rows(
          data.frame(
            rgn_id = 221,
            rgn_name = 'Northern Saint-Martin',
            cntry_key = 'BLM'
          ),
          # BLM is Saint Barthélemy, included within Northern Saint-Martin (MAF)
          data.frame(
            rgn_id = 209,
            rgn_name =                'China',
            cntry_key = 'HKG'
          ),
          # add Hong Kong to China (CHN)
          data.frame(
            rgn_id = 209,
            rgn_name =                'China',
            cntry_key = 'MAC'
          )
        )  # add Macau to China (CHN)
    })
    cntry_landlocked <-
      c(
        'BDI',
        'BOL',
        'BWA',
        'CHE',
        'LUX',
        'MKD',
        'MWI',
        'PRY',
        'PSE',
        'SCG',
        'SVK',
        'SWZ',
        'TKM',
        'UGA',
        'ZMB'
      )
    
    # remove landlocked countries and check for any country codes still not matched
    status_score <-
      filter(status_score,!cntry_key %in% cntry_landlocked)
    if (sum(!status_score$cntry_key %in% cntry_rgn$cntry_key) != 0) {
      stop(sprintf(
        'LIV_ECO status missing country to region lookup for: %s.',
        paste(
          setdiff(status_score$cntry_key, cntry_rgn$cntry_key),
          collapse = ', '
        )
      ))
    }
  }
  
  # get weights, for 1) aggregating to regions and 2) georegionally gap filling
  weights <- workforce_adj %>%
    filter(year == liv_workforcesize_year) %>%
    select(cntry_key, w = jobs) %>%
    mutate(component = 'livelihood') %>%
    rbind(
      rev_adj %>%
        select(cntry_key, year, w = usd) %>%
        filter(year >= eco_rev_adj_min_year) %>%
        arrange(cntry_key, year) %>%
        group_by(cntry_key) %>%
        summarize(w = last(w)) %>%
        mutate(component = 'economy')
    )
  
  # aggregate countries to regions by weights
  # e.g., for US score: avg. USA main, Alaska, Hawaii weighted by rev, workforce, etc.
  s_r <- status_score %>%
    merge(cntry_rgn, by = 'cntry_key', all.x = TRUE) %>%
    merge(weights,
          by = c('cntry_key', 'component'),
          all.x = TRUE) %>%
    select(component, rgn_id, rgn_name, cntry_key, score, w) %>%
    arrange(component, rgn_name, cntry_key) %>%
    group_by(component, rgn_id, rgn_name) %>%
    summarize(
      cntry_w     = paste(cntry_key[!is.na(w)], collapse = ','),
      cntry_w_na  = paste(cntry_key[is.na(w)], collapse = ','),
      n           = n(),
      n_w_na      = sum(is.na(w)),
      score_w_avg = weighted.mean(score, w),
      score_avg   = mean(score),
      w_sum       = sum(w, na.rm = TRUE)
    ) %>%
    mutate(score = ifelse(!is.na(score_w_avg), score_w_avg, score_avg)) %>%
    ungroup()
  
  # setup georegions gapfill by region
  georegions <-
    SelectLayersData(layers, layers = 'rgn_georegions') %>%
    select(rgn_id = id_num,
           level = category,
           georgn_id = val_num) %>%
    reshape2::dcast(rgn_id ~ level, value.var = 'georgn_id')
  
  data <- s_r %>%
    filter(component == g.component) %>%
    as.data.frame() %>%
    select(rgn_id, score, w_sum)
  
  # georegional gap fill ----
  if (conf$config$layer_region_labels == 'rgn_global') {
    # georegional gapfill, and output gapfill_georegions attributes
    if (!file.exists('temp'))
      dir.create('temp', recursive = TRUE)
    csv <-
      sprintf('temp/eez2013_%s-status-gapfill-georegions.csv',
              subgoal)
    s_r_g <- gapfill_georegions(
      data              = data,
      fld_id            = 'rgn_id',
      fld_value         = 'score',
      fld_weight        = 'w_sum',
      georegions        = georegions,
      ratio_weights     = FALSE,
      georegion_labels  = NULL,
      r0_to_NA          = TRUE,
      attributes_csv    = csv
    )
  } else {
    s_r_g = data
  }
  
  status <- s_r_g %>%
    select(region_id = rgn_id, score) %>%
    mutate(goal      = subgoal,
           dimension = 'status',
           score     = score * 100) %>%
    arrange(region_id)
  
  # trend layers ----
  
  ## trend layers for different scenario years include different years of data (<= 6 years for trend analysis)
  ## This will be revised when we update LE
  a_year <- ifelse(scen_year == 2012, 2012, 2013)
  
  le_unemployment     <- layers$data$le_unemployment
  le_gdp              <- layers$data$le_gdp
  le_jobs_sector_year <- layers$data$le_jobs_sector_year %>%
    filter(analysis_year == a_year) %>%
    select(-analysis_year)
  le_rev_sector_year  <- layers$data$le_rev_sector_year %>%
    filter(analysis_year == a_year) %>%
    select(-analysis_year)
  le_wage_sector_year <- layers$data$le_wage_sector_year %>%
    filter(analysis_year == a_year) %>%
    select(-analysis_year)
  
  suppressWarnings({
    # Warning message: In rbind_list__impl(environment()) : Unequal factor levels: coercing to character
    #browser()
    
    # adjustments
    adjustments <- dplyr::bind_rows(
      le_unemployment %>%
        mutate(metric = 'jobs',
               value = 100 - percent) %>%
        select(metric, cntry_key, year, value),
      le_gdp %>%
        mutate(metric = 'rev') %>%
        select(metric, cntry_key, year, value = usd)
    )
    
    # metric-country-sector-year
    mcsy <- dplyr::bind_rows(
      le_jobs_sector_year %>%
        mutate(metric = 'jobs'),
      le_rev_sector_year %>%
        mutate(metric = 'rev'),
      le_wage_sector_year %>%
        mutate(metric = 'wage')
    ) %>%
      select(metric, cntry_key, sector, year, value)
    
  })
  
  # merge metric-country-sector-year with adjustments
  mcsy <- mcsy %>%
    select(metric, cntry_key, sector, year, base_value = value) %>%
    left_join(
      adjustments %>%
        select(metric, cntry_key, year, adj_value = value),
      by = c('metric', 'cntry_key', 'year')
    ) %>%
    mutate(adj_value = ifelse(metric == 'wage', 1, adj_value),
           value = base_value / adj_value) %>%
    arrange(metric, cntry_key, year)
  
  # trend per metric-country-sector, based on 5 intervals (6 years of data)
  mcs <-
    mcsy %>%
    # for clip-n-ship where cntry_key is one value, drops factor to integer so adding this bit
    mutate(cntry_key = as.character(cntry_key)) %>%
    filter(!is.na(value)) %>%
    group_by(metric, cntry_key, sector) %>%
    do(mdl = lm(value ~ year, data = .)) %>%
    summarize(
      metric    = metric,
      cntry_key = cntry_key,
      sector    = sector,
      trend     = max(-1, min(1, coef(mdl)[['year']] * 5))
    ) %>%
    # get sums for weight
    left_join(
      mcsy %>%
        filter(!is.na(value)) %>%
        group_by(metric, cntry_key, sector) %>%
        summarize(value_sum = sum(value)),
      by = c('metric', 'cntry_key', 'sector')
    )
  
  # trend per metric-country
  mc <- dplyr::bind_rows(
    # wage: simple average of sectors
    mcs %>%
      group_by(metric, cntry_key) %>%
      filter(metric == 'wage') %>%
      summarize(trend = mean(trend)),
    # jobs and rev: weighted average by total jobs or rev per sector
    mcs %>%
      group_by(metric, cntry_key) %>%
      filter(metric %in% c('jobs', 'rev')) %>%
      summarize(trend = weighted.mean(trend, value_sum))
  )
  
  # trend per goal-country
  gc <- dplyr::bind_rows(
    # LIV: avg(jobs, wage)
    mc %>%
      group_by(cntry_key) %>%
      filter(metric %in% c('jobs', 'wage') & !is.na(trend)) %>%
      summarize(score     = mean(trend),
                component = 'livelihood'),
    # ECO: rev
    mc %>%
      filter(metric %in% c('rev')) %>%
      mutate(component = 'economy',
             score     = trend)
  ) %>%
    select(component, cntry_key, score)
  
  # remove landlocked countries and check for any country codes still not matched
  if (conf$config$layer_region_labels == 'rgn_global') {
    gc <- filter(gc,!cntry_key %in% cntry_landlocked)
    if (sum(!gc$cntry_key %in% cntry_rgn$cntry_key) != 0) {
      stop(sprintf(
        'LIV_ECO trend missing country to region lookup for: %s.',
        paste(setdiff(gc$cntry_key, cntry_rgn$cntry_key), collapse = ', ')
      ))
    }
  }
  
  # aggregate countries to regions by weights
  gr <- gc %>%
    merge(cntry_rgn, by = 'cntry_key', all.x = TRUE) %>%
    merge(weights,
          by = c('cntry_key', 'component'),
          all.x = TRUE) %>%
    select(component, rgn_id, rgn_name, cntry_key, score, w) %>%
    arrange(component, rgn_name, cntry_key) %>%
    group_by(component, rgn_id, rgn_name) %>%
    summarize(
      cntry_w     = paste(cntry_key[!is.na(w)], collapse = ','),
      cntry_w_na  = paste(cntry_key[is.na(w)], collapse = ','),
      n           = n(),
      n_w_na      = sum(is.na(w)),
      score_w_avg = weighted.mean(score, w),
      score_avg   = mean(score),
      w_sum       = sum(w, na.rm = TRUE)
    ) %>%
    mutate(score = ifelse(!is.na(score_w_avg), score_w_avg, score_avg)) %>%
    ungroup() %>%
    filter(!is.na(rgn_id))
  
  data <- gr %>%
    filter(component == g.component) %>%
    as.data.frame() %>%
    select(rgn_id, score, w_sum)
  
  # georegional gap fill ----
  if (conf$config$layer_region_labels == 'rgn_global') {
    # georegional gapfill, and output gapfill_georegions attributes
    if (!file.exists('temp'))
      dir.create('temp', recursive = TRUE)
    csv <-
      sprintf('temp/eez2013_%s-trend-gapfill-georegions.csv', subgoal)
    rg <- gapfill_georegions(
      data              = data,
      fld_id            = 'rgn_id',
      fld_value         = 'score',
      fld_weight        = 'w_sum',
      georegions        = georegions,
      ratio_weights     = FALSE,
      georegion_labels  = NULL,
      r0_to_NA          = TRUE,
      attributes_csv    = csv
    )
    
  } else {
    rg <- data
  }
  
  trend <- rg %>%
    select(region_id = rgn_id, score) %>%
    mutate(goal      = subgoal,
           dimension = 'trend',
           score     = score) %>%
    arrange(region_id)
  
  scores <- rbind(status, trend)
  return(scores)
}

LE <- function(scores, layers) {
  scores.LE <- scores %>%
    filter(goal %in% c('LIV', 'ECO') &
             dimension %in% c('status', 'trend', 'score', 'future')) %>%
    spread(goal, score) %>%
    mutate(score = rowMeans(cbind(ECO, LIV), na.rm = TRUE)) %>%
    select(region_id, dimension, score) %>%
    mutate(goal  = 'LE')
  
  # rbind to all scores
  scores <- scores %>%
    rbind(scores.LE)
  
  # LIV, ECO and LE: nullify unpopulated regions and those of the Southern Ocean Islands
  r_s_islands   <-   layers$data$rgn_georegions %>%
    filter(level == "r2" & georgn_id == 999) %>%
    .$rgn_id
  
  r_unpopulated <- layers$data$le_popn %>%
    group_by(rgn_id) %>%
    arrange(rgn_id, year) %>%
    summarize(count = last(count)) %>%
    filter(count == 0) %>%
    .$rgn_id
  
  NA_rgns <- union(r_s_islands, r_unpopulated)
  
  scores <- scores %>%
    mutate(score = ifelse(
      goal %in% c('LIV', 'ECO', 'LE')
      &
        !(dimension %in% c('pressures', 'resilience')) &
        region_id %in% NA_rgns,
      NA,
      score
    ))
  # scores[with(scores,
  #            goal %in% c('LIV','ECO','LE') &
  #              !dimension %in% c('pressures','resilience') &
  #              region_id %in% union(r_s_islands, r_unpopulated)),
  #       'score'] = NA
  
  # return scores
  return(scores)
}
