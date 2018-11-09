#' Gapfill using georegional means
#' 
#' Gapfill using georegional means, providing the finest possible resolution from 3 hierarchies (r2 > r1 > r0) derived from \href{http://en.wikipedia.org/wiki/United_Nations_geoscheme}{United Nations geoscheme}.
#' 
#' @param data data.frame to gapfill having at least fields: \code{fld_id} and \code{fld_value}, and optionally \code{fld_weight}
#' @param georegions data.frame having at least fields: \code{fld_id} and \code{r0}, \code{r1}, and \code{r2} with georegion id values
#' @param fld_id common spatial id field (eg region_id or country_key) between \code{data} and \code{georegions}
#' @param fld_weight optional weighting field in \code{data}
#' @param rgn_weights data frame of weights, expecting rgn_id in first column and weight in second
#' @param ratio_weights if TRUE, multiply the gapfilled value by the ratio of the region's weight to the regional average weight. Defaults to FALSE. IMPORTANT to set to TRUE if dealing with values that SUM!
#' @param fld_year optional year field in \code{data}
#' @param fld_value value to gapfill in \code{data}
#' @param georegion_labels with same dimensions as georegions having fields: \code{r0_label}, \code{r1_label}, \code{r2_label} and \code{v_label}
#' @param gapfill_scoring_weights used to determine gapfilling scoreset. should range 0 to 1. defaults to \code{c('r0'=1, 'r1'=0.8, 'r2'=0.5, 'v'=0)}
#' @param r0_to_NA assign value of NA if only georegional average availabe at the global level (r0). defaults to True.
#' @param attributes_csv optional path and filename to save attribute table. defaults to NULL
#' 
#' @return Returns a data.frame of having all the \code{fld_id} from georegions filled in the following columns:
#' \itemize{
#'   \item \code{fld_id} - spatial id (eg region_id or country_key).
#'   \item \code{fld_value} - the gapfilled value (eg score).
#' }
#' The returned data.frame also has an attribute "gapfill_georegions" which shows the calculated georegional means and which levels were chosen:
#' \itemize{
#'   \item \code{r0} - georegional id for level 0, ie global.
#'   \item \code{r1} - georegional id for level 1.
#'   \item \code{r2} - georegional id for level 2, the finest resolution of georegions.
#'   \item \code{id} - spatial id (eg region_id or country_key).
#'   \item \code{w} - weight used to apply \code{\link{weighted.mean}}. Defaults to 1 if not supplied as \code{fld_weight} parameter.
#'   \item \code{v} - original \code{fld_value} in \code{data}
#'   \item \code{r2_v} - weighted.mean for level 2
#'   \item \code{r1_v} - weighted.mean for level 1
#'   \item \code{r0_v} - weighted.mean for level 0 (global)
#'   \item \code{r2_n} - count of regions available for level 2
#'   \item \code{r1_n} - count of regions available for level 1
#'   \item \code{r0_n} - count of regions available for level 0
#'   \item \code{r2_n_notna} - count of region values that are not NA for level 2
#'   \item \code{r1_n_notna} - count of region values that are not NA for level 1
#'   \item \code{r0_n_notna} - count of region values that are not NA for level 0
#'   \item \code{z_level} - finest level available
#'   \item \code{z_ids} - ids for regions that are not NA which contributed to the score
#'   \item \code{z_n} - count of input values for finest level available
#'   \item \code{z_n_pct} - percent of region values that are not NA over all possible [0 to 1]
#'   \item \code{z_g_score} - gapfilling score (see details)
#'   \item \code{z} - weighted.mean for finest level available
#' }
#' 
#' @details
#' Gapfill using georegional means, providing the finest possible resolution from 3 hierarchies (r2 > r1 > r0).
#' 
#' The gapfill score (z_g_score) in the attribute table is formulated such that the higher the score, the 
#' more gapfilling performed. The maximal gapfill score is based on gapfilling at the global level (r0=1) and least
#' if no gapfilling performed (ie z = v). But then some regional averages are applied with only a few regional values 
#' while others might have all but the gapfilled region available. To account for this aspect, the difference between the next
#' finer level's weight is multiplied by the percent regions and subtracted from the level's weight, like so:
#'
#' \code{gapfill_scoring_weights[z_level] - z_n_pct * diff(gapfill_scoring_weights[z_level, z_level_finer])}
#' 
#' @keywords ohi
#' @examples
#' 
#' \dontrun{
#' ## setup
#' require(ohicore)
#' 
#' # gapfill
#' g = gapfill_georegions(data, georegions, fld_weight='w_sum')
#' 
#' # show result and table
#' head(g)
#' head(attr(g, 'gapfill_georegions'))
#' }
#' @import dplyr
#' 
#' @export
gapfill_georegions = function(
  data, 
  georegions, 
  fld_id            = intersect(names(data), names(georegions)),
  fld_year          = ifelse('year' %in% names(data), 'year', NA),
  fld_value         = setdiff(names(data), c(fld_id, fld_weight, 'year')),
  georegion_labels  = NULL,
  fld_weight        = NULL,
  rgn_weights       = NULL,
  ratio_weights     = FALSE,
  gapfill_scoring_weights = c('r0'=1, 'r1'=0.8, 'r2'=0.5, 'v'=0),
  r0_to_NA          = TRUE,
  attributes_csv    = NULL 
){
  # TODO: provide gapfilling with category data
  
  # check arguments
  stopifnot(length(fld_id) == 1, fld_id %in% names(data), fld_id %in% names(georegions), !fld_id %in% c('r0','r1','r2'))
  stopifnot( is.null(fld_weight) || (!is.null(fld_weight) && fld_weight %in% names(data)) )
  if (!is.null(rgn_weights)) stopifnot(ncol(rgn_weights) == 2 & names(rgn_weights)[1] == 'rgn_id')
  stopifnot(length(fld_value) == 1, fld_value %in% names(data))
  stopifnot(all(c('r0','r1','r2') %in% names(georegions)))
  stopifnot(all(data[[fld_id]] %in% georegions[[fld_id]]))
  stopifnot( is.na(fld_year) || (!is.na(fld_year) && fld_year %in% names(data)) )
  stopifnot(!(!is.null(fld_weight) & !is.null(rgn_weights))) # can't weight both ways by georegion and data.frame
  stopifnot( ratio_weights==F | (ratio_weights==T & !is.null(rgn_weights)) )  # need rgn_weights if applying ratio_weights
  
  # rename fields
  g = plyr::rename(georegions      , setNames('id', fld_id))
  d = plyr::rename(data            , setNames(c('id','v'),  c(fld_id, fld_value)))
  
  # check for duplicate georegion entries
  stopifnot(anyDuplicated(g$id) == 0)
  
  # georegion_labels
  if (!is.null(georegion_labels)){
    stopifnot(fld_id %in% names(georegion_labels))
    stopifnot(all(c('r0_label','r1_label','r2_label') %in% names(georegion_labels)))
    stopifnot(nrow(georegion_labels) == nrow(georegions))
    l = plyr::rename(georegion_labels, setNames('id', fld_id))
    stopifnot(anyDuplicated(l$id) == 0)
  }
  
  # get n regions per georegion for later calculating gapfill score
  g = g %>%
    dplyr::group_by(r0) %>%
    dplyr::mutate(r0_n = n()) %>%  
    dplyr::group_by(r1) %>%
    dplyr::mutate(r1_n = n()) %>%
    dplyr::group_by(r2) %>%
    dplyr::mutate(r2_n = n())
  
  # add weights to data
  if (is.null(fld_weight) & is.null(rgn_weights)){
    
    # default weights
    d[['w']] = 1
    
  } else if (!is.null(fld_weight)){
    
    # weights in data
    d = plyr::rename(d, setNames('w', fld_weight))
    
    if (sum(is.na(d$w))>0){
      message(sprintf('\n  data[[fld_weights]] are NA (where values reported) so removed: %d of %d rows\n    %s', sum(is.na(d$w)), nrow(d), paste(unique(d$id[is.na(d$w)]), collapse=',') ))
      d = subset(d, !is.na(w))      
    }    
  } else if(!is.null(rgn_weights)){
    
    # add weights to georegions
    g = g %>%
      dplyr::left_join(
        rgn_weights %>%
          plyr::rename(
            setNames('w', names(rgn_weights)[2])) %>%
          dplyr::select(id=rgn_id, w),
        by='id')
    
    if (sum(is.na(g$w))>0){
      message(sprintf('\n  georegions[[weights]] are NA (where georegions with rgn_id exist) so removed: %d of %d rows\n    %s', sum(is.na(g$w)), nrow(g), paste(unique(g$id[is.na(g$w)]), collapse=',') ))
      g = subset(g, !is.na(g))
    }    
  } else {
    stop('logical impossibility!')
  }
  
  # remove NAs
  if (sum(is.na(d$v))>0){
    message(sprintf('\n  data values are NA so removed: %d of %d rows', sum(is.na(d$v)), nrow(d) ))
    d = subset(d, !is.na(v))
  }
  
  if (is.na(fld_year)){
    # check for duplicates
    stopifnot(anyDuplicated(d$id) == 0)
    
    # merge georegions with data
    x = 
      merge(
        g, 
        d, 
        by='id', all.x=T) %>%
      dplyr::arrange(id)
    
    # georegion means
    y = x %>%
      dplyr::filter(!is.na(v), !is.na(w))
    z = x %>%
      dplyr::left_join(
        y %>% 
          dplyr::group_by(r2) %>%
          dplyr::summarise(
            r2_v       = weighted.mean(v, w),
            r2_w_avg   = mean(w),
            r2_n_notna = n(),
            r2_ids     = paste(id, collapse=',')),
        by='r2') %>%
      dplyr::left_join(
        y %>% 
          dplyr::group_by(r1) %>%
          dplyr::summarise(
            r1_v       = weighted.mean(v, w),
            r1_w_avg   = mean(w),
            r1_n_notna = n(),
            r1_ids     = paste(id, collapse=',')),
        by='r1') %>%
      dplyr::left_join(
        y %>% 
          dplyr::group_by(r0) %>%
          dplyr::summarise(
            r0_v       = weighted.mean(v, w),
            r0_w_avg   = mean(w),
            r0_n_notna = n(),
            r0_ids     = paste(id, collapse=',')),
        by='r0') %>%
      dplyr::arrange(r0, r1, r2, id) %>%
      dplyr::select(r0, r1, r2, id, w, v, r2_v, r1_v, r0_v, r2_w_avg, r1_w_avg, r0_w_avg, r2_n, r1_n, r0_n, r2_n_notna, r1_n_notna, r0_n_notna, r2_ids, r1_ids, r0_ids)    
  } else {    
    # using year
    d = plyr::rename(d, setNames('yr', fld_year))
    
    # check for duplicates
    stopifnot(anyDuplicated(d[,c('id','yr')]) == 0)
    
    # TODO: expand gapfill_georegions to use rgn_weights with year to match data
    
    # expand georegions to every possible year in data
    gy = expand.grid(list(
      yr = sort(unique(d$yr)),
      id = g$id)) %>%
      merge(
        g, 
        by='id') %>%
      #select(yr, id, r0, r1, r2, r2_n, r1_n, r0_n, w) %>%
      dplyr::arrange(yr, id)
    
    # merge with data
    x = gy %>%
      merge(
        d, 
        by=c('yr','id'), all.x=T) %>%      
      dplyr::arrange(yr, id) %>%
      dplyr::select(yr, id, r0, r1, r2, r2_n, r1_n, r0_n, v, w)
    
    # get rows with v and w
    y = x %>%
      dplyr::filter(!is.na(v), !is.na(w))
    
    # calculate georegion means
    z = x %>%
      dplyr::left_join(
        y %>% 
          dplyr::group_by(yr, r2) %>%
          dplyr::summarise(
            r2_v       = weighted.mean(v, w),
            r2_w_avg   = mean(w),
            r2_n_notna = n(),
            r2_ids     = paste(id, collapse=',')),
        by=c('yr','r2')) %>%
      dplyr::left_join(
        y %>% 
          dplyr::group_by(yr, r1) %>%
          dplyr::summarise(
            r1_v       = weighted.mean(v, w),
            r1_w_avg   = mean(w),
            r1_n_notna = n(),
            r1_ids     = paste(id, collapse=',')),
        by=c('yr','r1')) %>%
      dplyr::left_join(
        y %>% 
          dplyr::group_by(yr, r0) %>%
          dplyr::summarise(
            r0_v       = weighted.mean(v, w),
            r0_w_avg   = mean(w),
            r0_n_notna = n(),
            r0_ids     = paste(id, collapse=',')),
        by=c('yr','r0')) %>%
      dplyr::arrange(yr, r0, r1, r2, id) %>%
      dplyr::select(yr, r0, r1, r2, id, w, v, r2_v, r1_v, r0_v, r2_w_avg, r1_w_avg, r0_w_avg, r2_n, r1_n, r0_n, r2_n_notna, r1_n_notna, r0_n_notna, r2_ids, r1_ids, r0_ids)    
  }
  
  # select best available value and calculate gapfilling score  
  z = z %>%
    dplyr::mutate(
      z_level = ifelse(!is.na(v), 'v',
                       ifelse(!is.na(r2_v), 'r2',
                              ifelse(!is.na(r1_v), 'r1',
                                     ifelse(!is.na(r0_v), 'r0', NA)))))
  
  # assign attributes by georegion level (r#)
  z  = 
  dplyr::bind_rows(
    # rgn
    z %>%
      dplyr::filter(z_level=='v') %>%
      dplyr::mutate(
        z_ids     = as.character(id),
        z_w_avg   = w,
        z_n       = 1,
        z_n_pct   = 1,
        z_g_score = 0,
        z         = v),
    # r2
    z %>%
      dplyr::filter(z_level=='r2') %>%
      dplyr::mutate(
        z_ids     = r2_ids,
        z_w_avg   = r2_w_avg,
        z_n       = r2_n_notna,
        z_n_pct   = r2_n_notna/r2_n,
        z_g_score = gapfill_scoring_weights['r2'] - z_n_pct * diff(gapfill_scoring_weights[c('v','r2')]),
        z         = r2_v),
    # r1
    z %>%
      dplyr::filter(z_level=='r1') %>%
      dplyr::mutate(
        z_ids     = r1_ids,
        z_w_avg   = r1_w_avg,
        z_n       = r1_n_notna,
        z_n_pct   = r1_n_notna/r1_n,
        z_g_score = gapfill_scoring_weights['r1'] - z_n_pct * diff(gapfill_scoring_weights[c('v','r1')]),
        z         = r1_v),
    # r0
    z %>%
      dplyr::filter(z_level=='r0') %>%
      dplyr::mutate(
        z_ids     = r0_ids,
        z_w_avg   = r0_w_avg,
        z_n       = r0_n_notna,
        z_n_pct   = r0_n_notna/r0_n,
        z_g_score = gapfill_scoring_weights['r0'] - z_n_pct * diff(gapfill_scoring_weights[c('v','r0')]),
        z         = r0_v)
  ) %>%
    dplyr::select(-r2_ids, -r1_ids, -r0_ids)
  
  # multiply by ratio if argument
  if (ratio_weights){  
    z = z %>% mutate(
      z_orig = z,
      z = z * w / z_w_avg)    
  }


  # if r0_to_NA, assign value of NA if only georegional average availabe at the global level (r0)
  if (r0_to_NA) z$z = ifelse(z$z_level=='r0', NA, z$z)
  
  # add labels if provided
  if (!is.null(georegion_labels)){
    z = z %>%
      dplyr::left_join(
        l %>%
          dplyr::select(id=id, r0_label, r1_label, r2_label, v_label),
        by='id') %>%
      dplyr::arrange(r0_label, r1_label, r2_label, v_label) 
    if (is.na(fld_year)){
      z = z %>%
        dplyr::select(r0_label, r1_label, r2_label, v_label, 
               r0, r1, r2, id, w, v, r2_v, r1_v, r0_v, r2_n, r1_n, r0_n, 
               r2_n_notna, r1_n_notna, r0_n_notna, z_level, z_ids, z_n, z_n_pct, z_g_score, z)
    } else {
      z = z %>%
        dplyr::select(r0_label, r1_label, r2_label, v_label, yr, 
               r0, r1, r2, id, w, v, r2_v, r1_v, r0_v, r2_n, r1_n, r0_n, 
               r2_n_notna, r1_n_notna, r0_n_notna, z_level, z_ids, z_n, z_n_pct, z_g_score, z)
    }
  }
  
  # return result
  if (is.na(fld_year)){
    r = z %>%
      dplyr::select(id, z) %>%
      dplyr::arrange(id) %>%
      plyr::rename(setNames(c(fld_id, fld_value), c('id','z')))
  } else {
    r = z %>%
      dplyr::select(yr, id, z) %>%
      dplyr::arrange(yr, id) %>%
      plyr::rename(setNames(c(fld_year, fld_id, fld_value), c('yr', 'id', 'z')))
  }
  
  # store attributes, with option to save as .csv
  attr(r, 'gapfill_georegions') = z
  
  if (!is.null(attributes_csv)){
    write.csv(z, attributes_csv, na = '', row.names=FALSE)   
  }
  
  return(r)
}