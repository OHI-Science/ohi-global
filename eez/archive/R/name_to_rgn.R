#' Name to region translate (old version still used in LE function...keep until this is revised)
#' 
#' (old version still used in LE function...keep until this is revised)
#' 
#' @param d dataset
#' @param fld_name field name of the region from the dataset
#' @param flds_unique field name for the dataset
#' @param fld_value field with value, defaults to 'value'
#' @param collapse_fxn function to collapse duplicate regions into one (example: China, Macau, Hong Kong)
#' @param collapse_csv optional .csv file provided to collapse duplicate regions
#' @param collapse_flds_join optional list of fields identified to collapse duplicate regions
#' @param dir_lookup directory of name-to-region look up tables
#' @param rgn_master.csv .csv file of eez-to-region combinations
#' @param rgn_synonyms.csv .csv file of synonyms of eez-to-region combinations
#' @param add_rgn_name T or F whether to include a column with the region name
#' @param add_rgn_type T of F whether to include the region type (eez...)
#' 
#' @details This function translates name to region id with a lookup.
#'  
#' @keywords ohi
#' @export

name_to_rgn = function(
  d, fld_name = 'country', 
  flds_unique=fld_name, 
  fld_value='value', 
  collapse_fxn = c('sum_na','mean','weighted.mean')[1],
  collapse_csv = NULL,
  collapse_flds_join = NULL,
  dir_lookup = '~/github/ohiprep/src/LookupTables',
  rgn_master.csv   = file.path(dir_lookup, 'eez_rgn_2013master.csv'),
  rgn_synonyms.csv = file.path(dir_lookup, 'rgn_eez_v2013a_synonyms.csv'),
  add_rgn_name=F, add_rgn_type=F) {
  # DETAIL. Return a data.frame (vs add_rgn_id which writes to a csv) and perform extra checks, including collapsing on duplicates.
  #  Note: The original fld_name lost because possible to collapse multiple countries into a single region.
  #
  # debug: fld_name='country'; flds_unique=c('country','commodity','year'); fld_value='value'; collapse_fxn=function(x) sum(x, na.rm=T); dpath = '../ohiprep/src/LookupTables'; rgn_master.csv   = file.path(dpath, 'eez_rgn_2013master.csv'); rgn_synonyms.csv = file.path(dpath, 'rgn_eez_v2013a_synonyms.csv')
  
  #   # ensure dplyr's summarize overrides plyr's summarize by loading in succession
  #   if ('package:reshape2'  %in% search()) detach('package:reshape2')
  #   if ('package:plyr'      %in% search()) detach('package:plyr')
  #   if ('package:dplyr'     %in% search()) detach('package:dplyr')
  #   library(reshape2)
  #   library(plyr)
  #   library(dplyr)
  
  # check for valid arguments  stopifnot(fld_name %in% names(d))
  stopifnot(fld_value %in% names(d))
  stopifnot(all(flds_unique %in% names(d)))
  stopifnot(sum(duplicated(d[, flds_unique])) == 0)
  
  rgns = read.csv(rgn_master.csv, na = "", stringsAsFactors = F) %>% 
    dplyr::select(rgn_id = rgn_id_2013, rgn_name = rgn_nam_2013, rgn_type = rgn_typ) %>% 
    dplyr::arrange(rgn_type, rgn_id, rgn_name) %>% 
    dplyr::group_by(rgn_id) %>% 
    dplyr::summarize(rgn_name = first(rgn_name), rgn_type = first(rgn_type)) %>% 
    dplyr::ungroup()
  
  r = dplyr::bind_rows(rgns %>% 
                         dplyr::select(rgn_id, tmp_name = rgn_name, tmp_type = rgn_type), read.csv(rgn_synonyms.csv, na = "") %>% 
                         dplyr::select(rgn_id = rgn_id_2013, tmp_name = rgn_nam_2013, tmp_type = rgn_typ)) %>% group_by(tmp_name) %>% 
    dplyr::summarize(rgn_id = first(rgn_id), tmp_type = first(tmp_type))
  
  d["tmp_name"] = d[fld_name]
  
  d = d %>% dplyr::mutate(tmp_name = str_replace(tmp_name, "^'", ""), 
                   tmp_name = stringr::str_replace(tmp_name, ".+voire", "Ivory Coast"), 
                   tmp_name = stringr::str_replace(tmp_name, ".+union", "Reunion"), 
                   tmp_name = stringr::str_replace(tmp_name, ".+publique du", "Republic of"), 
                   tmp_name = stringr::str_replace(tmp_name, "Cura.+", "Curacao"), 
                   tmp_name = stringr::str_replace(tmp_name, "Saint Barth.+", "Saint Barthelemy"), 
                   tmp_name = stringr::str_replace(tmp_name, ".+Principe", "Sao Tome and Principe"))
  
  m = merge(d, r, by = "tmp_name") %>% 
    dplyr::filter(tmp_type %in% c("eez", "ohi_region"))
  
  m_r = dplyr::left_join(d, r, by = "tmp_name") %>% 
    dplyr::filter(!tmp_type %in% c("eez", "ohi_region")) %>% 
    dplyr::mutate(tmp_name = factor(as.character(tmp_name)))
  
  if (sum(is.na(m_r$tmp_type)) > 0) {
    toprint = m_r %>% filter(is.na(tmp_type))
    cat("\nThese data were removed for not having any match in the lookup tables:\n")
    print(table(as.character(unique(toprint$country))))
  }
  
  if (nrow(m_r) > 0) {
    cat("\nThese data were removed for not being of the proper rgn_type (eez,ohi_region) or mismatching region names in the lookup tables:\n")
    print(table(select(m_r, tmp_name, tmp_type), useNA = "ifany"))
  }
  
  m_na = dplyr::filter(m, is.na(rgn_id))
  
  if (nrow(m_na) > 0) {
    cat("\nThese data have a valid tmp_type but no rgn_id:\n")
    print(table(m_na[, c(fld_name, "tmp_type")], useNA = "ifany"))
    stop("FIX region lookups.")
  }
  
  flds_unique_rgn_id = c("rgn_id", setdiff(c(flds_unique), fld_name))
  
  i_dup = duplicated(m[, flds_unique_rgn_id], fromLast = F) | 
    duplicated(m[, flds_unique_rgn_id], fromLast = T)
  
  if (sum(i_dup) > 0) {
    cat(sprintf("\nDUPLICATES found. Resolving by collapsing rgn_id with collapse_fxn: %s after first removing all NAs from duplicates...\n", 
                collapse_fxn))
    m_dup = m[i_dup, ]
    m_dup$tmp_value = m_dup[[fld_value]]
    m_dup$fld_name = m_dup[[fld_name]]
    m_dup = mutate(m_dup, fld_name = factor(as.character(fld_name)))
    print(table(select(m_dup, fld_name, rgn_id)))
    sum_na = function(x) {
      if (sum(is.na(x)) == length(x)) 
        return(NA)
      return(sum(x, na.rm = T))
    }
    
    weighted_mean = function(x, collapse_csv) {
      if (sum(is.na(x)) == length(x)) 
        return(NA)
      return(sum(x, na.rm = T))
    }
    
    if (collapse_fxn == "sum_na") {
      m_dup = m_dup %>% 
        dplyr::filter(!is.na(tmp_value)) %>% 
        dplyr::group_by_(.dots=as.list(flds_unique_rgn_id)) %>% 
        dplyr::summarize(tmp_value = sum_na(tmp_value)) %>% 
        dplyr::rename_(.dots = setNames('tmp_value', fld_value))
      head(m_dup)
    } else {
      if(collapse_fxn == "mean") {
        m_dup = m_dup %>% 
          dplyr::filter(!is.na(tmp_value)) %>% 
          dplyr::group_by_(.dots=as.list(flds_unique_rgn_id)) %>% 
          dplyr::summarize(tmp_value = mean(tmp_value, na.rm = T)) %>% 
          dplyr::rename_(.dots = setNames('tmp_value', fld_value))
        head(m_dup)
      } else {
        if(collapse_fxn == "weighted.mean") {
          w = read.csv(collapse_csv)
          flds = intersect(names(w), names(m_dup))
          fld = setdiff(names(w), names(m_dup))
          stopifnot(length(fld) == 1)
          #stopifnot(length(flds) < 1) #this doesn't seem to be a good test
          w["tmp_weight"] = w[fld]
          w = w[, c(flds, "tmp_weight")]
          criteria  <-  ~by == flds
          m_dup = m_dup %>% 
            dplyr::filter(!is.na(tmp_value)) %>% 
            dplyr::group_by_(.dots=as.list(flds_unique_rgn_id)) %>% 
            dplyr::left_join(w) %>% 
            dplyr::summarize(tmp_value = weighted.mean(tmp_value, 
                                                tmp_weight, na.rm = T)) %>% 
            dplyr::rename_(.dots = setNames('tmp_value', fld_value))
          head(m_dup)
        }   else {
          stop("collapse_fxn needs to be a string of one of the following: sum_na, mean, weighted.mean.")
        }
      }
    }
    m_d = dplyr::bind_rows(m[!i_dup, c(flds_unique_rgn_id, fld_value)], 
                     m_dup)
  } else {
    m_d = m
  }
  
  # limit to same subset of fields for consistent behavior regardless of duplicates presents
  m_d = m_d[, c(flds_unique_rgn_id, fld_value)]
  
  # add fields if explicitly requested
  if (add_rgn_type) m_d = dplyr::left_join(m_d, rgns %>% select(rgn_id, rgn_type), by='rgn_id')
  if (add_rgn_name) m_d = dplyr::left_join(m_d, rgns %>% select(rgn_id, rgn_name), by='rgn_id')
  
  # check to ensure no duplicates
  stopifnot(duplicated(m_d[, c(flds_unique_rgn_id)]) == 0) 
  
  m_d = m_d[, c(flds_unique_rgn_id, fld_value)]
  if (add_rgn_type) 
    m_d = dplyr::left_join(m_d, rgns %>% 
                             dplyr::select(rgn_id, rgn_type), by = "rgn_id")
  if (add_rgn_name) 
    m_d = dplyr::left_join(m_d, 
                    rgns %>% dplyr::select(rgn_id, rgn_name), 
                    by = "rgn_id")
  stopifnot(duplicated(m_d[, c(flds_unique_rgn_id)]) == 0)
  return(as.data.frame(m_d))
}