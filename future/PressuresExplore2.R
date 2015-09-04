###CalculatePressuresScore fucntion

function (p, w, GAMMA = 0.5, browse = F, pressures_categories = list(environmental = c("po", 
                                                                                       "hd", "fp", "sp", "cc"), social = "ss")) 
{  # a few error checks:
  if (getOption("debug", FALSE)) {
    stopifnot(is.array(w) && is.array(p))
    stopifnot(min(p, na.rm = T) >= 0 && max(p, na.rm = T) <= 
                1)
    stopifnot(min(w, na.rm = T) >= 0 && max(w, na.rm = T) <= 
                3)
  }
  
  # a few more error checks:
  stopifnot(all(names(dimnames(p)) == c("region_id", "pressure")))
  stopifnot(all(names(dimnames(w)) == c("region_id", "pressure")))
  stopifnot(all(dimnames(w)$region_id %in% dimnames(p)$region_id))
  stopifnot(all(dimnames(w)$pressure %in% dimnames(p)$pressure))
  w <- with(dimnames(p), w[region_id, pressure, drop = F])
  stopifnot(all(grepl("_", dimnames(p)$pressure)))
  
  # extract the 2 letter pressure category code from the pressure layer name
  pcat <- data.frame(pressure = unique(dimnames(p)$pressure))
  pcat <- within(pcat, {
    category <- gsub("^([a-z]+)_.*$", "\\1", tolower(pressure))
  })
  
  # replace NA weights with zeros:
  w <- ifelse(w == 0, NA, w)
  
  # multiply the pressures by the weights (this is just used as a template)
  p_w <- p * w
  
  
  
  ################### Calculations for environmental pressures ################
  
  # make data into longform and add pressure category:
  p_w <- merge(reshape2::melt(p_w), pcat, all.x = T, by = "pressure")
 
  # make 3-d array from pressure * weight data, for details: str(p_w)
  p_w <- reshape2::acast(p_w, region_id ~ category ~ pressure)
  
  # make empty 2-d array
  p_w_max <- array(NA, dim = c(dim(p_w)[[1]], length(pressures_categories$environmental)), 
                   dimnames = list(region_id = dimnames(p)[[1]], category = pressures_categories$environmental))
  # make another copy of the p_w_max array created above:
  p_k <- p_w_max
  
  # Following function: for each region/pressure category, the maximum weight is determined (p_w_max)
  # and the weights are multiplied by the pressures and then summed:
  for (k in dimnames(p_k)$category) { #k="po"  k="fp" k="cc"
    j <- grep(paste("^", k, "_", sep = ""), dimnames(w)[[2]],   # get names of pressures in a category
              value = T)
    wj <- w[, j, drop = F]   # isolate the weights in this pressure category
    pj <- p[, j, drop = F]   # isolate the pressures in this pressure category
    
    # get maximum weight for each region/pressure within a category
    p_w_max[, k] <- apply(wj, 1, function(x) {  
      if (all(is.na(x))) {
        NA
      }
      else {
        max(x, na.rm = T)
      }
    })
    
    # multiply pressures and weights and sum
    p_k[, k] <- apply(pj * wj, 1, function(x) { # within a category/region: multiply weights and pressures and then sum across rows
      if (all(is.na(x))) {
        NA
      }
      else {
        sum(x, na.rm = T)
      }
    })
    if (browse & k == "po") 
      browser()
    range(p_k[, k])
    
    p_k[, k] <- score.rescale(p_k[, k], xlim = c(0, 3))   # rescales data to be 0 and 1, 
                                                          # see score.rescale(c(-1, 0, 0.5, 1, 2, 3, 5), xlim=c(0, 3))
    p_k[, k] <- score.clamp(p_k[, k])  # converts scores <0 to 0 and >1 to 1, this is needed because the sum of pressure layers may be > 3 
  }
  
  #weighting pressures by maximum weight in each pressure category (po, hd, fp, ...) - pressure categories are not equally weighted...
  k <- pressures_categories$environmental
  p_e <- rep(NA, nrow(p_k))
  for (i in 1:nrow(p_k)) {
    p_e[i] <- sum(p_k[i, k] * p_w_max[i, k], na.rm = T)/sum(ifelse(is.na(p_k[i, k]), NA, 1) * p_w_max[i, k], na.rm = T)
  }
  names(p_e) <- dimnames(p_k)$region_id
  
  
  ################ Following is for social pressures ####################
  stopifnot(length(pressures_categories$social) == 1)
  
  k <- pressures_categories$social[[1]]
  j <- grep(paste("^", k, "_", sep = ""), dimnames(p)[[2]], 
            value = T)
  
  # take mean (not really relevant here because there is only one)
  # score.clamp converts negative numbers to zero and numbers >1 to one
  p_s <- score.clamp(apply(p[, j, drop = F], 1, mean, na.rm = T))
  names(p_s) <- rownames(p)
  
  # If there is no social pressure, the score reverts to the average environmental pressure
  # otherwise they are combined based on gamma (gamma determines the relative contribution of social vs. environmental pressures)
  # when gamma is 0.5, it is the average of the environmental and social
  if (all(is.nan(p_s))) {
    p_x <- p_e
  }
  else {
    p_x <- (GAMMA * p_e) + ((1 - GAMMA) * p_s)
  }
  round(p_x * 100, 2)
}