# CalculatePressuresMatrix which is called when a goal has components (called when goal has componenets)

function (alpha, beta, calc = "avg") 
{
  w <- matrix(NA, nrow = dim(beta)[[1]], ncol = dim(alpha)[[2]], 
              dimnames = list(region_id = dimnames(beta)[[1]], pressure = dimnames(alpha)[[2]]))
  for (i in dimnames(w)$region_id) { # i=1
    for (j in dimnames(w)$pressure) { # j="cc_slr"
      if (calc == "avg") {
        w[i, j] <- sum(t(alpha)[j, ][dimnames(beta)$category] * 
                         beta[i, ], na.rm = T)/sum(beta[i, ], na.rm = T)
      }
      else if (calc == "mean") {
        w[i, j] <- mean(t(alpha)[j, ] * beta[i, ], na.rm = T)
      }
      else if (calc == "presence") {
        w[i, j] <- mean(t(alpha)[j, ] * beta[i, ], na.rm = T)
      }
      else {
        stop("CalculatePressuresMatrix() calc argument not one of required: 'avg','mean','presence'")
      }
    }
  }
  w[is.nan(w)] <- NA
  w
}