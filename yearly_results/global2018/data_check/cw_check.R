## check on CW zero scores
library(here)
library(dplyr)


get_data <- function(x){
  name_cw <- basename(x)
  cw <- read.csv(x)
  cw$pressure = name_cw
  return(cw)}

polist <- list.files(here("eez/layers"), full=TRUE, pattern = "po_")
polist <- polist[-grep("water", polist)]

data_list <- lapply(polist, get_data)

cw <- bind_rows(data_list) 

filter(cw, rgn_id ==98)
