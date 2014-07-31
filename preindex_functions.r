# preindex_functions.r

# compare the 'preindex_function's assigned in goals.csv for each scenario

library(dplyr)

# identify scenarios
scenarios = c('eez2012', 
              'eez2013', 
              'eez2014')

# loop through scenarios
if (exists('d')) rm(d)
for (i in 1:length(scenarios)) { # i=1
  
  f = read.csv(sprintf('%s/conf/goals.csv', scenarios[i])) %>%
    select(goal, preindex_function)
  names(f) = c('goal', sprintf('preindex_fnx_%s', scenarios[i]))
  
  # rbind
  if (!exists('d')){ 
    d = f
  } else {
    d = d %>%
      inner_join(f, by='goal')
  }
}

# maybe best not to save this as a .csv because then will seem like a static file. Best to rerun preindex_functions.r each time.
# write.csv(d, 'preindex_functionsreport.csv', row.names=F)
