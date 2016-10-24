library(tidyverse)

# new: copy from eez2016 ---
for (yr in 2012:2015){ # yr = 2012
  file.copy('eez2016/conf/goals.Rmd', sprintf('eez%d/conf/goals.Rmd', yr), overwrite=T)
}


# old: fetch from each layers.csv ----
for (yr in 2012:2016){ # yr = 2012
  sprintf('eez%d/layers.csv', yr) %>%
    read_csv() %>%
    select(layer, targets, description) %>%
    gather(field_name, field_info, -layer, -targets) %>%
    write_csv(sprintf('eez%d/layers_meta.csv', yr))
}