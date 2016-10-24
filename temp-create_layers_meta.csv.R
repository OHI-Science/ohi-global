library(tidyverse)

for (yr in 2012:2016){ # yr = 2012
  sprintf('eez%d/layers.csv', yr) %>%
    read_csv() %>%
    select(layer, targets, description) %>%
    gather(field_name, field_info, -layer, -targets) %>%
    write_csv(sprintf('eez%d/layers_meta.csv', yr))
}