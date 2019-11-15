## checking on scores

#fluctuations in HAB subgoal
# only two variables that really change are sb and seaice edge
# changes in score appear reasonable based on these variables

ice <- read.csv("eez/layers/hab_seaice_health.csv") %>%
  filter(habitat == "seaice_edge")
ice_shore <- read.csv("eez/layers/hab_seaice_health.csv") %>%
  filter(habitat == "seaice_shoreline")
sb <- read.csv("eez/layers/hab_softbottom_health.csv")
coral <- read.csv("eez/layers/hab_coral_health.csv")
man <- read.csv("eez/layers/hab_mangrove_health.csv")
marsh <- read.csv("eez/layers/hab_saltmarsh_health.csv")
grass <- read.csv("eez/layers/hab_seagrass_health.csv")

# Jarvis Is
filter(sb, rgn_id == 149)

# Poland
filter(sb, rgn_id == 178)
filter(ice, rgn_id == 178)

# Jan Mayan
filter(sb, rgn_id == 144)
filter(ice, rgn_id == 144)

# Lithuania
filter(sb, rgn_id == 189)
filter(ice, rgn_id == 189)

# Bouvet
filter(sb, rgn_id == 189)
filter(ice, rgn_id == 189)

# Peru
filter(sb, rgn_id == 138)
filter(ice, rgn_id == 138)

# Iceland
filter(sb, rgn_id == 143)
filter(ice, rgn_id == 143)
filter(ice_shore, rgn_id == 143)
filter(coral, rgn_id == 143)
filter(man, rgn_id == 143)
filter(salt, rgn_id == 143)
filter(grass, rgn_id == 143)

### Checking out LSP drop in scores

scores <- read.csv("eez/scores.csv")

#Sint Maarten
dplyr::filter(scores, goal == "LSP" & region_id == 220) %>%
  dplyr::arrange(dimension, year)

# Curacao
dplyr::filter(scores, goal == "LSP" & region_id == 244) %>%
  dplyr::arrange(dimension, year)

# Kuwait
dplyr::filter(scores, goal == "LSP" & region_id == 51) %>%
  dplyr::arrange(dimension, year)


####################
## TR
## Why are Israel and Lebanon so low?
jobs <- read.csv("eez/layers/tr_jobs_pct_tourism.csv")
warn <- read.csv("eez/layers/tr_travelwarnings.csv")
sus <- read.csv("eez/layers/tr_sustainability.csv")

filter(jobs, rgn_id == 79)
filter(warn, rgn_id == 79)
filter(sus, rgn_id == 79)

filter(jobs, rgn_id == 78)
filter(warn, rgn_id == 78)
filter(sus, rgn_id == 78)

#### Increase in TR in Samoa 152

tr <- read.csv("layers/tr_jobs_pct_tourism.csv") %>%
  filter(rgn_id == 152)

### increase in NP in Samoa
np <- read.csv("layers/np_harvest_tonnes.csv") %>%
  filter(rgn_id == 152)


#### Increase in TR in Myanmar 205

tr <- read.csv("layers/tr_jobs_pct_tourism.csv") %>%
  filter(rgn_id == 205)

### increase in NP in Myanmar
np <- read.csv("layers/np_harvest_tonnes.csv") %>%
  filter(rgn_id == 205)


#### Pitcairn 146
lsp_off <- read.csv("layers/lsp_prot_area_offshore3nm.csv") %>%
  filter(rgn_id == 146)
lsp_in <- read.csv("layers/lsp_prot_area_inland1km.csv") %>%
  filter(rgn_id == 146)

#### Increase in TR in Qatar 190

tr <- read.csv("layers/tr_jobs_pct_tourism.csv") %>%
  filter(rgn_id == 190)

#### Decrease in TR in Saudi Arabia 50

tr <- read.csv("layers/tr_jobs_pct_tourism.csv") %>%
  filter(rgn_id == 50)
(0.0509511-0.0711695)/0.0711695

np <- read.csv("layers/np_harvest_tonnes.csv") %>%
  filter(rgn_id == 50)
