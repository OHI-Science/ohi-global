##############################################
# figuring out the proportion of each layer
# with gapfilling
##############################################

# In the future (when I do a better job of standardizing the data), 
# I will want to automate this.  But this time I am going to muscle through it

### Fisheries ----
bmsy <- read.csv('eez2015/layers/fis_b_bmsy.csv')

catch <- read.csv('eez2015/layers/fis_meancatch.csv') %>%
  separate(fao_saup_id, c("fao_id", "saup_id"), sep="_") %>%
  separate(taxon_name_key, c("taxon_name", "taxon_key"), sep="_") %>%
  mutate(fao_id = as.numeric(fao_id)) %>%
  mutate(saup_id = as.numeric(saup_id)) %>%
  mutate(taxon_key = as.numeric(taxon_key)) %>%
  left_join(bmsy, by=c("fao_id", "taxon_name", "year")) %>%
  mutate(gapfill = ifelse(is.na(b_bmsy), 1, 0)) %>%
  filter(year == 2011)

mean(catch$gap_fill)

data <- data.frame(layer = c("Fisheries catch data", "Stock exploitation status (B/Bmsy)"), 
                   proportion_gf = c(0, mean(catch$gap_fill)))

### Mariculture ----
sustain <- read.csv('global2015/gapFilling/layers/mar_sustainability_score.csv') %>%
  select(region_id = id, species, species_code, year, gap_fill_sustain = gapfill) %>%
  mutate(gap_fill_sustain = ifelse(gap_fill_sustain == "actuals", 0, 1)) %>%
  filter(year==2013)

data <- rbind(data, data.frame(layer = c("Mariculture Sustainability Index (MSI):status"),
                               proportion = c(mean(sustain$gap_fill_sustain))))

harvest <- read.csv('global2015/gapFilling/layers/mar_harvest_tonnes.csv') %>%
  select(region_id = id, species, species_code, year, gap_fill_harvest = gap_0_fill)%>%
  mutate(gap_fill_harvest = ifelse(gap_fill_harvest == "NA_to_zero", 1, 0)) %>%
  filter(year==2013)

data <- rbind(data, data.frame(layer = c("Mariculture yield (tonnes)",
                                         "Mariculture yield (USD)"),
                               proportion = c(mean(sustain$gap_fill_sustain),
                                              0)))
