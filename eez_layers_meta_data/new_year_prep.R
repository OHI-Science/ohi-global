# Preparing files for a new assessment year!
# This adds an additional assessment year to the eez/conf/scenario_data_years.csv
# The data year is the same as the previous assessment year.
# As each data layer is added, the data year will be manually updated
# directly within the eez/conf/scenario_data_years.csv
# NOTE: The years will need to be updated

# NOTE: After this, update the calculate_scores.R file with the latest links, etc.
# Then run calculate_scores.R to make sure there are no changes

library(dplyr)
library(here)

yrs <- read.csv(here("eez/conf/scenario_data_years.csv"))
new_yr <- yrs %>%
  filter(scenario_year == 2018) %>%  # indicate year of previous assessment
  mutate(scenario_year = 2019)       # indicate this year's assessment

yrs <- rbind(yrs, new_yr)

write.csv(yrs, here("eez/conf/scenario_data_years.csv"), row.names=FALSE)
