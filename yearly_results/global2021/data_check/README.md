# Mariculture layers 

* mariculture_potential.csv: Data set with each OHI region and its corresponding mariculture production potential based on data fron [Gentry et al. 2017](https://www.nature.com/articles/s41559-017-0257-9)

* mariculture_production.csv: [Global Aquaculture Production](https://www.fao.org/fishery/statistics/global-aquaculture-production/en) from FAO. Prepped to match OHI regions and includes most brackishwater and marine production. Seaweeds are multiplied by a % that goes to human consumption based on lit review of produced species. 

* mariculture_sustainability.csv: From [Seafood Watch](https://www.seafoodwatch.org/recommendations/our-standards). Gapfilled to match FAO production for each OHI region. 

* combined_mariculture_layers.csv: I went ahead and joined these three tables together to save you some time. 

* summarized_mariculture_layers.csv: This is the final layer that scores are calculated from. Has OHI regions (rgn_id, rgn_name), the OHI scenario year (scenario_year), production weighted mean of sustainability for entire region (sust_rgn), 4 year rolling mean of total production tonnes (tonnes_sum), potential production (potential_mar_tonnes), observed production/potential production (tonnes_score), tonnes_score capped at 1 (tonnes_score_rescale), final score (sust_times_tonnes_score)
 
