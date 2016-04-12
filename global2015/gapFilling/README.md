# Summary of gapfilling 
Project to develop methods of describing the contribution of gapfilled data to OHI scores.  Focus is on the 2015 assessment.

## To do:
1. Move code to calculate scores (stuff at the end that weights by eez area, etc.) from status_trend and move to scores.Rmd
2. Calculate pressures gapfilling
3. Calculate resilience gapfilling
4. Combine components to calculate likely future state and final scores
5. Redo figures
6. Fix problem where instead of keeping NAs for data as NAs, they may have been converted to zeros in gapfilling datasets.

## Workflow for gapfilling

#### Step 1: layers_download.Rmd (do this if there were updates in dataprep to the layers)
Based on the paths in "layers_eez.csv", gapfilling datasets are hunted down and placed in the ohi-global/global2015/gapFilling/layers folder.  In most cases, the gapfilling data will be located in the same location as the actual data, with a _gf extension.

#### Step 2: layers_summary.R
Summarizes the amount of gapfilling in each data layer and saves information as: "layers_summary.csv"

#### Step 3: status_trend.Rmd
Calculates percent contribution of gapfilled data to status and trend scores.  Saves file with these dimensions as "scores.csv"

#### Step 4: pressure_resilience.Rmd
Calculates pressure and resilience components for each goal and appends them to scores.csv.

#### Step 5: scores.Rmd
Calculates contribution of gapfilling to final OHI scores.

#### Step 6: gapfilling_visualization.Rmd
Creates relevant tables and figures to describe gapfilling. Saves files to figures_tables folder.


## Other files
* scores.csv: Describes percent gapfilling for OHI components (output of status_trend, pressures_resilience, scores scripts)
* layers_summary: Summarizes each data layer, output of layers_summary.R
* layers_eez.csv, resilience_matrix.csv, resilience_weights.csv: key files archived from the OHI 2015 assessment (these have since been updated so can't refer to them from the ohi-global folder)
* dissagregated_gap_fill.R and dissagregated_gap_fill.csv: Description of how regions changed from 2012 to 2013 assessment that required gapfilling
* layers: the majority of gapfilling data layers downloaded from layers_download.Rmd



