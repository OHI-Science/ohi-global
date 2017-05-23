ohi-global
==========
  
### Global assessments (EEZ, Antarctica, and High Seas) and scenarios (2012-2016) of the Ocean Health Index 

#### Anatomy of the file structure

General information about file structure is here: http://ohi-science.org/manual/#file-system-organization

Three OHI assessments (for a range of scenario years) are included in this folder:

* eez (scenarios: 2012 - 2016): OHI for 220 country/territorial EEZ regions, this is typically considered the "OHI global assessment"
* antarctica (scenarios: 2014 - 2015): OHI for Antarctica CCAMLR regions (includes the high seas/FAO and EEZ regions)
* high seas (scenarios: 2014 - 2015): OHI for high seas regions (FAO regions without the EEZ regions or Antarctica/CCAMLR regions)
 

Additional files/folders include:

* eez_layers_meta_data: Database for the EEZ data layers (data sources, names, etc.) and functions to create different data formats
* global_supplement: Descriptions of data layers and goals 
* global (2014-2016): Post assessment analysis and visualization of data
* calculate\_scores_??.R: These files provide the code to calculate scores for the corresponding OHI assessment
* layers\_??.csv: These files provide the location of the data layers used to calculate the OHI assessments.  This file is created by data and functions in the eez_layers_meta_data folder, and they are used by calculate_scores_??.R.  
* other files can be ignored

Important files within the "eez", "antarctica", and "high seas" folders include:

* scores.csv: The OHI scores
* layers: All the data layers used by the OHI models to calculate scores
* conf: Files that are used to set up the OHI model parameters
    - config.R: define model parameters, weighting files, etc.
    - functions.R: functions used to calculate goal/subgoal status and trend scores
    - goals.csv: list of goals and corresponding weights (also where status years are defined for each assessment year)
    - pressures_matrix.csv: Weights for each pressure layer and goal
    - pressure_categories: Defines the pressure category for each pressure layer
    - resilience_matrix.csv: Indicates which resilience layers affect which goals
    - resilience_categories: Defines the resilience category for each resilience layer
   

### Creating a new scenario within an assessment (e.g., eez2012, eez2016, etc.)
1. Copy the folder of a previous scenario and change the year
2. Update the eez_layers_meta_data/layers_eez_file_locations.csv with the file paths and names of the data for each scenario
3. Update conf/goals.csv with appropriate status_year information (which year in the data layer is used for the scenario)
4. Update the calculate_scores_??.R as follows:
   - Add the "scenario" to the scenario list (we might want to move this information to a table at some point):
   - And update the sync functions so the functions.R files stay consistent

```
 # scenario list (need to add new scenarios here)
scenarios = list(
  eez2015     = list(
    layer   = 'layers_eez',
    fld_dir      = 'dir_2015a',
    fld_fn       = 'fn_2015a',
    f_spatial    = c('../ohiprep/Global/NCEAS-Regions_v2014/data/regions_gcs.js'),
    do           = T),  
  eez2014     = list(
    layer   = 'layers_eez',
  
```

```
 ### sync functions.R: 
# overwrite eez2012, eez2014, eez2015, with eez2013
for (dir in c('eez2012','eez2014', 'eez2015')){
  stopifnot(file.copy('eez2013/conf/functions.R', file.path(dir, 'conf/functions.R'), overwrite=T))
}
# overwrite antarctica2015 with antarctica2014
for (dir in c('antarctica2015')){
  stopifnot(file.copy('antarctica2014/conf/functions.R', file.path(dir, 'conf/functions.R'), overwrite=T))
}
# overwrite highseas2015 with highseas2014
for (dir in c('highseas2015')){
  stopifnot(file.copy('highseas2014/conf/functions.R', file.path(dir, 'conf/functions.R'), overwrite=T))
}

```
