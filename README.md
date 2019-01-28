ohi-global
==========
  
### Ocean Health Index assessment of global EEZ regions 
This repository includes files for the global OHI assessment using eez boundaries for 220 country and territorial regions.

General information about file structure is here: http://ohi-science.org/manual/#file-system-organization

#### Anatomy of the file structure

*eez* 

Files associated with calculating the OHI scores for scenarios from 2012 to present.

Some of the relevant files in the eez folder include:

* calculate_scores.R: code to calculate scores
* layers.csv: provides the location of the data layers used to calculate the OHI assessments.  This file is created by data and functions in the eez_layers_meta_data folder

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
    - scenario_data_years.csv: Links the scenario year to the corresponding year of the data for each data layer
* calculate\_scores_??.R: These files provide the code to calculate scores for the corresponding OHI assessment (the calculate_scores.R for the eez regions is located in the eez folder)
* layers\_??.csv: These files provide the location of the data layers used to calculate the OHI assessments.  This file is created by data and functions in the eez_layers_meta_data folder, and they are used by calculate_scores_??.R.  
* other files can be ignored at this point


*eez_layers_meta_data* 
Database for the data layers (data sources, names, etc.) and functions to create different data formats

*global_supplement* 
Documents and code to create methods document. layers_eez_base.csv describes filepaths of data layers used to calculate scores. 

*yearly_results* 
Post assessment analysis and visualization of data



