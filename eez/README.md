# eez folder structure

Files in the eez folder include:

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

