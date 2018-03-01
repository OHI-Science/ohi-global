ohi-global
==========
  
### Global assessments (EEZ, Antarctica, and High Seas) and scenarios of the Ocean Health Index 

#### Anatomy of the file structure

General information about file structure is here: http://ohi-science.org/manual/#file-system-organization

Three OHI assessments (for a range of scenario years) are included in this folder:

* eez (scenarios: 2012 - 2017): OHI for 220 country/territorial EEZ regions, this is typically considered the "OHI global assessment"
* antarctica (scenarios: 2014 - 2015): OHI for Antarctica CCAMLR regions (includes the high seas/FAO and EEZ regions)
* high seas (scenarios: 2014 - 2015): OHI for high seas regions (FAO regions without the EEZ regions or Antarctica/CCAMLR regions)
 

Additional files/folders include:

* eez_layers_meta_data: Database for the EEZ data layers (data sources, names, etc.) and functions to create different data formats
* global_supplement: Descriptions of data layers and goals 
* global (2015-2017): Post assessment analysis and visualization of data
* calculate\_scores_??.R: These files provide the code to calculate scores for the corresponding OHI assessment (the calculate_scores.R for the eez regions is located in the eez folder)
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
    - scenario_data_years.csv: Links the scenario year to the corresponding year of the data for each data layer
    
  * **
   #testing