# Global OHI score metadata

All years (2012 to present) have been recalculated using the most up-to-date data and methods. 

Scores range from 0-100, with the exception of trend data which ranges from -1 to 1. Blank cells indicate that goal is not applicable to that region.  

csv file contains 7 variables and 170562 (in 2018) rows of data

| header | description | 
| ------ | ----------------------------------------------------------------------- | 
| scenario | Year (2012 to 2018) |
| goal | Code for the OHI goal (10 goals, represented by 2 characters; and 8 subgoals, represented by 3 characters), see table below for details | 
| long_goal | Full goal/subgoal name |
| dimension | Dimension of the OHI score that is being calculated, see table below for details |
| region_id | Numeric identifier of the global region, includes countries and territories |
| region_name | Name of the global region | 
| value | Scores ranging from 0-1 for all dimensions except trend, which is between -1 and 1 | 


### List of goals and subgoals
These are 10 services that oceans provide that are important to humans.  Four of the goals (biodiversity, livelihoods and economies, food provision and sense of place) are comprised of 2 subgoals.

|Goal/subgoal | Name |
| ------ | ----------- | 
| AO | Artisanal Opportunities score (goal) | 
| SPP | Species score (sub-goal of Biodiversity) | 
| BD | Biodiversity score (goal) | 
| HAB | Habitats score (sub-goal of Biodiversity) | 
| CP | Coastal Protection score (goal) | 
| CS | Carbon Storage score (goal) | 
| CW | Clean Waters score (goal) | 
| ECO | Economies score (sub-goal of Livelihoods & Economies) | 
| LE | Livelihoods & Economies score (goal) | 
| LIV | Livelihoods score (sub-goal of Livelihoods & Economies) | 
| FIS | Wild-caught Fisheries score (sub-goal of Food Provision) | 
| FIS | Food Provision score (goal) | 
| MAR | Mariculture score (sub-goal of Food Provision) | 
| ICO | Iconic Species score (sub-goal of Sense of Place) | 
| SP | Sense of Place score (goal) | 
| LSP | Lasting Special Places score (sub-goal of Sense of Place) | 
| NP | Natural Products score (goal) | 
| TR | Tourism & Recreation score (goal) | 


### List of dimension and short description
|Dimension | Description |
| ------ | ---------------------------------------------------------------------- | 
| score | Final OHI score that takes into account current status and predicted future status | 
| status | Current status of goal based on current state relative to reference state  | 
| future | Predicted future status (based on contributions of pressures, resilience, and trend) | 
| pressures | Cumulative pressures acting on a goal to reduce scores, used to calculate future status |
| resilience | Average resilience variables acting on a goal to increase scores, used to calculate future status |
| trend | Predicted proportional change in status after five years, based on average change in status per year multiplied by five, used to estimate future status |

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

