# OHI 2018 global metadata

### Metadata for the scores data  

All years (2012 to 2018) have been recalculated using the most up-to-date data and methods. 

Scores range from 0-100, with the exception of trend data which ranges from -1 to 1. Blank cells indicate that goal is not applicable to that region.  

csv file contains 7 variables and 146196 rows of data

| header | description | 
| ------ | ---------------------------------- | 
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
| ------ | ------------------------------------ | 
| score | Final OHI score that takes into account current status and predicted future status | 
| status | Current status of goal based on current state relative to reference state  | 
| future | Predicted future status (based on contributions of pressures, resilience, and trend) | 
| pressures | Cumulative pressures acting on a goal to reduce scores, used to calculate future status |
| resilience | Average resilience variables acting on a goal to increase scores, used to calculate future status |
| trend | Predicted proportional change in status after five years, based on average change in status per year multiplied by five, used to estimate future status |


