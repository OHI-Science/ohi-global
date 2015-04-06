### Tbx 2014a cheatsheet 
* temp file by JSL. July 22*

## to run the toolbox
* everything happens in ohi-global/calculate_scores_all.R: see below for specifics
* calculate_scores_all.r will do other prep in addition to the calls found in calculate_scores.r, which are within each scenario file. eg: ohi-global/eez2014/calculate_scores.R 


## calculate_scores_all
* 'do' flags: turn on/off which chunks to run


# check goals.csv to make sure the parameters passed make sense

## 2014a warnings and errors from July 22 run (JSL and MF)

Calculating Resilience...
goal: FIS
Aggregation function missing: defaulting to length
goal: MAR
Aggregation function missing: defaulting to length
goal: AO
Aggregation function missing: defaulting to length
goal: NP
Aggregation function missing: defaulting to length
... [[JSL: huge number of warnings; longer than the history]]
...
Aggregation function missing: defaulting to length
goal: CS
Aggregation function missing: defaulting to length
...
Aggregation function missing: defaulting to length
goal: CP
Aggregation function missing: defaulting to length
...
Aggregation function missing: defaulting to length
goal: TR
Aggregation function missing: defaulting to length
goal: LIV
Aggregation function missing: defaulting to length
goal: ECO
Aggregation function missing: defaulting to length
goal: ICO
Aggregation function missing: defaulting to length
goal: LSP
Aggregation function missing: defaulting to length
goal: CW
Aggregation function missing: defaulting to length
goal: HAB
Aggregation function missing: defaulting to length
...
Aggregation function missing: defaulting to length
  skipping region 35 for HAB since no matching conditions, but having components: coral, mangrove
Aggregation function missing: defaulting to length
...
Aggregation function missing: defaulting to length
  skipping region 99 for HAB since no matching conditions, but having components: mangrove, seagrass
Aggregation function missing: defaulting to length
...
Aggregation function missing: defaulting to length
  skipping region 105 for HAB since no matching conditions, but having components: seaice_edge
Aggregation function missing: defaulting to length
...
Aggregation function missing: defaulting to length
  skipping region 151 for HAB since no matching conditions, but having components: coral, seagrass
  skipping region 152 for HAB since no matching conditions, but having components: coral, mangrove, seagrass
Aggregation function missing: defaulting to length
...
Aggregation function missing: defaulting to length
  skipping region 156 for HAB since no matching conditions, but having components: coral, saltmarsh
Aggregation function missing: defaulting to length
...
Aggregation function missing: defaulting to length
  skipping region 215 for HAB since no matching conditions, but having components: coral, seagrass
Aggregation function missing: defaulting to length
...
Aggregation function missing: defaulting to length
  skipping region 244 for HAB since no matching conditions, but having components: coral, mangrove
  skipping region 245 for HAB since no matching conditions, but having components: coral, mangrove, seagrass
Aggregation function missing: defaulting to length
...
Aggregation function missing: defaulting to length
  skipping region 250 for HAB since no matching conditions, but having components: coral, mangrove, seagrass
goal: SPP
Aggregation function missing: defaulting to length

  getwd(): /Users/julialowndes/github/ohi-global/eez2014
Calculating Status and Trend for FIS...


Calculating Goal Score and Likely Future for FIS...
Error: min(d$r, na.rm = T) >= 0 && max(d$r, na.rm = T) <= xlim[2] is not TRUE


### also ran 2013a to test if that was working.

Instead of those warnings (above), should just say:
goal: HAB
  skipping region 35 for HAB since no matching conditions, but having components: coral, mangrove
...
  skipping region 250 for HAB since no matching conditions, but having components: coral, mangrove, seagrass
  
  
* first time run with clean, restarted workspace. No problems. 
* second time run without restarting workspace, and these warnings:
Warning messages:
1: In min(status$score, na.rm = T) :
  no non-missing arguments to min; returning Inf
2: In max(status$score, na.rm = T) :
  no non-missing arguments to max; returning -Inf


# other
* system.file(package='ohicore') # check which version of ohicore you're running (developer or package). (Load_all is a function of devtools)
* score comparisons in git-annex on Neptune:: data_edit/git-annex/Global/NCEAS-OHI-Scores-Archive/scores
