### running Tbx 2014a cheatsheet 
* temp file by JSL. July 22*

## to run the toolbox
* everything happens in ohi-global/calculate_scores_all.R: see below for specifics
* calculate_scores_all.r will do other prep in addition to the calls found in calculate_scores.r, which are within each scenario file. eg: ohi-global/eez2014/calculate_scores.R 


## calculate_scores_all
* 'do' flags: turn on/off which chunks to run


# check goals.csv to make sure the parameters passed make sense


# other
system.file(package='ohicore') # check which version of ohicore you're running (developer or package). (Load_all is a function of devtools)
