library(git2r)
library(shinyapps)

# vars
dir_ohicore = '~/github/ohicore'
git_url    = 'https://github.com/ohi-science/ohi-global'
git_branch     = 'dev'      # branch in repo to use
scenario    = 'eez2013'  # default scenario to load
dir_repo    = '~/github/ohi-global'
dir_app     = '~/tmp/ohi-global_shinyapp'
app_name    = 'global'
        
# create clean app dir to contain shiny app files
unlink(dir_app, recursive=T, force=T)
dir.create(dir_app, showWarnings=F, recursive=T)
setwd(dir_app)
      
# copy ohicore shiny app files
shiny_files = list.files(file.path(dir_ohicore, 'inst/shiny_app'), recursive=T)
for (f in shiny_files){ # f = shiny_files[1]
  dir.create(dirname(f), showWarnings=F, recursive=T)
  suppressWarnings(file.copy(file.path(dir_ohicore, 'inst/shiny_app', f), f, overwrite=T, recursive=T, copy.mode=T, copy.date=T))
}
    
# write config
cat(sprintf('# configuration for ohi-science.shinyapps.io/%s
git_url: %s
git_branch: %s
dir_scenario: %s
tabs_hide: Calculate, Report
debug: False
last_updated: %s
', app_name, git_url, git_branch, scenario, Sys.Date()), file='app_config.yaml')
    
# allow app to populate github repo locally
if (file.exists('github')){
  unlink('github', recursive=T, force=T)
}
  
# dir_app='~/tmp/ohi-global_shinyapp'; app_name='global'
shinyapps::deployApp(appDir=dir_app, appName=app_name, upload=T, launch.browser=T, lint=T)