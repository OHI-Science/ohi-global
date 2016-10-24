# OHI gh-pages branch

These files provide the template for creating a website for an OHI repository per [Github Pages](http://pages.github.com) by populating the HTML files from Rmarkdown into the gh-pages branch of the Github repo.

Normally these files are translated from template files into the website using `ohirepos::deploy_website()` function, like so:

```r
library(ohirepos) # devtools::install_github('ohi-science/ohirepos')

# ohi-global
deploy_website('ohi-global', 'Global', 'eez2015', dir_out='~/Desktop/ohirepos_tmp', del_out=F, open_url=T)
#  cannot open file './ohi-global_draft/eez2015/conf/goals.Rmd': No such file or directory

# baltic
deploy_website('bhi', 'Baltic', 'baltic2015', dir_out='~/Desktop/ohirepos_tmp', del_out=F, open_url=T)
```

When [`rmarkdown::render_site(dir_web)`](http://rmarkdown.rstudio.com/rmarkdown_websites.html) runs, it knits each Rmarkdown (*.Rmd) based on parameters in `_site.R` and `_site.yml` before pushing the files back to Github. These Rmd files also pull from the `data_branch`, typically `draft`, that gets copied into `[gh_repo]_[gh_branch_data]`. 

## Development

In practice, for developing these Rmarkdown files, I launch RStudio with `ohirepos/inst/gh-pages/gh-pages.Rproj` to set the working directory here, and create the `_site.yml` and `_site.R` for whichever repo, knit with the Build tab in RStudio > Build Website, and add the `[gh_repo]_[gh_branch_data]` folder to `.gitignore`.

Here's how to generate the `_site.yml` and `_site.R` manually for local development in `ohirepos/inst/gh-pages`:

```r
# library(devtools); load_all()

# ohi-global
gh_repo='ohi-global'; study_area='Global'; scenario_dir='eez2015'

# bhi
gh_repo='bhi'       ; study_area='Baltic'; scenario_dir='baltic2015'

# vars
gh_branch_data='draft'
app_url=sprintf('http://ohi-science.nceas.ucsb.edu/%s', gh_repo); 
ohirepos_commit = devtools:::local_sha('ohirepos')
dir_data = sprintf('%s_%s', gh_repo, gh_branch_data)

# brew config files
brew::brew(system.file('gh-pages/_site.brew.yml', package='ohirepos'), '_site.yml')
brew::brew(system.file('gh-pages/_site.brew.R'  , package='ohirepos'), '_site.R'  )

# get data branch 
system(sprintf('git clone https://github.com/ohi-science/%s.git %s', dir_data))
# OR copy from local
system(sprintf('cp -rf ~/github/%s %s', gh_repo, dir_data))

# update data branch with latest on Github
system(sprintf('cd %s; git reset -q --hard origin/%s', dir_data, gh_branch_data))

# be sure to add to .gitignore
cat(sprintf('\n%s', dir_data), file='.gitignore', append=T)

# render and launch
rmarkdown::render_site('.')
utils::browseURL('index.html')
```

