# OHI gh-pages branch


To build the website, run [`rmarkdown::render_site()`](http://rmarkdown.rstudio.com/rmarkdown_websites.html), which knits each Rmarkdown (*.Rmd) based on parameters in `_site.R` and `_site.yml` before pushing the files back to Github. These Rmd files also pull from the `data_branch`, typically `draft`, that gets copied into `[gh_repo]_[gh_branch_data]`. 

## Workflow to update website
This is generally done one time per year, in early November before an early December public release. 

This part of the work happens in the `gh-pages` branch after all of the appropriate metadata documentation updates have taken place in the `draft` branch. We are able to develop the website updates before making public updates to the website (ohi-science.org/ohi-global) by doing two things: 

1. Don't push any html files before launch (rendering them is great, but just push the .Rmds)
2. Develop from the `draft` branch (and then switch to the `published` branch and rebuild at the time of launch by just toggling this [on one line](https://github.com/OHI-Science/ohi-global/blob/d183f80397d38c8b3b06a33f99bd59474a6c38d1/_site.R#L19))

### First, build the website

From the console, run `rmarkdown::render_site()`. This will run `_site.R` along with `_site.yml` to create the website. The `.R` script will copy each Rmd file from the `draft` (ultimately `published`) branch and store a local copy that will be use to knit each webpage. 

If `rmarkdown::render_site()` fails, look first at this `_site.R` and walk through the code to see where the errors are occurring. The most likely explanation is that there has been some organizational structure to the metadata documentation and the filepaths will need to be updated. 

When `rmarkdown::render_site()` works, it will pull all the new updates and rebuild several pages: 

- /goals.html
- /layers.html
- /layers_table.html
- /scores.html

Hooray! 

That is the biggest lift for the website. 

### Home, Download & Fellows Pages

Next, you'll have to make slight modifications to the home (index.Rmd), download, and fellows pages. This involves changing 2018 to 2019 and updating URLs; they need to be done by hand but serve as a little checklist. Re-read each and update as you go, as you can. Pro-tip: there will be some things you can't update immediately, like the final scores flower plot on the home page. I go through and write `TODO` in these places, and as a final check, search each file for `TODO` and make sure there are none left (I delete as I do them).

Pro-tip: I write this as a comment in R: `#TODO` or html: `<!---TODO--->`



### Final things before publishing

- [ ] Rebuild website with final scores
- [ ] Look at rendered pages to make sure everything's great
- [ ] Search each page for `TODO`


---

The majority of this website architecture was coded by [Ben Best](https://github.com/bbest) with significant inputs from [Julie Lowndes](https://github.com/jules32).

