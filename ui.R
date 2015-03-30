# see global.R

#cat(ls(), file='~/Downloads/ohicore_ui_ls.txt'); system('open ~/Downloads/ohicore_ui_ls.txt')

#---- customize for leaflet map
customHeaderPanel <- function(title,windowTitle=title){
  tagList(tags$head(
    tags$title(windowTitle),
    tags$script(src='spatial/regions_gcs.js')))}      # assume regions geojson variable set by spatial/regions_gcs.js

#---- define ui
shinyUI(bootstrapPage(div(
  class='container-fluid',

  # browser title
  div(
    class= "row-fluid",
    customHeaderPanel(sprintf('OHI App for %s', study_area))),

  div(
    class = "row-fluid",
    HTML('<table><tr><td>Branch/Scenario:</td><td>'),
    selectInput(
      'sel_branch_scenario', label='',
      choices=branches_scenarios,
      selected=sprintf('%s/%s', default_branch, default_scenario), multiple=F, selectize=T),
    HTML('</td></tr></table>')),

  htmlOutput('git_commit'),
  htmlOutput('ls_files'),

  div(
    class = "row-fluid",
    uiOutput('ui_tabsetpanel')),

  div(
    class = "row-fluid",
    HTML(renderMarkdown(
      file=NULL,
      text=paste0(
        with(
          ohicore_app,
          sprintf(
            'Versions -- App: [%s@%.7s](https://github.com/%s/%s/commit/%.7s)',
            git_repo, git_commit, git_owner, git_repo, git_commit)),
        sprintf(
          ', Data: [%s@%.7s](https://github.com/%s/%s/commit/%.7s)',
          git_repo, repo_head@sha, git_owner, git_repo, repo_head@sha)))))
)))
