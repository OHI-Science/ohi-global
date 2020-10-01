
function (scenario_year, commit = "previous", file_name, scenario_name = "eez", 
          save_csv = FALSE, save_png = FALSE, NA_compare = TRUE) {
  cat("Wait for it....this takes a few seconds \n\n")
  scenario_path <- here(scenario_name)
  if (commit == "previous") {
    commit2 = substring(git2r::commits(git2r::repository(here()))[[1]][1], 
                        1, 7)
  }
  else {
    if (commit == "final_2014") {
      commit2 = "4da6b4a"
    }
    else {
      commit2 = commit
    }
  }
  tmp <- git2r::remote_url(git2r::repository(here()))
  repo_name <- stringr::str_split(tmp, "/")[[1]][5]
  repo_name_fix <- gsub(pattern = ".git", replacement = "", 
                        x = repo_name)
  org <- stringr::str_split(tmp, "/")[[1]][4]
  data_old <- read.csv(file.path("https://raw.githubusercontent.com", 
                                 org, repo_name_fix, commit2, scenario_name, "scores.csv")) %>% 
    dplyr::rename(old_score = score)
  if (sum(names(data_old) == "year") < 1) {
    data_new <- read.csv(sprintf(here("%s/scores.csv"), scenario_name)) %>% 
      dplyr::left_join(data_old, by = c("goal", "dimension", 
                                        "region_id")) %>% dplyr::mutate(year = substring(date(), 
                                                                                         21, 24)) %>% dplyr::mutate(change = score - old_score)
    scenario_year <- substring(date(), 21, 24)
  }
  else {
    data_new <- read.csv(sprintf(here("%s/scores.csv"), scenario_name)) %>% 
      dplyr::left_join(data_old, by = c("year", "goal", 
                                        "dimension", "region_id")) %>% dplyr::mutate(change = score - 
                                                                                       old_score) %>%
      dplyr::filter(goal %in% c("HAB", "Index", "BD"))
  }
  if (length(list.files("eez/spatial", pattern = "regions_list.csv")) > 
      0) {
    rgns <- read.csv(sprintf(here("%s/spatial/regions_list.csv"), 
                             scenario_name), stringsAsFactors = FALSE) %>% dplyr::select(region_id = rgn_id, 
                                                                                         rgn_name)
    data_new <- data_new %>% dplyr::left_join(rgns, by = "region_id") %>% 
      dplyr::mutate(rgn_name = ifelse(region_id == 0, "Region", 
                                      rgn_name))
  }
  else {
    data_new$rgn_name = ""
  }
  suppressWarnings(p <- ggplot2::ggplot(filter(data_new, year == 
                                                 scenario_year), ggplot2::aes(x = goal, y = change, color = dimension)) + 
                     ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, 
                                                                                              hjust = 1)) + ggplot2::labs(title = paste("Score compared to commit:", 
                                                                                                                                        commit, sep = " "), y = "Change in score", x = "") + 
                     ggplot2::scale_x_discrete(limits = c("Index", "AO", "SPP", 
                                                          "BD", "HAB", "CP", "CS", "CW", "FIS", "FP", "MAR", 
                                                          "ECO", "LE", "LIV", "NP", "LSP", "SP", "ICO", "TR")) + 
                     ggplot2::scale_colour_brewer(palette = "Dark2") + ggplot2::geom_jitter(ggplot2::aes(text = paste0("rgn = ", 
                                                                                                                       region_id, "\n", rgn_name)), position = ggplot2::position_jitter(width = 0.2, 
                                                                                                                                                                                        height = 0), shape = 19, size = 1))
  plotly_fig <- plotly::ggplotly(p, width = 800, height = 450)
  htmlwidgets::saveWidget(plotly::as_widget(plotly_fig), "tmp_file.html", 
                          selfcontained = TRUE)
  my.file.rename <- function(from, to) {
    todir <- dirname(to)
    if (!isTRUE(file.info(todir)$isdir)) 
      dir.create(todir, recursive = TRUE)
    file.rename(from = from, to = to)
  }
  my.file.rename(from = "tmp_file.html", to = sprintf(here("%s/score_check/%s_score_check.html"), 
                                                      scenario_name, file_name, Sys.Date()))
  cat("An interactive plot in the 'score_check' folder has been created \n")
  if (save_png) {
    ggplot2::ggsave(sprintf(here("%s/score_check/%s_score_check.png"), 
                            scenario_name, file_name, Sys.Date()), width = 8, 
                    height = 5)
    cat("A png plot has been saved in the 'score_check' folder \n")
  }
  if (save_csv) {
    write.csv(data_new, sprintf(here("%s/score_check/%s_diff_data_%s.csv"), 
                                scenario_name, file_name, Sys.Date()), row.names = FALSE)
    cat("A csv file comparing the scores has been saved in the 'score_check' folder \n")
  }
  if (NA_compare) {
    data_NA <- data_new %>% dplyr::filter(year == scenario_year) %>% 
      dplyr::mutate(NA_same = ifelse(is.na(score) & is.na(old_score), 
                                     1, 0)) %>% dplyr::mutate(NA_new = ifelse(is.na(score), 
                                                                              1, 0)) %>% dplyr::mutate(NA_old = ifelse(is.na(old_score), 
                                                                                                                       1, 0)) %>% dplyr::mutate(diff_new = NA_new - NA_same) %>% 
      dplyr::mutate(diff_old = NA_old - NA_same) %>% dplyr::summarize(new = sum(diff_new), 
                                                                      old = sum(diff_old))
    cat("\n NA check results: \n")
    if (sum(data_NA) == 0) {
      cat(sprintf("Excellent! The number of NA values in %s has not changed! \n", 
                  scenario_year))
    }
    else {
      cat(sprintf("The new version of data has an additional %s missing values compared to the previous version \n\n                  The previous version of data has an additional %s missing values compared to the new version \n\n                  Examine the .csv file in the 'score_check' folder to determine where these discrepancies occur", 
                  data_NA$new, data_NA$old))
    }
  }
}