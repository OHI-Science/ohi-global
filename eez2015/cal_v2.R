CalculateAll_v2 <- function (conf, layers, debug = F) 
{
  if (exists("scores", envir = .GlobalEnv)) 
    rm(scores, envir = .GlobalEnv)
  if ("Setup" %in% ls(conf$functions)) {
    cat("Running Setup()...\n")
    conf$functions$Setup()
  }
  goals_X = conf$goals %>% filter(!is.na(preindex_function)) %>% 
    arrange(order_calculate)
  scores = data.frame(goal = character(0), dimension = character(0), 
                      region_id = integer(0), score = numeric())
  for (i in 1:nrow(goals_X)) {
    g = goals_X$goal[i]
    cat(sprintf("Calculating Status and Trend for each region for %s...\n", 
                g))
    assign("scores", scores, envir = conf$functions)
    if (nrow(subset(scores, goal == g & dimension %in% c("status", 
                                                         "trend"))) != 0) 
      stop(sprintf("Scores were assigned to goal %s by previous goal function.", 
                   g))
    scores_g = eval(parse(text = goals_X$preindex_function[i]), 
                    envir = conf$functions)
    if (nrow(scores_g) > 0) {
      scores = rbind(scores, scores_g[, c("goal", "dimension", 
                                          "region_id", "score")])
    }
  }
  cat(sprintf("Calculating Pressures for each region...\n"))
  scores_P = CalculatePressuresAll(layers, conf, gamma = conf$config$pressures_gamma, 
                                   debug=TRUE)
  scores = rbind(scores, scores_P)
  cat(sprintf("Calculating Resilience for each region...\n"))
  cat(sprintf("Note: each goal in resilience_matrix.csv must have at least one resilience field\n"))
  scores = rbind(scores, CalculateResilienceAll_v2(layers, conf, 
                                                debug=TRUE))
  scores = data.frame(scores)
  goals_G = as.character(unique(subset(scores, dimension == 
                                         "status", goal, drop = T)))
  for (g in goals_G) {
    cat(sprintf("Calculating Goal Score and Likely Future for each region for %s...\n", 
                g))
    v = scores %>% filter(goal == g) %>% spread(dimension, 
                                                score)
    for (col in c("status", "trend", "pressures", "resilience")) {
      if (!col %in% names(v)) {
        cat(sprintf("  missing %s dimension, assigning NA!\n", 
                    col))
        v[col] = NA
      }
    }
    x = CalculateGoalIndex(id = v$region_id, status = v$status/100, 
                           trend = v$trend, resilience = v$resilience/100, pressure = v$pressures/100, 
                           DISCOUNT = conf$config$goal_discount, BETA = conf$config$goal_beta, 
                           default_trend = conf$config$default_trend)
    x$score = x$score * 100
    x$xF = x$xF * 100
    scores_G = x %>% dplyr::select(region_id = id, future = xF, 
                                   score) %>% gather(dimension, score, -region_id) %>% 
      mutate(goal = g) %>% select(goal, dimension, region_id, 
                                  score)
    scores = rbind(scores, scores_G)
  }
  goals_Y = subset(conf$goals, !is.na(postindex_function))
  supragoals = subset(conf$goals, is.na(parent), goal, drop = T)
  supragoals
  for (i in 1:nrow(goals_Y)) {
    cat(sprintf("Calculating post-Index function for each region for %s...\n", 
                goals_Y$goal[i]))
    assign("scores", scores, envir = conf$functions)
    scores = eval(parse(text = goals_Y$postindex_function[i]), 
                  envir = conf$functions)
  }
  cat(sprintf("Calculating Index score for each region for supragoals using goal weights...\n"))
  scores = rbind(scores, scores %>% filter(dimension == "score", 
                                           goal %in% supragoals) %>% merge(conf$goals %>% select(goal, 
                                                                                                 weight)) %>% group_by(region_id) %>% dplyr::summarise(score = weighted.mean(score, 
                                                                                                                                                                             weight, na.rm = T)) %>% mutate(goal = "Index", dimension = "score") %>% 
                   data.frame())
  cat(sprintf("Calculating Likely Future State for each region for supragoals using goal weights...\n"))
  scores = rbind(scores, scores %>% filter(dimension == "future", 
                                           goal %in% supragoals) %>% merge(conf$goals %>% select(goal, 
                                                                                                 weight)) %>% group_by(region_id) %>% dplyr::summarise(score = weighted.mean(score, 
                                                                                                                                                                             weight, na.rm = T)) %>% mutate(goal = "Index", dimension = "future") %>% 
                   data.frame())
  if ("PreGlobalScores" %in% ls(conf$functions)) {
    cat(sprintf("Calculating Post-process PreGlobalScores() function for each region...\n"))
    scores = conf$functions$PreGlobalScores(layers, conf, 
                                            scores)
  }
  cat(sprintf("Calculating scores for ASSESSMENT AREA (region_id=0) by area weighting...\n"))
  scores = rbind(scores, scores %>% filter(dimension %in% c("score", 
                                                            "status", "future")) %>% merge(SelectLayersData(layers, 
                                                                                                            layers = conf$config$layer_region_areas, narrow = T) %>% 
                                                                                             dplyr::select(region_id = id_num, area = val_num)) %>% 
                   group_by(goal, dimension) %>% summarise(score = weighted.mean(score, 
                                                                                 area, na.rm = T), region_id = 0))
  if ("FinalizeScores" %in% ls(conf$functions)) {
    cat(sprintf("Calculating FinalizeScores function...\n"))
    scores = conf$functions$FinalizeScores(layers, conf, 
                                           scores)
  }
  stopifnot(sum(duplicated(scores[, c("region_id", "goal", 
                                      "dimension")])) == 0)
  return(scores)
}