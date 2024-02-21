# This script opens data and excludes participants based on criteria used in Abir et al. 2021
library(gsubfn)

printnum <- function(x){
  if (abs(x) < .001) {
    return(sprintf("%0.2e", x))
  }
  else if (abs(x) < .01) {
    return(sprintf("%0.3f", x))
  }
  else {
    return(sprintf("%0.2f", x))
  }
}

exclude_sess_1 <- function(quality, quest, wait, returnStats = F){
  
  # Initiate exclusion list
  PID_exclude <- c()
  
  # Exclude based on fluency
  pre <- length(PID_exclude)
  PID_exclude <- unique(append(PID_exclude, quest[fluent < 3, PID]))
  stats <- data.table(exclude_flunecy = length(PID_exclude) - pre, exclude_flunecy_pct = printnum((length(PID_exclude) - pre) * 100 / nrow(quest)))
  print(sprintf("Excluding %d participants (%s%%) reporting less than good Hebrew fluency", 
                stats$exclude_flunecy[1], stats$exclude_flunecy_pct[1]))
  
  # Exclude participants with more than 5 important interactions
  pre <- length(PID_exclude)
  PID_exclude <- unique(append(PID_exclude, quality[important_interactions > 5, PID]))
  stats$exclude_interactions <- length(PID_exclude) - pre
  stats$exclude_interactions_pct <- printnum((length(PID_exclude) - pre) * 100 / nrow(quality))
  print(sprintf("Excluding %d participants (%s%%) with more than 5 browser interactions during wait task or covariate ratings", 
                stats$exclude_interactions[1], stats$exclude_interactions_pct[1]))
  
  # Exclude participants who did not respond on more than 20% of wait trials
  pre <- length(PID_exclude)
  PID_exclude <- unique(append(PID_exclude, quality[prop_missed > .2, PID]))
  stats$exclude_no_resp <- length(PID_exclude) - pre
  stats$exclude_no_resp_pct <- printnum((length(PID_exclude) - pre) * 100 / nrow(quality))
  print(sprintf("Excluding %d participants (%s%%) who did not respond to more than 20%% of WTW trials", 
                stats$exclude_no_resp[1], stats$exclude_no_resp_pct[1]))
  
  # Exclude participants lower by more than 2SD from group average RT
  mean_rt <- wait[, .(rt = mean(choice_rt, na.rm = T)), by = PID]
  mean_rt[, zrt := scale(rt)]
  
  pre <- length(PID_exclude)
  PID_exclude <- unique(append(PID_exclude, mean_rt[zrt < -2, PID]))
  stats$exclude_rt <- length(PID_exclude) - pre
  stats$exclude_rt_pct <- printnum((length(PID_exclude) - pre) * 100 / nrow(quality))
  print(sprintf("Excluding %d participants (%s%%) with question RTs lower by more than 2 SD from group mean", 
                stats$exclude_rt[1], stats$exclude_rt_pct[1]))
  
  
  if (returnStats){
    return(list(PID_exclude, stats))
  }else{
    return(PID_exclude)
  }
}

load_exclude <- function(sampleName){
  preprocDatDir <- file.path("..", "data", sampleName, "preproc")
  
  # Load data
  wait <- fread(file.path(preprocDatDir, "wait_data.csv"))
  rating <- fread(file.path(preprocDatDir, "rating_data.csv"))
  know_test <- fread(file.path(preprocDatDir, "knowledge_test_data.csv"))
  prob_judge <- fread(file.path(preprocDatDir, "prob_judge_data.csv"))
  load(file.path(preprocDatDir, "quest_data.rda"))
  quality <- fread(file.path(preprocDatDir, "quality_data.csv"), drop = c(1))
  
  # Exclude participants
  excluded_sess1 <- exclude_sess_1(quality, quest, wait)
  
  wait <- wait[!(PID %in% excluded_sess1)]
  rating <- rating[!(PID %in% excluded_sess1)]
  know_test <- know_test[!(PID %in% excluded_sess1)]
  prob_judge <- prob_judge[!(PID %in% excluded_sess1)]
  quest <- quest[!(PID %in% excluded_sess1)]
  quality <- quality[!(PID %in% excluded_sess1)]
  
  # Merge dates
  quality <- quality[!is.na(date)]
  dates <- unique(quality$date)
  for (d in dates){
    if ((d-2) %in% as.numeric(dates)){
      quality[date == d, date := date - 2]
    }
    else if ((d-1) %in% as.numeric(dates)){
      quality[date == d, date := date - 1]
    }
  }
  
  # Collapse across multiple versions of each probe
  rating_clps <- rating[, 
                        .(PID = PID,
                          sess = sess,
                          questionId = questionId,
                          block = block,
                          confidence = 
                            ifelse(is.na(confidence_neg), 
                                   confidence_pos, 4 - confidence_neg) + 1,
                          affect = ifelse(is.na(affect_neg), 
                                          affect_pos, 4 - affect_neg) + 1,
                          congruence = ifelse(is.na(congruence_neg), 
                                              congruence_pos, 4 - congruence_neg) + 1,
                          useful = ifelse(is.na(useful_neg), 
                                          useful_pos, 4 - useful_neg) + 1)]
  
  # Remove skipped trials
  pre <- nrow(wait)
  wait <- wait[choice != "no_resp"]
  print(sprintf("Removing %0d trials with no response (%2.0f%% of trials)",
                pre - nrow(wait), (pre - nrow(wait))/pre*100))
  
  # Value transformations
  wait[, choice := factor(choice, levels = c("skip", "wait", "know"))]
  wait[, block := factor(block, levels = c("general", "coup"))]
  
  return(list(wait, rating_clps, know_test, prob_judge, quest, quality))
}
