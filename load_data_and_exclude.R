# This script opens data and excludes particiapnts.
library(gsubfn)
source("exclude.R")

load_exclude <- function(sampleName){
  preprocDatDir <- file.path("..", "data", sampleName, "preproc")
  
  # Load data
  wait <- fread(file.path(preprocDatDir, "wait_data.csv"))
  rating <- fread(file.path(preprocDatDir, "rating_data.csv"))
  know_test <- fread(file.path(preprocDatDir, "knowledge_test_data.csv"))
  prob_judge <- fread(file.path(preprocDatDir, "prob_judge_data.csv"))
  quest <- fread(file.path(preprocDatDir, "quest_data.csv"))
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
    if ((d-1) %in% as.numeric(dates)){
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
  
  # Value transformations
  wait[, choice := factor(choice, levels = c("skip", "wait", "know"))]
  wait[, block := factor(block, levels = c("general", "coup"))]
  
  return(list(wait, rating_clps, know_test, prob_judge, quest, quality))
}
