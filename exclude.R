# This script contains functions to exclude participants based on criteria 
# used in Abir et al. 2021 

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