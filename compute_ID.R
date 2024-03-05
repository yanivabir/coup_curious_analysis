# This file contains functions computing individual differences index measures 
# from questionnaire responses
library(testit)

computeAlpha <- function(data, name){
  alpha <- psych::alpha(data[, -c("PID"), with = F])
  return(data.table(measure = name,
                    alpha = alpha$total$raw_alpha))
}

computeNaive <- function(quest){
  
  # Placeholder for alpha values
  alphas <- data.table(measure = character(), alpha = numeric())
  
  ## Compute coup relevance
  # Select items
  coup_rel_items <- 
    quest[, grepl("coup|PID", names(quest)), with = F]
  
  coup_relevance_items <- c(sapply(1:7, 
                                   function(x) sprintf("coup_rel_%02d", x)), 
                            "coup_rel_22")
  
  coup_attitude_items <- c(sapply(8:21, 
                                  function(x) sprintf("coup_rel_%02d", x)), 
                           "coup_rel_23")
  
  # Standardize each variable
  cols = colnames(coup_rel_items[, -c("PID")])
  coup_rel_items[, (cols) := lapply(.SD, scale), .SDcols=cols]
  
  # Compute alphas
  alphas <- rbind(alphas, suppressWarnings(computeAlpha(coup_rel_items, 
                                                        "naive_coup_rel_quest")))
  alphas <- rbind(alphas, suppressWarnings(computeAlpha(coup_rel_items[, 
                                                         coup_relevance_items, 
                                                         with = F], 
                                          "naive_coup_relevance")))
  alphas <- rbind(alphas, suppressWarnings(computeAlpha(coup_rel_items[, 
                                                         coup_attitude_items, 
                                                         with = F], 
                                          "naive_coup_attitude")))
  
  
  # Compute grand average
  coup_rel <- coup_rel_items[, .(naive_coup_rel_quest = 
                                       rowMeans(.SD, na.rm = T)), 
                                 by = "PID"]
  # Compute relevance average
  coup_rel_relevance <- 
    coup_rel_items[, .(naive_coup_relevance = 
                         rowMeans(.SD, na.rm = T)),
                   .SDcols = coup_relevance_items, by = "PID"]
  # Compute attitude average
  coup_rel_attitude <- 
    coup_rel_items[, .(naive_coup_attitude = 
                         rowMeans(.SD, na.rm = T)),
                   .SDcols = coup_attitude_items, by = "PID"]
  
  coup_rel <- merge(coup_rel, coup_rel_relevance, by = "PID",
                    all.x = T, all.y = T)
  
  coup_rel <- merge(coup_rel, coup_rel_attitude, by = "PID",
                    all.x = T, all.y = T)
  
  assert("wrong number of participants in coup_rel", 
         nrow(coup_rel) == nrow(quest))
  assert("wrong number of columns in coup_rel", ncol(coup_rel) == 4)
  
  ## Compute affect
  # Select items
  affect_items <- 
    quest[, grepl("PID|stai|gallup", names(quest)), with = F]
  
  # Standardize each variable
  cols = colnames(affect_items[, -c("PID")])
  affect_items[, (cols) := lapply(.SD, scale), .SDcols=cols]
  
  # Compute alpha
  alphas <- rbind(alphas, suppressWarnings(computeAlpha(affect_items, 
                                                        "naive_affect")))
  
  # Compute average
  affect <- affect_items[, .(naive_affect = rowMeans(.SD, na.rm = T)), 
                               by = "PID"]
  
  ## Compute motivation
  # Select items
  motivation_items <- 
    quest[, grepl("PID|reg_Q|apathy", names(quest)), with = F]
  
  # Standardize each variable
  cols = colnames(motivation_items[, -c("PID")])
  motivation_items[, (cols) := lapply(.SD, scale), .SDcols=cols]
  
  # Compute alpha
  alphas <- rbind(alphas, suppressWarnings(computeAlpha(motivation_items, 
                                                        "naive_motivation")))
  
  # Compute average
  motivation <- motivation_items[, .(naive_motivation = 
                                           rowMeans(.SD, na.rm = T)), 
                             by = "PID"]
  
  ## Compute coup regulatory focus
  # Select items
  reg_focus_items <- 
    quest[, grepl("PID|coup_reg", names(quest)), with = F]
  
  # Remove NAs
  reg_focus_items <- 
    reg_focus_items[rowSums(is.na(reg_focus_items[, .SD, .SDcols = -"PID"])) < 
                      ncol(reg_focus_items) - 1]
  
  
  # Standardize each variable
  cols = colnames(reg_focus_items[, -c("PID")])
  reg_focus_items[, (cols) := lapply(.SD, scale), .SDcols=cols]
  
  prevention <- colnames(reg_focus_items)[6:9]
  promotion <- colnames(reg_focus_items)[2:5]
  
  # Compute alpha
  alphas <- rbind(alphas, suppressWarnings(computeAlpha(reg_focus_items[, promotion, with = F], 
                                                        "naive_coup_reg_promote")))
  
  alphas <- rbind(alphas, suppressWarnings(computeAlpha(reg_focus_items[, prevention, with = F], 
                                                        "naive_coup_reg_prevent")))
  
  
  # Compute average
  coup_reg_focus <- reg_focus_items[, 
                                    .(naive_prevention = rowMeans(.SD[, prevention, with = F], na.rm = T),
                                      naive_promotion = rowMeans(.SD[, promotion, with = F], na.rm = T)), 
                                    by = "PID"]
  
  
  ## Merge all
  naive_ID <- merge(coup_rel, affect, by = "PID")
  naive_ID <- merge(naive_ID, motivation, by = "PID")
  naive_ID <- merge(naive_ID, coup_reg_focus, by = "PID", all.x = T)
  
  assert("wrong number of participants in coup_rel", 
         nrow(naive_ID) == nrow(quest))
  assert("wrong number of columns in coup_rel", ncol(naive_ID) == 8)
  
  return(list(naive_ID, alphas))
}
