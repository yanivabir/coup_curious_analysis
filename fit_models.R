# This script fits models to the data

# Load libraries
library(data.table)
setDTthreads(11)
library(ggplot2)
library(brms)
source("ginsburg.R")

# Parameters
sampleName <- "v1.01"
savedModelsDir <- file.path("..", "saved_models")

# Open data
source("load_data_and_exclude.R")
list[wait, rating_clps, know_test, 
     prob_judge, quest, quality] = load_exclude(sampleName)


# No rating choice model ----

# Standardize wait duration
wait[, wait_s := scale(wait_duration)]

# Contrasts for block
contrasts(wait$block) <- c(-0.5, 0.5)

jobid <- launch_model(data = wait,
                      formula = 'bf(choice ~ 1 + wait_s + block + (1 + wait_s + block | PID) + 
              (1 + wait_s| questionId)) + categorical(refcat = "skip")',
                      prior = 'prior(normal(0,1), class = "b", dpar = "muknow") +
              prior(normal(0,1), class = "b", dpar = "muwait") +
              prior(normal(0,1), class = "Intercept", dpar = "muknow") +
              prior(normal(0,1), class = "Intercept", dpar = "muwait") +
              prior(normal(0,1), class = "sd", dpar = "muknow") +
              prior(normal(0,1), class = "sd", dpar = "muwait") +
              prior(lkj(2), class = "cor")',
                      model_name = "chm0",
                      save_output = T,
                      iter = 3000,
                      warmup = 2000,
                      chains = 3,
                      seed = 1,
                      cores = 30,
                      wall_time = "0-05:00",
                      project = "coup")
(chm0 <- fetch_results(
  model_name = "chm0",
  project = "coup",
))

# Simple rating models ----
# Contrasts for block
rating_clps[, block := factor(block)]
contrasts(rating_clps$block) <- c(-0.5, 0.5)


jobid <- launch_model(data = rating_clps,
           formula = 'bf(mvbind(confidence, affect, useful) ~ 1 + block + 
             (1 + block | p | PID) +
             (1 | q | questionId)) + cumulative() + set_rescor(F)',
           prior = 'prior(normal(0,1), class = "b", resp = "affect") +
             prior(normal(0,1), class = "b", resp = "confidence") +
             prior(normal(0,1), class = "b", resp = "useful") +
             prior(normal(0,1), class = "Intercept", resp = "affect") +
             prior(normal(0,1), class = "Intercept", resp = "confidence") +
             prior(normal(0,1), class = "Intercept", resp = "useful") +
             prior(normal(0,1), class = "sd", resp = "affect") +
             prior(normal(0,1), class = "sd", resp = "confidence") +
             prior(normal(0,1), class = "sd", resp = "useful") + 
             prior(lkj(2), class = "cor")',
           model_name = "rm0",
           save_output = T,
           iter = 2000,
           chains = 3,
           seed = 1,
           cores = 30,
           wall_time = "0-05:00",
           project = "coup",
           criteria = "loo")


(rm0 <- fetch_results(
  model_name = "rm0",
  project = "coup",
))


# Extract coefficient per questionId
extract_coef_per_question <- function(m,
                                      data,
                                      re_formula = mvbind(confidence, affect, useful) ~ 1 + (1 |q| questionId),
                                      file){
  
  # Prepare data to predict for
  for_preds <- unique(data[, .(questionId, block)])[order(block, questionId)]
  
  # Generate predictions, summarized
  preds <- fitted(m, 
                  newdata = for_preds,
                  re_formula = re_formula,
                  scale = "linear",
                  summary = T)
  
  # Collapse to single data.table
  preds <- rbindlist(lapply(1:3, function(i) {
    variable <- dimnames(preds)[[3]][i]
    dat <- as.data.table(preds[,,i])
    dat[, rating := variable]
    return(dat)
    }))
  
  
  preds <- as.data.table(cbind(for_preds, preds))

  write.csv(preds, file = file.path(savedModelsDir, paste0(file, ".csv")))
  
  # Plot in order
  fp <- preds[order(rating, Estimate)]
  fp[, idx := 1:.N, by = "rating"]
  p1 <- ggplot(fp, aes(x = Estimate,
                                           y = idx,
                                           color = block)) +
    geom_errorbarh(aes(xmin = `Q2.5`, xmax = `Q97.5`), alpha = 0.5) +
    geom_point() +
    facet_wrap("rating", scales = "free", nrow = 2) +
    labs(x = "Estimate",
         y = "",
         color = "Question type")

  # Plot against raw means
  data_l <- melt(data, id.vars = c("PID", "sess", "questionId", "block"),
                 variable.name = "rating")
  rating_sum <- data_l[, .(raw_m = mean(value, na.rm = T),
                         raw_se = sd(value) / sqrt(length(!is.na(value)))), 
                       by = c("questionId", "rating")]

  cur_m0_coef_sum <- merge(preds, rating_sum, by = c("questionId", "rating"))

  p2 <- ggplot(cur_m0_coef_sum, aes(x = Estimate,
                                    y = raw_m,
                                    color = block)) +
    geom_errorbarh(aes(xmin = `Q2.5`, xmax = `Q97.5`), alpha = 0.5) +
    geom_errorbar(aes(ymin = raw_m - raw_se, ymax = raw_m + raw_se)) +
    geom_point() +
    facet_wrap("rating", scales = "free", nrow = 2) +
    labs(x = "Estimate",
         y = "Raw mean",
         color = "Question type")

  r <- cur_m0_coef_sum[, .(cor(Estimate, raw_m)), 
                       by = "rating"]
  print("Correlations b/w estimates and raw means:")
  print(r)
  
  return(list(preds, p1, p2))
}

list[rating_preds, p1, p2] <- extract_coef_per_question(rm0, rating_clps, file = "rm0_question_preds")

# Fit rating waiting model ----
rating_preds_w <- dcast(rating_preds, questionId + block ~ rating,
                        value.var = c("Estimate", "Est.Error"))
wait_ff <- merge(wait, rating_preds_w, by = c("questionId", "block"))

# Standardize variables
wait_ff[, confidence := scale(Estimate_confidence)]
wait_ff[, affect := scale(Estimate_affect)]
wait_ff[, useful := scale(Estimate_useful)]

jobid <- launch_model(data = wait_ff,
                      formula = 'bf(choice ~ 1 + wait_s + block * confidence + 
                        block * affect + block * useful +
                        block * I(confidence^2) +
                        block * I(affect^2) +
                        block * I(useful^2) +
                      (1 + wait_s + block * confidence + 
                        block * affect + block * useful +
                        block * I(confidence^2) +
                        block * I(affect^2) +
                        block * I(useful^2) | PID) + 
              (1 + wait_s | questionId)) + categorical(refcat = "skip")',
                      prior = 'prior(normal(0,1), class = "b", dpar = "muknow") +
              prior(normal(0,1), class = "b", dpar = "muwait") +
              prior(normal(0,1), class = "Intercept", dpar = "muknow") +
              prior(normal(0,1), class = "Intercept", dpar = "muwait") +
              prior(normal(0,1), class = "sd", dpar = "muknow") +
              prior(normal(0,1), class = "sd", dpar = "muwait") +
              prior(lkj(2), class = "cor")',
                      model_name = "chm1",
                      save_output = T,
                      iter = 3500,
                      warmup = 2500,
                      chains = 4,
                      seed = 1,
                      cores = 32,
                      wall_time = "0-25:00",
                      project = "coup")


# (chm1 <- fetch_results(
#   model_name = "chm1",
#   project = "coup",
# ))

# Compare models for the necessity of quadratic terms ----
# jobid <- launch_model(data = wait_ff,
#                       formula = 'bf(choice ~ 1 + wait_s + block * confidence +
#                         block * affect + block * useful +
#                       (1 + wait_s + block * confidence +
#                         block * affect + block * useful | PID) +
#               (1 + wait_s | questionId)) + categorical(refcat = "skip")',
#                       prior = 'prior(normal(0,1), class = "b", dpar = "muknow") +
#               prior(normal(0,1), class = "b", dpar = "muwait") +
#               prior(normal(0,1), class = "Intercept", dpar = "muknow") +
#               prior(normal(0,1), class = "Intercept", dpar = "muwait") +
#               prior(normal(0,1), class = "sd", dpar = "muknow") +
#               prior(normal(0,1), class = "sd", dpar = "muwait") +
#               prior(lkj(2), class = "cor")',
#                       model_name = "chm1000",
#                       save_output = T,
#                       iter = 3500,
#                       warmup = 2500,
#                       chains = 4,
#                       seed = 1,
#                       cores = 16,
#                       criteria = "loo",
#                       wall_time = "0-10:00",
#                       memory = "11gb",
#                       project = "coup")

# 
# (chm1000 <- fetch_results(
#   model_name = "chm1000",
#   project = "coup",
# ))


# Add naive ID measures ----
source("compute_ID.R")
list[naive_ID, alphas] <- computeNaive(quest)

add_ID <- function(dt, naive_ID) {
  
  dt_ff <- merge(dt, naive_ID, by = "PID")
  
  dt_ff[, affect_s := scale(naive_affect)]
  dt_ff[, motivation_s := scale(naive_motivation)]
  dt_ff[, coup_relevance_s := scale(naive_coup_relevance)]
  dt_ff[, coup_attitude_s := scale(naive_coup_attitude)]
  
  
  return(dt_ff)
}

wait_ID_ff <- add_ID(wait_ff, naive_ID)

jobid_chmq1 <- launch_model(data = wait_ID_ff,
                      formula = 'bf(choice ~ 1 + wait_s + 
                      block * coup_relevance_s + block * coup_attitude_s +
                      block * affect_s + block * motivation_s +
                      block * I(coup_attitude_s^2) +
                      block * confidence + 
                      block * affect + 
                      block * useful +
                      block * I(confidence^2) +
                      block * I(affect^2) +
                      (1 + wait_s +
                      block * confidence + 
                      block * affect + 
                      block * useful +
                      block * I(confidence^2) +
                      block * I(affect^2) | PID) +
                      (1 + wait_s +
                      block * coup_relevance_s + block * coup_attitude_s +
                      block * affect_s + block * motivation_s +
                      block * I(coup_attitude_s^2) | questionId))
                       + categorical(refcat = "skip")',
                      prior = 'prior(normal(0,1), class = "b", dpar = "muknow") +
                        prior(normal(0,1), class = "b", dpar = "muwait") +
                        prior(normal(0,1), class = "Intercept", dpar = "muknow") +
                        prior(normal(0,1), class = "Intercept", dpar = "muwait") +
                        prior(normal(0,1), class = "sd", dpar = "muknow") +
                        prior(normal(0,1), class = "sd", dpar = "muwait") +
                        prior(lkj(2), class = "cor")',
                      model_name = "chmq1",
                      save_output = T,
                      iter = 3500,
                      warmup = 2500,
                      chains = 4,
                      seed = 1,
                      cores = 16,
                      wall_time = "0-10:00",
                      project = "coup",
                      criteria = "loo",
                      memory = "11gb")



(chmq1 <- fetch_results(
  model_name = "chmq1",
  project = "coup",
))


# ID measures predict ratings ----
rating_clps_ID_ff <- add_ID(rating_clps, naive_ID)


jobid <- launch_model(data = rating_clps_ID_ff,
                      formula = 'bf(mvbind(confidence, affect, useful) ~ 1 + 
                      block * coup_relevance_s + block * coup_attitude_s +
                      block * affect_s + block * motivation_s +
                      block * I(coup_attitude_s^2) +
                      (1 + block | p | PID) +
                      (1 + coup_relevance_s + coup_attitude_s + affect_s + 
                      motivation_s + I(coup_attitude_s^2) | q | questionId)) + 
                      cumulative() + set_rescor(F)',
                      prior = 'prior(normal(0,1), class = "b", resp = "affect") +
             prior(normal(0,1), class = "b", resp = "confidence") +
             prior(normal(0,1), class = "b", resp = "useful") +
             prior(normal(0,1), class = "Intercept", resp = "affect") +
             prior(normal(0,1), class = "Intercept", resp = "confidence") +
             prior(normal(0,1), class = "Intercept", resp = "useful") +
             prior(normal(0,1), class = "sd", resp = "affect") +
             prior(normal(0,1), class = "sd", resp = "confidence") +
             prior(normal(0,1), class = "sd", resp = "useful") + 
             prior(lkj(2), class = "cor")',
                      model_name = "rmm1",
                      save_output = T,
                      iter = 3500,
                      warmup = 2500,
                      chains = 4,
                      seed = 1,
                      cores = 32,
                      wall_time = "0-25:00",
                      project = "coup")


(rmm1 <- fetch_results(
  model_name = "rmm1",
  project = "coup",
))