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
contrasts(wait$block) <- c(-0.5, 0.5)
(chm0 <- brm(data = wait,
            choice ~ 1 + wait_duration + block + (1 + wait_duration + block | PID) + 
              (1 + wait_duration| questionId),
            family = categorical(refcat = "skip"),
            prior = prior(normal(0,1), class = "b", dpar = "muknow") +
              prior(normal(0,1), class = "b", dpar = "muwait") +
              prior(normal(0,1), class = "Intercept", dpar = "muknow") +
              prior(normal(0,1), class = "Intercept", dpar = "muwait") +
              prior(normal(0,1), class = "sd", dpar = "muknow") +
              prior(normal(0,1), class = "sd", dpar = "muwait") +
              prior(lkj(2), class = "cor"),
            backend = 'cmdstanr',
            chains = 4,
            cores = 4,
            threads = threading(2),
            file = file.path(savedModelsDir, "chm0"),
            seed = 1,
            ))

# Simple rating models ----
jobid <- launch_model(data = rating_clps,
           formula = 'bf(mvbind(confidence, affect, useful) ~ 1 + block + (1 + block | p | PID) +
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

# No block effect - this was better in previous experiments.
jobid <- launch_model(data = rating_clps,
                      formula = 'bf(mvbind(confidence, affect, useful) ~ 1 + (1 | p | PID) +
             (1 | q | questionId)) + cumulative() + set_rescor(F)',
                      prior = 'prior(normal(0,1), class = "Intercept", resp = "affect") +
             prior(normal(0,1), class = "Intercept", resp = "confidence") +
             prior(normal(0,1), class = "Intercept", resp = "useful") +
             prior(normal(0,1), class = "sd", resp = "affect") +
             prior(normal(0,1), class = "sd", resp = "confidence") +
             prior(normal(0,1), class = "sd", resp = "useful") + 
             prior(lkj(2), class = "cor")',
                      model_name = "rm00",
                      save_output = T,
                      iter = 2000,
                      chains = 3,
                      seed = 1,
                      cores = 30,
                      wall_time = "0-05:00",
                      project = "coup",
                      criteria = "loo")

(rm00 <- fetch_results(
  model_name = "rm00",
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
extract_coef_per_question(rm00, rating_clps, file = "rm00_question_preds")

# Compare models
loo_compare(rm0, rm00)

# Fit rating waiting model ----
rating_preds_w <- dcast(rating_preds, questionId + block ~ rating,
                        value.var = c("Estimate", "Est.Error"))
wait_ff <- merge(wait, rating_preds_w, by = c("questionId", "block"))
wait_ff[, block := factor(block, levels = c("general", "coup"))]
contrasts(wait_ff$block) <- c(-0.5, 0.5)

jobid <- launch_model(data = wait_ff,
                      formula = 'bf(choice ~ 1 + wait_duration + block + 
                      me(Estimate_useful, sdx = Est.Error_useful, gr = questionId) +
                      me(Estimate_affect, sdx = Est.Error_affect, gr = questionId) +
                      me(Estimate_confidence, sdx = Est.Error_confidence, gr = questionId) +
                      (1 + wait_duration + block + 
                      me(Estimate_useful, sdx = Est.Error_useful, gr = questionId) +
                      me(Estimate_affect, sdx = Est.Error_affect, gr = questionId) +
                      me(Estimate_confidence, 
                      sdx = Est.Error_confidence, gr = questionId) | PID) + 
              (1 + wait_duration| questionId)) + categorical(refcat = "skip") +
                      set_mecor(F)',
                      prior = 'prior(lkj(2), class = "cor") +
                      prior(normal(0,1), class = "meanme") +
                      prior(normal(0,1), class = "sdme") +
                      prior(normal(0,1), class = "b", dpar = "muknow") +
                      prior(normal(0,1), class = "b", dpar = "muwait") +
                      prior(normal(0,1), class = "Intercept", dpar = "muknow") +
                      prior(normal(0,1), class = "Intercept", dpar = "muwait") +
                      prior(normal(0,1), class = "sd", dpar = "muknow") +
                      prior(normal(0,1), class = "sd", dpar = "muwait")',
                      model_name = "rw_me0",
                      save_output = T,
                      iter = 3500,
                      chains = 3,
                      seed = 1,
                      cores = 30,
                      wall_time = "0-17:00",
                      project = "coup",
                      criteria = "loo",
                      refit = T)


(rw_me0 <- fetch_results(
  model_name = "rw_me0",
  project = "coup",
))

conditional_effects(rw_me0, categorical = T)

# Add polynomial on confidence
jobid <- launch_model(data = wait_ff,
                      formula = 'bf(choice ~ 1 + wait_duration + block + 
                      me(Estimate_useful, sdx = Est.Error_useful, gr = questionId) +
                      me(Estimate_affect, sdx = Est.Error_affect, gr = questionId) +
                      me(Estimate_confidence, sdx = Est.Error_confidence, gr = questionId) +
                      I(me(Estimate_confidence, sdx = Est.Error_confidence, gr = questionId)^2) +
                      (1 + wait_duration + block + 
                      me(Estimate_useful, sdx = Est.Error_useful, gr = questionId) +
                      me(Estimate_affect, sdx = Est.Error_affect, gr = questionId) +
                      me(Estimate_confidence, sdx = Est.Error_confidence, 
                        gr = questionId) +
                      I(me(Estimate_confidence, sdx = Est.Error_confidence, 
                        gr = questionId)^2)| PID) + 
              (1 + wait_duration| questionId)) + categorical(refcat = "skip") +
                      set_mecor(F)',
                      prior = 'prior(lkj(2), class = "cor") +
                      prior(normal(0,1), class = "meanme") +
                      prior(normal(0,1), class = "sdme") +
                      prior(normal(0,1), class = "b", dpar = "muknow") +
                      prior(normal(0,1), class = "b", dpar = "muwait") +
                      prior(normal(0,1), class = "Intercept", dpar = "muknow") +
                      prior(normal(0,1), class = "Intercept", dpar = "muwait") +
                      prior(normal(0,1), class = "sd", dpar = "muknow") +
                      prior(normal(0,1), class = "sd", dpar = "muwait")',
                      model_name = "rw_me1",
                      save_output = T,
                      iter = 3500,
                      chains = 3,
                      seed = 1,
                      cores = 30,
                      wall_time = "0-17:00",
                      project = "coup",
                      criteria = "loo")


(rw_me1 <- fetch_results(
  model_name = "rw_me1",
  project = "coup",
))

# Add block interactions
rw_me2_jobid <- launch_model(data = wait_ff,
                      formula = 'bf(choice ~ 1 + wait_duration +
                      block * me(Estimate_useful, sdx = Est.Error_useful, gr = questionId) +
                      block * me(Estimate_affect, sdx = Est.Error_affect, gr = questionId) +
                      block * me(Estimate_confidence, sdx = Est.Error_confidence, gr = questionId) +
                      block * I(me(Estimate_confidence, sdx = Est.Error_confidence, gr = questionId)^2) +
                      (1 + wait_duration +
                      block * me(Estimate_useful, sdx = Est.Error_useful, gr = questionId) +
                      block * me(Estimate_affect, sdx = Est.Error_affect, gr = questionId) +
                      block * me(Estimate_confidence, sdx = Est.Error_confidence, 
                        gr = questionId) +
                      block * I(me(Estimate_confidence, sdx = Est.Error_confidence, 
                        gr = questionId)^2)| PID) + 
              (1 + wait_duration| questionId)) + categorical(refcat = "skip") +
                      set_mecor(F)',
                      prior = 'prior(lkj(2), class = "cor") +
                      prior(normal(0,1), class = "meanme") +
                      prior(normal(0,1), class = "sdme") +
                      prior(normal(0,1), class = "b", dpar = "muknow") +
                      prior(normal(0,1), class = "b", dpar = "muwait") +
                      prior(normal(0,1), class = "Intercept", dpar = "muknow") +
                      prior(normal(0,1), class = "Intercept", dpar = "muwait") +
                      prior(normal(0,1), class = "sd", dpar = "muknow") +
                      prior(normal(0,1), class = "sd", dpar = "muwait")',
                      model_name = "rw_me2",
                      save_output = T,
                      iter = 3500,
                      chains = 3,
                      seed = 1,
                      cores = 30,
                      wall_time = "0-17:00",
                      project = "coup",
                      criteria = "loo")


(rw_me2 <- fetch_results(
  model_name = "rw_me2",
  project = "coup",
))

loo_compare(rw_me2, rw_me1)

# Add naive ID measures ----
source("compute_ID.R")
list[naive_ID, alphas] <- computeNaive(quest)

wait_ID_ff <- merge(wait_ff, naive_ID, by = "PID", all.x = T)

jobid_rwid_me0 <- launch_model(data = wait_ID_ff,
                      formula = 'bf(choice ~ 1 + wait_duration + 
                      block * naive_coup_relevance + block * naive_coup_attitude +
                      block * naive_affect + block * naive_motivation +
                      block * me(Estimate_useful, sdx = Est.Error_useful, gr = questionId) +
                      block * me(Estimate_affect, sdx = Est.Error_affect, gr = questionId) +
                      block * me(Estimate_confidence, sdx = Est.Error_confidence, gr = questionId) +
                      block * I(me(Estimate_confidence, sdx = Est.Error_confidence, gr = questionId)^2) +
                      (1 + wait_duration +
                      block * me(Estimate_useful, sdx = Est.Error_useful, gr = questionId) +
                      block * me(Estimate_affect, sdx = Est.Error_affect, gr = questionId) +
                      block * me(Estimate_confidence, sdx = Est.Error_confidence, 
                        gr = questionId) +
                      block * I(me(Estimate_confidence, sdx = Est.Error_confidence, 
                        gr = questionId)^2)| PID) + 
              (1 + wait_duration +
              naive_coup_relevance + naive_coup_attitude +
                      naive_affect + naive_motivation | questionId)) + categorical(refcat = "skip") +
                      set_mecor(F)',
                      prior = 'prior(lkj(2), class = "cor") +
                      prior(normal(0,1), class = "meanme") +
                      prior(normal(0,1), class = "sdme") +
                      prior(normal(0,1), class = "b", dpar = "muknow") +
                      prior(normal(0,1), class = "b", dpar = "muwait") +
                      prior(normal(0,1), class = "Intercept", dpar = "muknow") +
                      prior(normal(0,1), class = "Intercept", dpar = "muwait") +
                      prior(normal(0,1), class = "sd", dpar = "muknow") +
                      prior(normal(0,1), class = "sd", dpar = "muwait")',
                      model_name = "rwid_me0",
                      save_output = T,
                      iter = 3500,
                      chains = 3,
                      seed = 1,
                      cores = 30,
                      wall_time = "0-20:00",
                      project = "coup",
                      criteria = "loo")


(rwid_me0 <- fetch_results(
  model_name = "rwid_me0",
  project = "coup",
))

# Add polynomial for coup attitude
jobid_rwid_me1 <- launch_model(data = wait_ID_ff,
                               formula = 'bf(choice ~ 1 + wait_duration + 
                      block * naive_coup_relevance + block * naive_coup_attitude +
                      block * I(naive_coup_attitude^2) + 
                      block * naive_affect + block * naive_motivation +
                      block * me(Estimate_useful, sdx = Est.Error_useful, gr = questionId) +
                      block * me(Estimate_affect, sdx = Est.Error_affect, gr = questionId) +
                      block * me(Estimate_confidence, sdx = Est.Error_confidence, gr = questionId) +
                      block * I(me(Estimate_confidence, sdx = Est.Error_confidence, gr = questionId)^2) +
                      (1 + wait_duration +
                      block * me(Estimate_useful, sdx = Est.Error_useful, gr = questionId) +
                      block * me(Estimate_affect, sdx = Est.Error_affect, gr = questionId) +
                      block * me(Estimate_confidence, sdx = Est.Error_confidence, 
                        gr = questionId) +
                      block * I(me(Estimate_confidence, sdx = Est.Error_confidence, 
                        gr = questionId)^2)| PID) + 
              (1 + wait_duration +
              naive_coup_relevance + naive_coup_attitude + I(naive_coup_attitude^2) +
                      naive_affect + naive_motivation | questionId)) + categorical(refcat = "skip") +
                      set_mecor(F)',
                               prior = 'prior(lkj(2), class = "cor") +
                      prior(normal(0,1), class = "meanme") +
                      prior(normal(0,1), class = "sdme") +
                      prior(normal(0,1), class = "b", dpar = "muknow") +
                      prior(normal(0,1), class = "b", dpar = "muwait") +
                      prior(normal(0,1), class = "Intercept", dpar = "muknow") +
                      prior(normal(0,1), class = "Intercept", dpar = "muwait") +
                      prior(normal(0,1), class = "sd", dpar = "muknow") +
                      prior(normal(0,1), class = "sd", dpar = "muwait")',
                               model_name = "rwid_me1",
                               save_output = T,
                               iter = 3500,
                               chains = 3,
                               seed = 1,
                               cores = 30,
                               wall_time = "0-20:00",
                               project = "coup",
                               criteria = "loo")


(rwid_me1 <- fetch_results(
  model_name = "rwid_me1",
  project = "coup",
))
