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

# Simple rating model ----
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
           iter = 20,
           chains = 3,
           seed = 1,
           cores = 30,
           wall_time = "0-00:10",
           project = "coup",
           criteria = "loo")

jobid <- launch_model(
  formula = cur_wait_bform0,
  data = wait_ffit,
  prior = "prior(normal(0,1), class = 'b', resp = 'waited') +
        prior(normal(0,1), class = 'Intercept', resp = 'waited') + 
        prior(normal(0,1), class = 'Intercept', resp = 'curiosity') +
        prior(normal(0,1), class = 'sd', resp = 'waited') +
        prior(lkj(2), class = 'cor') +
        prior(normal(0,1), class = 'sigma', resp = 'curiosity')",
  model_name = "cur_wait0",
  save_output = TRUE,
  iter = 2800,
  chains = 3,
  seed = 1,
  cores = 30,
  control = "list(adapt_delta = 0.85)",
  wall_time = "0-00:30",
  project = "energize",
  criteria = "loo")

(cur_wait_m0 <- fetch_results(
  model_name = "cur_wait0",
  project = "energize",
))

