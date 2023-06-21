# This script fits the questionnaire data with a BPCA model

# Load libraries
library(data.table)
library(ggplot2)
library(pcaMethods)
library(GPArotation)

# Load data
sampleName <- "v1.01"
preprocDatDir <- file.path("..", "data", sampleName, "preproc")

wait <- fread(file.path(preprocDatDir, "wait_data.csv"))
quest <- fread(file.path(preprocDatDir, "quest_data.csv"), drop = c(1))
quality <- fread(file.path(preprocDatDir, "quality_data.csv"), drop = c(1))

# Exclude participants
source("exclude.R")
excluded_sess1 <- exclude_sess_1(quality, quest, wait)

wait <- wait[!(PID %in% excluded_sess1)]
quest <- quest[!(PID %in% excluded_sess1)]
quality <- quality[!(PID %in% excluded_sess1)]

# Select questionnaire data
qaire <- quest[, grepl("stai|reg_Q|gallup|apathy|coup|PID", 
                       names(quest)), with = F]
