## This script prepares stimuli for second session knowledge block  
# Load libraries
library(data.table)

sampleName <- "v1.01"

# Save to
output_fld <- file.path("..", "data", sampleName, "preproc", "second_sess_known")
dir.create(output_fld, showWarnings = F)
do.call(file.remove, list(list.files(output_fld, full.names = TRUE)))

# Load data
source("load_data_and_exclude.R")
list[wait, rating_clps, know_test, 
     prob_judge, quest, quality] = load_exclude(sampleName)

# Select known trials
known <- wait[choice == "know", .(PID, firstBlock, block, type, questionId,
                                  question, choice)]

known[, write.csv(.SD, file = file.path(output_fld, paste0("S", unique(PID), 
                                                           "_known_stims.csv"))),
      by = PID]
