## This script preprocesses data, and returns stats for participant approval
library(data.table)
setDTthreads(11)
library(jsonlite)

sampleName <- "v1.01"
rawDatDir <- file.path("..", "data", sampleName, "raw")
preprocDatDir <- file.path("..", "data", sampleName, "preproc")

# Load data ----
# List relevant files
files <- list.files(rawDatDir, pattern = "sess2")
mfiles <- files[grepl(".csv", files, fixed =T) & !grepl("int", files)]
intfiles <- files[grepl(".csv", files, fixed =T) & grepl("int", files)]

# Check for double takers
getPID <- function(s) substring(strsplit(s,"_")[[1]][1],2)
getDate <- function(s) substring(strsplit(s,"_")[[1]][3],1,10)

doubles <- data.table(PID = sapply(mfiles, getPID),
                      date = sapply(mfiles, getDate))
doubles[, n := .N, by = PID]
doubles <- doubles[n > 1][order(date)]

# Remove second take from data
if (nrow(doubles) > 0)  {
  doubles[, i := 1:.N, by = "PID"]
  doubles_to_remove <- doubles[i > 1]
  doubles_to_remove[, mfile := paste0("S", PID, "_sess2_", date, ".csv")]
  doubles_to_remove[, intfile := paste0("S", PID, "_sess2_", date, "_int.csv")]
  mfiles <- mfiles[!(mfiles %in% doubles_to_remove$mfile)]
  intfiles <- intfiles[!(intfiles %in% doubles_to_remove$intfile)]
  
  # Save double takers list
  doubles <- dcast(doubles, PID ~ i, value.var = "date")
  write.csv(doubles, file = file.path(preprocDatDir, "double_takers_sess2.csv"))
}


# Open each file, and then rbind them together
data <- rbindlist(lapply(mfiles, function(f) fread(file.path(rawDatDir, f))),
                  fill = T)

# PID to string
data[, PID := as.character(PID)]

int_data <- do.call(rbind, lapply(intfiles, function(f) {
  dat <- fread(file.path(rawDatDir, f))
  dat$PID <- getPID(f)
  return(dat)}))

# Remove kickouts ----
kickouts <- data[, .(kickout = sum(category == "kick-out")), by = PID][kickout>0]
write.csv(kickouts, file = file.path(preprocDatDir, "kickouts_sess2.csv"))
data <- data[!(PID %in% kickouts$PID)]
int_data <- int_data[!(PID %in% kickouts$PID)]

# Quality measures ----
# Add trial category to interaction data to see if any occurred in important bits
int_data <- merge(int_data, 
                  data[, .(PID, trial_index, category)],
                  by.x = c("PID","trial"),
                  by.y = c("PID", "trial_index"),
                  all.x = TRUE)

recall_cats <- c("answer_recall", "answer_recall_yn")

int_data[, important := category %in% recall_cats]

# Create session quality table
quality <- int_data[, .(interactions = .N,
                        important_interactions = sum(important)), by = PID]

# Write interaction data to file
write.csv(int_data, file.path(preprocDatDir, "browser_interactions_sess2.csv"))

# Add interactions to main datatable
int_sum <- int_data[, .(events = paste(event, collapse=",")), by = c("PID", "trial")]
data <- merge(data, int_sum[, .(PID, trial, events)],
              by.x = c("PID", "trial_index"),
              by.y = c("PID", "trial"), 
              all.x = TRUE)

# Experiment duaration
duration <- data[, .(duartion = (max(time_elapsed) - min(time_elapsed))/60000), 
                 by = PID]
quality <- merge(quality, duration, all.x = TRUE)

# Number of warnings given
warnings <- data[, .(warnings = max(n_warnings)), by=PID]
quality <- merge(quality, warnings, all.x=TRUE)

## Preprocess recall data ----
# Parse text responses
recall_text <- data[category == "answer_recall"]

recall_text[, recall := fromJSON(gsub('""', '"', responses))$recall,
       by = .(PID, trial_index)]

# Gather forced choice responses
recall_yn <- data[category == "answer_recall_yn"]
recall_yn[button_pressed == "0", choice := "yes"]
recall_yn[button_pressed == "1", choice := "no"]

recall <- merge(recall_yn[, .(PID, sess, firstBlock, version, block, type, 
                              trial_index, questionId, question, 
                              answer, choice)], 
                recall_text[, .(PID, questionId, recall)],
                by = c("PID", "questionId"), all.x = T)[order(PID, trial_index)]
assert("NAs in recall data", sum(is.na(recall[choice == "yes"]$recall)) == 0)

# Remove html from stimuli
recall[, question := gsub("<div class='question'>|</div>", "", question)]
recall[, answer := gsub("<div class='answer'>|</div>", "", answer)]

# Score easy scores ----

# Mark numeric answers
suppressWarnings(recall[!is.na(recall), is_number := !is.na(as.numeric(answer)), 
       by = .(PID, trial_index)])

# Calculate string distance
recall[!is.na(recall), distance := adist(answer, recall, fixed = F, 
                                         ignore.case = T),
       by = .(PID, trial_index)]

# If numeric and exact match, or non numeric and dist < 1, mark as true
recall[(!is.na(recall)) & ((is_number & (distance == 0)) |
                           (!is_number & (distance < 2))), correct := T]

save(recall, file = file.path(preprocDatDir, "recall_data.rda"))
