## This script preprocesses data, and returns stats for participant approval
library(data.table)
setDTthreads(11)
library(jsonlite)

sampleName <- "debug"
rawDatDir <- file.path("..", "data", sampleName, "raw")
preprocDatDir <- file.path("..", "data", sampleName, "preproc")

# Load data ----
# List relevant files
files <- list.files(rawDatDir, pattern = "sess1")
mfiles <- files[grepl(".csv", files, fixed =T) & !grepl("int", files)]
intfiles <- files[grepl(".csv", files, fixed =T) & grepl("int", files)]

# Open each file, and then rbind them together
data <- do.call(rbind, lapply(mfiles, function(f) fread(file.path(rawDatDir, f))))

getPID <- function(s) substring(strsplit(s,"_")[[1]][1],2)

int_data <- do.call(rbind, lapply(intfiles, function(f) {
  dat <- fread(file.path(rawDatDir, f))
  dat$PID <- getPID(f)
  return(dat)}))

# Remove kickouts ----
kickouts <- data[, .(kickout = sum(category == "kick-out")), by = PID][kickout>0]
write.csv(kickouts, file = file.path(preprocDatDir, "kickouts.csv"))
data <- data[!(PID %in% kickouts$PID)]
int_data <- int_data[!(PID %in% kickouts$PID)]

# Anonimize ----
# Assign random strings instead of worker IDs
PIDs <- unique(data[, .(PID = PID, wait_start_time)])[order(wait_start_time)]
randString <- function(x) paste(sample(c(0:9, letters),
                                      5, replace=TRUE),
                               collapse="")
set.seed(0)
PIDs[, AID := randString(PID), by = PID]

renamePIDs <- function(dat) dat[.(PID = PIDs$PID, to = PIDs$AID), 
                                 on = "PID", 
                                 PID := i.to]


# Type conversion
data[, rt := as.numeric(rt)]

# Quality measures ----
# Add trial category to interaction data to see if any occurred in important bits
int_data <- merge(int_data, 
                  data[, .(PID, trial_index, category)],
                  by.x = c("PID","trial"),
                  by.y = c("PID", "trial_index"),
                  all.x = TRUE)

important_cats <- c("wait_question", "wait_answer", "wait_satisfaction", 
                    "wait_wait", "wait_fixation", "rating_question1", 
                    "rating_question2", "probability_judgment", "knowledge_test")

int_data[, important := category %in% important_cats]

# Create session quality table
quality <- int_data[, .(interactions = .N,
                        important_interactions = sum(important)), by = PID]

# Write interaction data to file
write.csv(int_data, file.path(preprocDatDir, "browser_interactions.csv"))

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

# Preprocess waiting task ----
wait_cats <- c("wait_question", "wait_answer", "wait_satisfaction")
wait <- data[category %in% wait_cats]

wait_trial_index <- wait[category == "wait_question", 
                         .(PID, questionId, trial_index)]

# Arrange choices
wait_choice <- dcast(wait[button_pressed != "null"], 
                     PID + sess + firstBlock + block + type + questionId + 
                       wait_duration ~ category, value.var = "button_pressed")
setnames(wait_choice, wait_cats[1:2], c("choice", "answer_clicked"))

# Arrange RTs
wait_rt <- dcast(wait, 
                     PID + sess + firstBlock + block + type + questionId + 
                       wait_duration ~ category, value.var = "rt")
setnames(wait_rt, wait_cats, c("choice_rt", 
                                   "answer_rt", 
                                   'satisfaction_rt'))

# Arrange presented stimuli
wait_stim <- dcast(wait[category %in% wait_cats[1:2]], 
                   PID + sess + firstBlock + block + type + questionId + 
                     wait_duration ~ category, value.var = "stimulus")

setnames(wait_stim, wait_cats[1:2], c("question", 
                               "answer"))

wait_stim[, question := gsub("<div class='question'>|</div>", "", question)]
wait_stim[, answer := gsub("<div class='answer'>|</div>", "", answer)]

# Arrange interaction data
wait_int_cats <- c("wait_question", 
                   "wait_answer", 
                   "wait_satisfaction", 
                   "wait_wait")

wait_int <- dcast(data[category %in% wait_int_cats], 
                   PID + sess + firstBlock + block + type + questionId + 
                     wait_duration ~ category, value.var = "events")

setnames(wait_int, wait_int_cats, c("question_ints", 
                               "answer_ints", 
                               'satisfaction_ints',
                               "wait_ints"))

# Merge all
wait <- merge(wait_choice, wait_rt, 
                 by = c("PID",
                        "sess",
                        "firstBlock", 
                        "block",
                        "type", 
                        "questionId", 
                        "wait_duration"),
                 all.x = T)

wait <- merge(wait, wait_stim,
              by = c("PID", 
                     "sess",
                     "firstBlock", 
                     "block",
                     "type", 
                     "questionId", 
                     "wait_duration"),
              all.x = T)

wait <- merge(wait, wait_int,
              by = c("PID", 
                     "sess",
                     "firstBlock", 
                     "block",
                     "type", 
                     "questionId", 
                     "wait_duration"),
              all.x = T)

wait <- merge(wait, wait_trial_index, 
              by = c("PID", "questionId"), all.x = T)

# Recode variables
wait[.(answer_clicked = c(NA, "null", "0", 0), to = c(NA, 0, 1, 1)), 
     on = "answer_clicked", 
     answer_clicked := i.to]
wait[, answer_clicked := factor(answer_clicked)]

wait[.(choice = c(0, "0", "1", 1, "2", 2, NA, "null"), 
       to = c("skip", "skip", "wait", "wait", "know", "know", "no_resp", "no_resp")), 
     on = "choice", 
     choice := i.to]
wait[, choice := factor(choice)]

wait[, wait_satisfaction := as.numeric(wait_satisfaction)]

# Add choice quality to quality
mrt <- wait[, .(m_choice_rt = mean(choice_rt, na.rm = T),
                m_answer_rt = mean(answer_rt, na.rm = T)), by = PID]
quality <- merge(quality, mrt, by = "PID", all.x=T)

# Save waiting task data
write.csv(renamePIDs(wait), file = file.path(preprocDatDir, "wait_data.csv"))

# Preprocess rating task ----
rating_cats <- c("rating_question1", "rating_question2")
rating <- data[category %in% rating_cats]
rating <- rating[, .(rating = as.numeric(fromJSON(gsub('""', '"', responses))),
           probe = names(fromJSON(gsub('""', '"', responses)))), 
       by = .(PID, sess, firstBlock, questionId, trial_index)]
rating <- dcast(rating, PID + sess + firstBlock + questionId ~ probe, value.var = "rating")

# Save rating task data
write.csv(renamePIDs(rating), file = file.path(preprocDatDir, "rating_data.csv"))

# Preprocess questionnaire data ----
quest_cats <- c("anxiety", "apathy",
                "demographics", "difficulties",
                "impulsive", "regulatory_focus", 
                "pleasure")
quest <- data[category %in% quest_cats]
quest <- quest[, .(response = fromJSON(gsub('""', '"', responses)),
                     probe = names(fromJSON(gsub('""', '"', responses)))), 
                 by = .(PID, trial_index)]
quest <- dcast(quest, PID ~ probe, value.var = "response")

# Transform questionnaire ratings to numeric
quest <- as.data.table(apply(quest, 2, function(x) unlist(x, use.names = T)))
numeric_cats <- c("anxiety|apathy|age|impulsive|reg_focus|pleasure|fluent")
ns <- colnames(quest)
quest <- cbind(quest[, ns[!grepl(numeric_cats, ns)], with = F],
      quest[, ns[grepl(numeric_cats, ns)], with = F][, lapply(.SD, as.numeric)])

# Add quest data to quality
quality <- merge(quality, quest[, .(PID, native_english, fluent, 
                                    difficult, instructions, strategy)],
                 by = "PID", all.x = T)
t_quest_cats <- "anxiety|apathy|impulsive|reg_focus|pleasure"
n_quest_na <- data.table(PID = quest$PID, n_miss_quest = rowSums(quest[, ns[grepl(t_quest_cats, ns)], 
                                                                       with = F][, lapply(.SD, is.na)]))
quality <- merge(quality, n_quest_na, by = "PID", all.x = T)

# Reverse code items ---
# Anxiety - higher is negative affect
quest[, anxiety_0 := 3 - (anxiety_0 )] # Calm
quest[, anxiety_1 := 3 - (anxiety_1)] # Secure
quest[, anxiety_4 := 3 - (anxiety_4)] # At ease
quest[, anxiety_7 := 3 - (anxiety_7)] # Satisfied
quest[, anxiety_9 := 3 - (anxiety_9)] # Comfortable
quest[, anxiety_10 := 3 - (anxiety_10)] # Self confident
quest[, anxiety_14 := 3 - (anxiety_14)] # Relaxed
quest[, anxiety_15 := 3 - (anxiety_15)] # Content
quest[, anxiety_18 := 3 - (anxiety_18)] # Steady
quest[, anxiety_19 := 3 - (anxiety_19)] # Pleasant

# Apathy - higher is apatehtic
quest[, apathy_0 := 3 - (apathy_0)] # interested in learning new things
quest[, apathy_1 := 3 - (apathy_1)] # anything interest you
quest[, apathy_3 := 3 - (apathy_3)] # put much effort into things
quest[, apathy_4 := 3 - (apathy_4)] # always looking for something to do
quest[, apathy_5 := 3 - (apathy_5)] # plans and goals for the future
quest[, apathy_6 := 3 - (apathy_6)] # have motivation
quest[, apathy_7 := 3 - (apathy_7)] # have the energy for daily activities

# Impulsiveness - higher is impulsive
quest[, impulsive_0 := 3 - (impulsive_0)] # plan tasks carefully
quest[, impulsive_6 := 3 - (impulsive_6)] # plan trips well ahead of time
quest[, impulsive_7 := 3 - (impulsive_7)] # self-controlled
quest[, impulsive_8 := 3 - (impulsive_8)] # concentrate easily
quest[, impulsive_9 := 3 - (impulsive_9)] # save regularly
quest[, impulsive_11 := 3 - (impulsive_11)] # am a careful thinker
quest[, impulsive_12 := 3 - (impulsive_12)] # plan for job security
quest[, impulsive_14 := 3 - (impulsive_14)] # like to think
quest[, impulsive_19 := 3 - (impulsive_19)] # am a steady thinker
quest[, impulsive_28 := 3 - (impulsive_28)] # I like puzzles
quest[, impulsive_29 := 3 - (impulsive_29)] # future oriented

# Pleasure - higher is not feeling pleasure
quest[, pleasure_0 := 3 - (pleasure_0)]
quest[, pleasure_1 := 3 - (pleasure_1)]
quest[, pleasure_2 := 3 - (pleasure_2)]
quest[, pleasure_3 := 3 - (pleasure_3)]
quest[, pleasure_4 := 3 - (pleasure_4)]
quest[, pleasure_5 := 3 - (pleasure_5)]
quest[, pleasure_6 := 3 - (pleasure_6)]
quest[, pleasure_7 := 3 - (pleasure_7)]
quest[, pleasure_8 := 3 - (pleasure_8)]
quest[, pleasure_9 := 3 - (pleasure_9)]
quest[, pleasure_10 := 3 - (pleasure_10)]
quest[, pleasure_11 := 3 - (pleasure_11)]
quest[, pleasure_12 := 3 - (pleasure_12)]
quest[, pleasure_13 := 3 - (pleasure_13)]

# Regulatory focus - higher is assessment. Rename Lie items
quest[, reg_focus_0 := 5 - reg_focus_0] # doing things even if they involve extra effort
quest[, reg_focus_1 := 5 - reg_focus_1] # never evaluate my social interactions
quest[, reg_focus_2 := 5 - reg_focus_2] # workaholic
quest[, reg_focus_3 := 5 - reg_focus_3] # excited before goal
quest[, reg_focus_4 := 5 - reg_focus_4] # enjoy actively doing things
quest[, reg_focus_7 := 5 - reg_focus_7] # I am a doer
quest[, reg_focus_9 := 5 - reg_focus_9] # don't spend much time thinking
setnames(quest, 'reg_focus_11', 'lie_1') # one should never engage in leisure activities
setnames(quest, 'reg_focus_13', 'lie_2') # never been late for work
quest[, reg_focus_15 := 5 - reg_focus_15] # I can't wait to get started
setnames(quest, 'reg_focus_16', 'lie_3') # always make the right decision
setnames(quest, 'reg_focus_17', 'lie_4') # never find faults in someone I like
quest[, reg_focus_20 := 5 - reg_focus_20] # already have the next one in mind
setnames(quest, 'reg_focus_22', 'lie_5') # never hurt another person's feelings
quest[, reg_focus_24 := 5 - reg_focus_24] # occupied with the task I wish to accomplish
setnames(quest, 'reg_focus_25', 'lie_6')# no such thing as an honest mistake
quest[, reg_focus_26 := 5 - reg_focus_26] # rarely analyze the conversations
quest[, reg_focus_27 := 5 - reg_focus_27]# I usually persevere
quest[, reg_focus_28 := 5 - reg_focus_28]# go-getter

# Save questionnaire to file
write.csv(renamePIDs(quest), file = file.path(preprocDatDir, "quest_data.csv"))

# Save quality data to file
write.csv(renamePIDs(quality), file = file.path(preprocDatDir, "quality_data.csv"))

