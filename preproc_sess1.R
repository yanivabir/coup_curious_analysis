## This script preprocesses data, and returns stats for participant approval
library(data.table)
setDTthreads(11)
library(jsonlite)
library(rvest)
library(lubridate)
library(testit)

sampleName <- "v1.01"
rawDatDir <- file.path("..", "data", sampleName, "raw")
midgamDatDir <- file.path("..", "data", sampleName, "midgam")
preprocDatDir <- file.path("..", "data", sampleName, "preproc")

# Load data ----
# List relevant files
files <- list.files(rawDatDir, pattern = "sess1")

# Remove empty files
files <- files[sapply(files, function(x) file.size(file.path(rawDatDir, x)) > 10)]

# Split by type
mfiles <- files[grepl(".csv", files, fixed =T) & !grepl("int", files) & !grepl("yaniv", files)]
intfiles <- files[grepl(".csv", files, fixed =T) & grepl("int", files) & !grepl("yaniv", files)]
midgamfiles <- list.files(midgamDatDir, pattern = ".xls")

# Check for double takers
getPID <- function(s) substring(strsplit(s,"_")[[1]][1],2)
getDate <- function(s) substring(strsplit(s,"_")[[1]][3],1,10)

doubles <- data.table(PID = sapply(mfiles, getPID),
                      date = sapply(mfiles, getDate))
doubles[, n := .N, by = PID]
doubles <- doubles[n > 1][order(date)]

# Remove second take from data
doubles[, i := 1:.N, by = "PID"]
doubles_to_remove <- doubles[i > 1]
doubles_to_remove[, mfile := paste0("S", PID, "_sess1_", date, ".csv")]
doubles_to_remove[, intfile := paste0("S", PID, "_sess1_", date, "_int.csv")]
mfiles <- mfiles[!(mfiles %in% doubles_to_remove$mfile)]
intfiles <- intfiles[!(intfiles %in% doubles_to_remove$intfile)]

# Save double takers list
doubles <- dcast(doubles, PID ~ i, value.var = "date")
write.csv(doubles, file = file.path(preprocDatDir, "double_takers.csv"))

# Open each file, and then rbind them together
data <- rbindlist(lapply(mfiles, function(f) {
  dat <- fread(file.path(rawDatDir, f))
  dat$date <- getDate(f)
  return(dat)
  }),
                  fill = T)

# PID to string
data[, PID := as.character(PID)]


int_data <- do.call(rbind, lapply(intfiles, function(f) {
  dat <- fread(file.path(rawDatDir, f))
  dat$PID <- getPID(f)
  return(dat)}))

# Open midgam data
midgam <- rbindlist(lapply(midgamfiles, 
                           function(f) {
                             file.path(midgamDatDir, f) %>%
                               read_html() %>%
                               html_element("table") %>%
                               html_table %>%
                               as.data.table
                           }), use.names=TRUE)

# Remove kickouts ----
kickouts <- data[, .(kickout = sum(category == "kick-out")), 
                 by = .(PID, date)][kickout>0]
write.csv(kickouts, file = file.path(preprocDatDir, "kickouts.csv"))
data <- data[!(PID %in% kickouts$PID)]
int_data <- int_data[!(PID %in% kickouts$PID)]

# Use midgam data to fix dates ----
# Parse midgam dates
midgam[, date := tstrsplit(startTime, " ")[1]]
midgam$date <- mdy(midgam$date)

# Find outlier dates in data
dates <- data[, .(date = unique(date)), by = PID]
dates[, date_N := .N, by = "date"]
problem_dates <- dates[date_N < 5]

# Find correct dates in midgm data
amended_dates <- midgam[userId %in% problem_dates$PID]
amended_dates[, userN := .N, by = userId]
amended_dates <- amended_dates[userN <= 2]
amended_dates <- amended_dates[, .(date = min(date)), by = userId]

# Fix in data
for(i in 1:nrow(amended_dates)){
  warning("Fixing date for participant ", amended_dates$userId[i], 
          ": from ", problem_dates[PID == amended_dates$userId[i]]$date, " to ",
          as.character(amended_dates$date[i]))
  data[PID == amended_dates$userId[i], date := as.character(amended_dates$date[i])]
}

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

# Add timestamp
quality <- merge(quality, 
                 data[, .(date = unique(date)), by = "PID"], 
                 by = "PID",
                 all.x = T)

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
wait <- data[(category %in% wait_cats) & is_practice == F]

wait_trial_index <- wait[category == "wait_question", 
                         .(PID, questionId, trial_index)]

# Arrange choices
wait_choice <- dcast(wait, 
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

mchoice <- wait[, .(n_waited = sum(choice == "wait", na.rm = T),
                    prop_missed = mean(choice == "no_resp" | 
                                     ((choice == "wait") & 
                                        (is.na(answer_clicked) | 
                                           is.na(wait_satisfaction))))), 
                by = PID]
quality <- merge(quality, mchoice, by = "PID", all.x=T)

# Save waiting task data
write.csv(wait, file = file.path(preprocDatDir, "wait_data.csv"))

# Preprocess rating task ----
rating_cats <- c("rating_question1", "rating_question2")
rating <- data[category %in% rating_cats]
rating <- rating[, .(rating = as.numeric(fromJSON(gsub('""', '"', gsub('""', '"', responses)))),
           probe = names(fromJSON(gsub('""', '"', gsub('""', '"', responses))))), 
       by = .(PID, sess, firstBlock, questionId, trial_index)]
rating <- dcast(rating, PID + sess + firstBlock + questionId ~ probe, value.var = "rating")

# Recover question type from question ID
rating[, block := ifelse(substr(questionId, 1,3) == "gen", "general", "coup")]

# Save rating task data
write.csv(rating, file = file.path(preprocDatDir, "rating_data.csv"))

# Preprocess probability judgment ----
prob_judge <- data[category == "probability_judgment"]

prob_judge <- prob_judge[, .(PID, sess, firstBlock, block, itemId, 
                             trial_index, stimulus, response, rt)]

# Add keys to probability judgment
prob_judge_key <- fread(file.path("..", "data", "prob_judge_key.csv"))

prob_judge <- merge(prob_judge, prob_judge_key[, .(itemId, short_desc)],
                    by = "itemId", all.x = T)

prob_judge[, short_desc := factor(short_desc,
                                  levels = prob_judge_key$short_desc)]

# Save probability judgment data
write.csv(prob_judge, file = file.path(preprocDatDir, "prob_judge_data.csv"))

# Preprocess knowledge test ----
know_test <- data[category == "knowledge_test"]
know_test[, correct_answers := gsub("כן,",
                                   "כן-",
                                   correct_answers)]
know_test[, responses := gsub("כן,",
                              "כן-",
                              responses)]

know_test <- know_test[, .(response = fromJSON(gsub('""', '"', gsub('""', '"', responses))),
                   probe = names(fromJSON(gsub('""', '"', gsub('""', '"', responses)))),
                   correct_answer = strsplit(correct_answers, ",")[[1]]), 
               by = .(PID, sess, block, trial_index)]
know_test[, correct := response == correct_answer]

# Type transformation
know_test[, response := unlist(response)]

# Save knowledge test data
write.csv(know_test, file = file.path(preprocDatDir, "knowledge_test_data.csv"))

# Preprocess questionnaire data ----
quest_cats <- c("stai", "gallup", "reg_mode", "apathy",
                "coup_relevance", "iwin",
                "demographics", "difficulties",
                "coup_reg_focus")
quest <- data[category %in% quest_cats]
quest <- quest[, .(response = fromJSON(gsub('""', '"', responses)),
                     probe = names(fromJSON(gsub('""', '"', responses)))), 
                 by = .(PID, sess, category, trial_index)]

# Type transformation
quest[, response := unlist(response)]

# Recode
quest[(category == "gallup") & (response == "כן"), response := "1"]
quest[(category == "gallup") & (response == "לא"), response := "0"]

quest <- dcast(quest, PID + sess ~ probe, value.var = "response")

# Transform questionnaire ratings to numeric
numeric_items <- c("stai|apathy|age|coup_rel|reg_Q|coup_reg|gallup|fluent|secular_religious|left_right|iwin|socialism_capitalism")
ns <- colnames(quest)
quest <- cbind(quest[, ns[!grepl(numeric_items, ns)], with = F],
      quest[, ns[grepl(numeric_items, ns)], with = F][, lapply(.SD, as.numeric)])

# Add quest data to quality
quality <- merge(quality, quest[, .(PID, native_english, fluent, 
                                    difficult, instructions, strategy)],
                 by = "PID", all.x = T)
t_quest_cats <- "stai|apathy|reg_Q|coup_rel|gallup|secular_religious|left_right|iwin|socialism_capitalism"
n_quest_na <- data.table(PID = quest$PID, n_miss_quest = rowSums(quest[, ns[grepl(t_quest_cats, ns)], 
                                                                       with = F][, lapply(.SD, is.na)]))
quality <- merge(quality, n_quest_na, by = "PID", all.x = T)

# Reverse code items DOUBLE CHECK ---
# Anxiety - higher is negative affect
quest[, stai_0 := 3 - (stai_0 )] # Calm
quest[, stai_1 := 3 - (stai_1)] # Secure
quest[, stai_4 := 3 - (stai_4)] # At ease
quest[, stai_7 := 3 - (stai_7)] # Satisfied
quest[, stai_9 := 3 - (stai_9)] # Comfortable
quest[, stai_10 := 3 - (stai_10)] # Self confident
quest[, stai_14 := 3 - (stai_14)] # Relaxed
quest[, stai_15 := 3 - (stai_15)] # Content
quest[, stai_18 := 3 - (stai_18)] # Steady
quest[, stai_19 := 3 - (stai_19)] # Pleasant

# Gallup - higher is negative affect
quest[, gallup01 := 10 - (gallup01)] # Satisfied with life
quest[, gallup04 := 1 - (gallup04)] # A lot of joy
quest[, gallup05 := 1 - (gallup05)] # A lot of happiness
quest[, gallup09 := 1 - (gallup09)] # Something interesting
quest[, gallup10 := 1 - (gallup10)] # Smiled or laughed
quest[, gallup12 := 1 - (gallup12)] # More like yesterday
quest[, gallup13 := 1 - (gallup13)] # Current best life
quest[, gallup14 := 1 - (gallup14)] # Past best life
quest[, gallup15 := 1 - (gallup15)] # Future best life


# Apathy - higher is motivated
quest[, apathy_2 := 3 - (apathy_2)] # worried
quest[, apathy_8 := 3 - (apathy_8)] # tell you what to do
quest[, apathy_9 := 3 - (apathy_9)] # indifferent
quest[, apathy_10 := 3 - (apathy_10)] # unconcerned
quest[, apathy_11 := 3 - (apathy_11)] # push
quest[, apathy_12 := 3 - (apathy_12)] # neither happy nor sad
quest[, apathy_13 := 3 - (apathy_13)] # apathetic


# Regulatory mode - higher is motivated.
quest[, reg_Q13 := 5 - reg_Q13] # wait a little
quest[, reg_Q24 := 5 - reg_Q24] # low energy

# Coup relevance - higher is more relevant, stronger opposistion
quest[, coup_rel_01 := 4 - coup_rel_01] # I have more important stuff
quest[, coup_rel_04 := 4 - coup_rel_04] # No meaningful chage
quest[, coup_rel_05 := 4 - coup_rel_05] # Not for lay people

quest[, coup_rel_08 := 4 - coup_rel_08] # Politician similar to people I know
quest[, coup_rel_09 := 4 - coup_rel_09] # Proud of Israel post reform
quest[, coup_rel_13 := 4 - coup_rel_13] # Judicial system needs a thorough change
quest[, coup_rel_14 := 4 - coup_rel_14] # Judicial needs to be reigned in
quest[, coup_rel_15 := 4 - coup_rel_15] # No representation w/o reform
quest[, coup_rel_18 := 4 - coup_rel_18] # Reform will improve economic status
quest[, coup_rel_20 := 4 - coup_rel_20] # No reform no Jewish state
quest[, coup_rel_21 := 4 - coup_rel_21] # I support reform
quest[, coup_rel_23 := 4 - coup_rel_23] # I support reform relative to two weeks ago

# Coup regulatory focus - higher is promotion
quest[, coup_reg_focus_06 := 4 - coup_reg_focus_06] # Prevent harm to state
quest[, coup_reg_focus_05 := 4 - coup_reg_focus_05] # My duty
quest[, coup_reg_focus_08 := 4 - coup_reg_focus_08] # Conserve character
quest[, coup_reg_focus_07 := 4- coup_reg_focus_07] # Damage control

# Add midgam data ----
midgam[, firstSession := min(startTime), by = userId]
midgam <- midgam[startTime == firstSession]

midgam[, midgam_age := 2023 - byear]
midgam[, midgam_gender := factor(fifelse(sex == 2, "female", "male"))]

# Code family status
fstat_legend <- c("single", "married","separated", "widow/er")
midgam[, family := factor(fstat_legend[fstat]), by = "userId"]

# Recode missing value
midgam[, n_kids := fifelse(kids == 99, NA_integer_, kids)]

# Recode region
region_legend <- c("JLM", "north", "haifa", "center", "TLV", "south",
                   "OT", "abroad")
midgam[, region := factor(region_legend[nafa]), by = "userId"]

# Recode education
midgam[, midgam_edu := factor(edu,
                             levels = 1:13,
                             labels = c("8 or less",
                                        "9, 10",
                                        "11, 12",
                                        "highschool student",
                                        "highschool graduate",
                                        "post highschool non academic student",
                                        "post highschool non academic graduate",
                                        "BA student",
                                        "BA graduate",
                                        "MA student",
                                        "MA graduate",
                                        "PhD student",
                                        "PhD graduate"),
                             ordered = T)]

# Recode religion
religion_legend <- c("jewish", "christian", "muslim", "druze", "other", "no religion")
midgam[, religion := factor(religion_legend[rel]), by = "userId"]

# Recode religiosity
religiosity_legend <- c("secular", "traditional", "religious", "haredi")
midgam[, religiosity := factor(relid, 
                               levels = 1:4,
                               labels = religiosity_legend,
                               ordered = T)]

# Recode income
midgam[, income := factor(ses,
                          levels = 1:6,
                          labels = c("no income",
                                     "much lower than average",
                                     "lower than average",
                                     "average",
                                     "higher than average",
                                     "much higher than average"),
                          ordered = T)]

# Recode birth region
asia <- c('AF','BD','BN','BT','CN','HK','ID','IN','JP','KH','KP','KR','LA',
              'LK','MM','MN','MO','MV','MY','NP','PH','PK','SG','TH','TL','TW',
              'VN')
cent_ame <- c('AG','AI','AN','AW','BB','BM','BS','CU','DM','DO','GD','GP','HT',
              'JM','KN','KY','LC','MQ','MS','PR','TT','VC','BZ','CR','GT','HN',
              'MX','NI','PA','SV')
east_eu <- c('AL','BA','BG','CS','CY','CZ','GR','HR','HU','MK','PL','RO','SI',
             'SK')

magreb <- c('DZ','EG','LY','MA','TN')

middle_east <- c('AE','BH','IQ','IR','JO','KW','LB','OM','QA','SA','SY','TR','YE')

north_ame <- c('CA','US')

oceania <- c('AS','AU','CK','FJ','FM','GU','KI','MH','MP','NC','NR','NU','NZ',
             'PF','PG','PN','PW','SB','TO','TV','VG','VI','VU','WS')

south_ame <- c('AR','BO','BR','CL','CO','EC','FK','GF','GY','PE','PY','SR','UY',
               'VE','PG')

ussr <- c('AM','AZ','BY','EE','GE','KG','KZ','LT','LV','MD','RU','TJ','TM','UA',
          'UZ')

west_eu <- c('AD','AT','BE','CH','DE','DK','ES','FI','FO','FR','GB','GI','GL',
             'IE','IM','IS','IT','JE','LI','LU','MC','MT','NL','NO','PT','SE',
             'SM')

africa <- c('AO','BF','BI','BJ','BW','CD','CF','CG','CI','CM','CV','DJ','EH',
            'ER','ET','GA','GH','GM','GN','GQ','GW', 'KE','KM','LR','LS','MG',
            'ML','MR','MU','MW','MZ','NA','NE','NG','RE','RW','SC','SD','SL',
            'SN','SO','ST','SZ','TD','TG','TZ','UG','YT','ZA','ZM','ZW')

midgam[cbor == "IL", birth_region := "Israel"]
midgam[cbor %in% asia, birth_region := "Asia"]
midgam[cbor %in% cent_ame, birth_region := "Central America"]
midgam[cbor %in% east_eu, birth_region := "Eastern Europe"]
midgam[cbor %in% magreb, birth_region := "Magreb"]
midgam[cbor %in% middle_east, birth_region := "Middle East"]
midgam[cbor %in% north_ame, birth_region := "North America"]
midgam[cbor %in% oceania, birth_region := "Oceania"]
midgam[cbor %in% south_ame, birth_region := "South America"]
midgam[cbor %in% ussr, birth_region := "Former USSR"]
midgam[cbor %in% west_eu, birth_region := "Western Europe"]
midgam[cbor %in% africa, birth_region := "Africa"]

midgam[, birth_region := factor(birth_region)]

# Recode vote
vote2021_legend <- c("likud", "yesh atid", "shas", "kachol lavan", "yamina",
                     "haavoda", "yahadut hatora", "yisrael beiteynu", 
                     "meshutefet", "zionut datit", "tikva hadasha",
                     "meretz", "raam", "other", "didn't vote", NA_character_)

midgam[, party2021 := vote2021_legend[vote2021], by = 'userId']
midgam[, party2021 := factor(party2021)]

vote2022_legend <- c("likud", "yesh atid", "zionut datit", "machane mamlachti", 
                     "shas", "yahadut hatora", "yisrael beiteynu", 
                     "raam", "hadash taal", "haavoda",
                     "meretz", "balad", "habayit hayehudi", "other",
                     "didn't vote", NA_character_)

midgam[, party2022 := vote2022_legend[vote2022], by = 'userId']
midgam[, party2022 := factor(party2022)]

# Merge into quest
midgam[, PID := as.character(userId)]
quest[, sPID := trimws(PID)]
quest <- merge(quest, midgam[, .(PID, midgam_age, midgam_gender, family,
                                n_kids, region, midgam_edu, religion, religiosity,
                                income, birth_region, party2021, party2022)],
               by.x = "sPID", by.y = "PID", all.x = T)

quest$sPID <- NULL

# Save questionnaire to file
save(quest, file = file.path(preprocDatDir, "quest_data.rda"))

# Determine who to invite to second session
quality[, invite := (n_waited > 0) & (important_interactions <= 5) &
          (prop_missed <= 0.2)]

# Save invite list to file
approve <- quality[, .(PID, date, invite)]

secSessFiles <- list.files(rawDatDir, pattern = "secondSessStims")
approve[, secSessFile := sum(grepl(PID, secSessFiles)), by = "PID"]
assert("Missing second sesstion stim files", nrow(approve[invite == T & 
                                                            secSessFile == 0]) == 0)

write.csv(approve[invite == T, .(PID, date)][order(date)], file = file.path(preprocDatDir, "invite.csv"))
write.csv(approve[order(date)], file = file.path(preprocDatDir, "approve.csv"))


# Save quality data to file
write.csv(quality, file = file.path(preprocDatDir, "quality_data.csv"))

