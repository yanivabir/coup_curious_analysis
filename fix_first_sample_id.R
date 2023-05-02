# Fix using uid in pilot sample instead of userid

library(data.table)

sampleName <- "v1.0"
preprocDatDir <- file.path("..", "data", sampleName, "preproc")
rawDatDir <- file.path("..", "data", sampleName, "raw")

invite <- fread(file.path(preprocDatDir, "invite.csv"))
invite$V1 <- NULL

files <- list.files(rawDatDir, pattern = "second")

getPID <- function(s) substring(strsplit(s,"_")[[1]][1],2)

for (f in files){
  uid <- getPID(f)
  userid <- invite[invitationId == uid]$userId
  
  file.rename(file.path(rawDatDir, paste0("S",uid, "_secondSessStims.csv")),
              file.path(rawDatDir, paste0("S", userid, "_secondSessStims.csv")))
}

inviteids <- unlist(lapply(files, getPID))

invite[!(invitationId %in% inviteids)]