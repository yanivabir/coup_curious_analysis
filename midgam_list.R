## Translate Midgam invitation IDs to user IDs
library(data.table)

sampleName <- "v1.0"
preprocDatDir <- file.path("..", "data", sampleName, "preproc")
midgamDatDir <- file.path("..", "data", sampleName, "midgam")


invite <- fread(file.path(preprocDatDir, "invite.csv"))

midgam_list <- fread(file.path(midgamDatDir, "dshoamy2023042701.csv"))

invite <- merge(midgam_list[, .(invitationId, userId)], invite, 
                     by.x = "invitationId", by.y = "PID")

invite$V1 <- NULL

write.csv(invite, file = file.path(preprocDatDir, "invite.csv"))
