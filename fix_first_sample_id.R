# Fix using uid in pilot sample instead of userid

library(data.table)

sampleName <- "v1.0"
preprocDatDir <- file.path("..", "data", sampleName, "preproc")
rawDatDir <- file.path("..", "data", sampleName, "raw")
midgamDatDir <- file.path("..", "data", sampleName, "midgam")

midgam_list <- fread(file.path(midgamDatDir, "dshoamy2023042701.csv"))

files <- list.files(rawDatDir, pattern = "sess1")

renamePIDs <- function(dat) dat[.(PID = midgam_list$invitationId, 
                                  to = midgam_list$userId), 
                                on = "PID", 
                                PID := i.to]

for (f in files){
  
  # Get new PID
  PID <- substring(strsplit(f,"_")[[1]][1],2)
  print(paste0("working on ", PID))
  PID <- midgam_list[invitationId == PID]$userId
  
  # Load data
  dat <- fread(file.path(rawDatDir, f))
  
  # Rename PIDs
  if (!is.null(dat$PID)){
    dat <- renamePIDs(dat)
  }
  
  # Get file name
  fname <- paste0(strsplit(f,"_")[[1]][-1], collapse = "_")
  fname <- paste0("S", PID, "_", fname)
  
  # Save data
  print(paste0("Saving to ", fname))
  write.csv(dat, file = file.path(preprocDatDir, fname))
}