## This script preprocesses data, and returns stats for participant approval
library(data.table)
setDTthreads(11)
library(jsonlite)

sampleName <- "v1.0"
rawDatDir <- file.path("..", "data", sampleName, "raw")
preprocDatDir <- file.path("..", "data", sampleName, "preproc")

# Load data ----
# List relevant files
files <- list.files(rawDatDir, pattern = "sess2")
mfiles <- files[grepl(".csv", files, fixed =T) & !grepl("int", files)]
intfiles <- files[grepl(".csv", files, fixed =T) & grepl("int", files)]

