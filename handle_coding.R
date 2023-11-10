# This script exports unique recalls for coding

# Load libraries
library(data.table)

# File paths
preprocDatDir <- file.path("..", "data", "v1.01", "preproc")

# Load recall data
load(file.path(preprocDatDir, "recall_data.rda"))

saveForCoding <- function(recall){
  
  # Find unique
  unique_recall <- unique(recall[is.na(correct) & (!is.na(recall)), 
                                 .(questionId, question, answer, recall)])
  
  # Sort
  unique_recall <- unique_recall[order(questionId)]
  
  # Save to file
  write.csv(unique_recall, file = 
              file.path(preprocDatDir, 
                        paste(Sys.Date(),"recall_for_coding.csv", sep="_")))
  
  return(unique_recall)
}

