# This script exports unique recalls for coding

# Load libraries
library(data.table)

# File paths
preprocDatDir <- file.path("..", "data", "v1.01", "preproc")

# Load recall data
load(file.path(preprocDatDir, "recall_data.rda"))
load(file.path(preprocDatDir, "answer_knowledge_data.rda"))


saveForCoding <- function(dt, col, name){
  
  # Find unique
  unique_dt <- unique(dt[is.na(correct) & (!is.na(get(col))), 
                                 .(questionId, question, answer, get(col))])
  
  # Sort
  unique_dt <- unique_dt[order(questionId)]
  
  # Save to file
  write.csv(unique_dt, file = 
              file.path(preprocDatDir, 
                        paste(Sys.Date(),paste0(name, "_for_coding.csv"), sep="_")))
  
  return(unique_dt)
}

saveForCoding(know, "response", "answer_knowledge")