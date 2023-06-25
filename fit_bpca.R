# This script fits the questionnaire data with a BPCA model

# Load libraries
library(data.table)
library(ggplot2)
library(pcaMethods)
library(GPArotation)

# Paths
sampleName <- "v1.01"
preprocDatDir <- file.path("..", "data", sampleName, "preproc")
savedModelDir <- file.path("..", "saved_models")
savedModelID <- "20230621a" # An identifier for version of saved model output.

# Load data
wait <- fread(file.path(preprocDatDir, "wait_data.csv"))
quest <- fread(file.path(preprocDatDir, "quest_data.csv"), drop = c(1))
quality <- fread(file.path(preprocDatDir, "quality_data.csv"), drop = c(1))

# Exclude participants
source("exclude.R")
excluded_sess1 <- exclude_sess_1(quality, quest, wait)

wait <- wait[!(PID %in% excluded_sess1)]
quest <- quest[!(PID %in% excluded_sess1)]
quality <- quality[!(PID %in% excluded_sess1)]

# Select questionnaire data
qaire <- quest[, grepl("stai|reg_Q|gallup|apathy|coup|PID", 
                       names(quest)), with = F]

# Compute  cross validation for regularized PCA with 1 to 10 components. This takes a LONG time
kCV <- function(data, evalPCs, nCVs, nImputes, seed){
  set.seed(seed)
  
  .data <- as.matrix(data)
  
  errMat <- matrix(NA,  length(evalPCs), nCVs)
  normDat <- apply(.data, 2, scale)
  
  for (jj in 1:nCVs) {
    print(paste0("run ", jj))
    # Draw elements to delete at random
    delIndx <- sample(which(!is.na(normDat)), nImputes)
    
    # Create copy of data and delete elements from it
    missDat <- normDat
    missDat[delIndx] <- NA
    
    for (ii in 1:length(evalPCs)){
      print(paste0(evalPCs[ii], " PCs"))
      # Impute deleted data
      imputed <- completeObs(pca(missDat, method = "bpca", nPcs = evalPCs[ii], scale = "none", center = F))
      
      # Calculate rmse for deleted data
      errMat[ii, jj] <- sum((normDat[delIndx] - imputed[delIndx])^2) / sum(normDat[delIndx]^2)
    }
  }
  
  return(errMat)
}

cvFile <- file.path(savedModelDir, paste0("bpca_CV_", savedModelID, ".rda"))
if (!file.exists(cvFile)){
  errMat <- kCV(qaire[, -c("PID"), with = F], 1:10, 10, round(sum(!is.na(qaire)) / 20), 524)
  save(errMat, file = cvFile)
}else{
  load(cvFile)
}

# Plot CV results
kCVe_sum <- data.frame(m = apply(errMat,1,mean), sd = apply(errMat,1,sd))
ggplot(kCVe_sum, aes(x = 1:10, y = m, ymin = m-sd/2, ymax = m+sd/2)) + geom_pointrange()

# Performa a regualarized PCA
coup_rel <- qaire[, grepl("coup|stai|gallup", 
                          names(qaire)), with = F]
coup_rel <- coup_rel[rowSums(is.na(coup_rel)) != ncol(coup_rel)]

firstPCA <- pca(coup_rel, method = "bpca", nPcs = 10, scale = "uv")

q2 <- Q2(firstPCA, type = "krzanowski")

ggplot(data.frame(PC = 1:10, q2 = q2), aes(x = PC, y = q2)) + geom_point()

# Performa a regularized PCA with 3 components
quairePCA <- pca(coup_rel, method = "bpca", nPcs = 4, scale = "uv")

# Rotate the loadings matrix - Quartimax looks for the rotation that assigns each variable to one PC as much as we can (keeps the rows sparse)
quairePCA_rotLoad <- quartimax(loadings(quairePCA))$loadings

quairePCA_rotLoad <- as.data.table(quairePCA_rotLoad, keep.rownames = T)

prettyLoad <- function(x) ifelse(abs(x) < 0.2, "", sprintf("%0.2f", x))
ß <- quairePCA_rotLoad[, .(rn = rn,
                                          PC1 = prettyLoad(PC1),
                                          PC2 = prettyLoad(PC2),
                                          PC3 = prettyLoad(PC3),
                                          PC4 = prettyLoad(PC4))]
