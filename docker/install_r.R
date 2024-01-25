# Following instructions from 
# https://github.com/stan-dev/rstan/wiki/Installing-RStan-on-Linux

args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
	platform = "linux/arm64/v8"
}else{
	platform = args[1]
}

print("Running install_r.R")

## Creating /home/rstudio/.R/Makevars for rstan
dotR <- file.path(Sys.getenv("HOME"), ".R")
if (!file.exists(dotR)) dir.create(dotR)
M <- file.path(dotR, "Makevars")
if (!file.exists(M)) file.create(M)
cat("\nCXX14FLAGS=-O3 -march=native -mtune=native -fPIC",
    		"CXX14=clang++",
    		file = M, sep = "\n", append = TRUE)

if (platform == "linux/arm64/v8"){
	cat("CXX14FLAGS += -O3 -march=ARMv8.6-A -ftemplate-depth-256",
    		file = M, sep = "\n", append = FALSE)
}

options(Ncpus = 12)

# Install packages
# install.packages("devtools")
# library(devtools)
Sys.setenv(DOWNLOAD_STATIC_LIBV8 = 1)
install.packages(c("data.table", "rstan", "brms", "brms", "ggplot2", "cowplot",
                   "GGally","psych", "GPArotation", "BiocManager"))

BiocManager::install(c("pcaMethods"))
