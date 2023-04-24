# Following instructions from 
# https://github.com/stan-dev/rstan/wiki/Installing-RStan-on-Linux

print("Running install_r.R")

## Creating /home/rstudio/.R/Makevars for rstan
dotR <- file.path(Sys.getenv("HOME"), ".R")
if (!file.exists(dotR)) dir.create(dotR)
M <- file.path(dotR, "Makevars")
if (!file.exists(M)) file.create(M)
cat("\nCXX14FLAGS=-O3 -march=native -mtune=native -fPIC",
    "CXX14=clang++",
    file = M, sep = "\n", append = TRUE)

options(Ncpus = 12)

# Install packages
install.packages("devtools")
library(devtools)
install.packages("data.table")
install.packages("rstan")
install.packages("brms")
install.packages("loo")
install.packages("ggplot2")
install.packages("cowplot")
install.packages("GGally")
install.packages("psych")

install.packages("BiocManager")
BiocManager::install(c("pcaMethods"))

install.packages("GPArotation")
