# Install cmdstanr
library(devtools)
options(Ncpus = 12)
install_version("cmdstanr", version="0.5.3", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
library(cmdstanr)

dir.create(file.path("/home/rstudio/", ".cmdstanr"), recursive = TRUE)
install_cmdstan(version = "2.32.0", 
    cores = 11, 
    dir = file.path("/home/rstudio/", ".cmdstanr"),
    cpp_options = list("CXX" = "clang++"))
cmdstan_path()