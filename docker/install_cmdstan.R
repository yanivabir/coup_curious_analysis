# Install cmdstanr
options(Ncpus = 6)
install.packages("remotes")
install.packages("posterior")
remotes::install_version("cmdstanr", version="0.7.1", 
                         repos = c("https://mc-stan.org/r-packages/", 
                                   getOption("repos")))
library(cmdstanr)

dir.create(file.path("/home/rstudio/", ".cmdstanr"), recursive = TRUE)
install_cmdstan(version = "2.34.0", 
    cores = 11, 
    dir = file.path("/home/rstudio/", ".cmdstanr"),
    cpp_options = list("CXX" = "clang++"))
cmdstan_path()