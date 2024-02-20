# Install cmdstanr
options(Ncpus = 12)
install.packages("remotes")
remotes::install_github('stan-dev/posterior', 'v1.5.0')
remotes::install_version("cmdstanr", version="0.7.1", 
                         repos = c("https://mc-stan.org/r-packages/", 
                                   getOption("repos")))
library(cmdstanr)

dir.create(file.path("/home/rstudio/", ".cmdstanr"), recursive = TRUE)
install_cmdstan(version = "2.30.1", 
    cores = 11, 
    dir = file.path("/home/rstudio/", ".cmdstanr"),
    cpp_options = list("CXX" = "clang++"))
cmdstan_path()