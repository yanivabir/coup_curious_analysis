FROM rocker/rstudio:latest-daily@sha256:ad4dca6727c6d996a3c75f34d11090c16d1a4e1f2f7f88ae5387cc4c3f897109

# Using clang to compile Stan
# Using the default g++ causes memory issues
RUN apt-get update \
&& apt-get install -y --no-install-recommends \
clang

# Set CRAN date
RUN echo 'options(repos = c(CRAN = "https://packagemanager.rstudio.com/cran/2023-04-21"), download.file.method = "libcurl")' >> $(Rscript -e 'cat(R.home())')/etc/Rprofile.site

RUN apt-get update \
&& apt-get install -y --no-install-recommends \
libgit2-dev \
libfontconfig1-dev \
libharfbuzz-dev \
libfribidi-dev \
libfreetype6-dev \
libpng-dev \
libtiff5-dev \
libjpeg-dev \
libxml2-dev \
libcairo2-dev \
libsqlite-dev \
libmariadbd-dev \
#libmariadbclient-dev \
libpq-dev \
libssh2-1-dev \
unixodbc-dev \
libsasl2-dev

# Run script installing packages
COPY install_r.R install_r.R
RUN ["r", "install_r.R"]

# Run script installing cmdstand
COPY install_cmdstan.R install_cmdstan.R
RUN ["r", "install_cmdstan.R"]
# RUN sudo chown -R rstudio /home/rstudio/.cmdstanr

# Install additional r libraries
RUN Rscript -e "install.packages('gsubfn')"

# Install additional packages needed
RUN apt-get update \
&& apt-get install -y --no-install-recommends \
libxt-dev

# Install additional r libraries
RUN Rscript -e "install.packages('bit64')"
RUN Rscript -e "install.packages('testit')"
RUN Rscript -e "install.packages('ggrain')"
RUN Rscript -e "install.packages('lubridate')"
RUN Rscript -e "install.packages('corrplot')"
RUN Rscript -e "install.packages('rvest')"
RUN apt-get update \
&& apt-get install -y --no-install-recommends \
libssh-dev
RUN Rscript -e "install.packages('ssh')"
RUN Rscript -e "tinytex::install_tinytex()"
