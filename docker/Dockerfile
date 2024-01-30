FROM rocker/rstudio:4.3.2

ARG TARGETPLATFORM

# Using clang to compile Stan
# Using the default g++ causes memory issues
RUN apt-get update \
&& apt-get install -y --no-install-recommends \
clang

# Set CRAN date
RUN echo 'options(repos = c(CRAN = "https://packagemanager.rstudio.com/cran/2023-01-26"), download.file.method = "libcurl")' >> $(Rscript -e 'cat(R.home())')/etc/Rprofile.site

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

