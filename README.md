# coup_curious_analysis
 Analysis code for coup experiement.
 This code runs on a Docker container, so requires an installation of Docker. To launch RStudio on the container use the following command in your terminal (assumes that your terminal path is this folder, if not, use `cd` to change your path):

 ```
 docker run --rm -p 8787:8787 -e PASSWORD=democratia -v $(pwd):/home/rstudio/analysis -v $(dirname `pwd`)/data:/home/rstudio/data -v $(dirname `pwd`)/saved_models:/home/rstudio/saved_models yanivabir/coup:v1.0
 ```
 
## Progress log:

