# coup_curious_analysis
 Analysis code for coup experiement.
 This code runs on a Docker container, so requires an installation of Docker. To launch RStudio on the container use the following command in your terminal (assumes that your terminal path is this folder, if not, use `cd` to change your path):

 ```
 docker run --rm -p 8787:8787 -e PASSWORD=democratia -v $(pwd):/home/rstudio/analysis -v $(dirname `pwd`)/data:/home/rstudio/data -v $(dirname `pwd`)/saved_models:/home/rstudio/saved_models yanivabir/coup:v1.0
 ```
 
## Progress log:

### 06/21/2023
- Decided with DS to start develop analysis pipelines.

- Tentative plan:

- [ ] Relationship between ratings and waiting task

- [ ] Factor analyse motivation and affect questionnaires

- [ ] Observe development of factors over time, and relationship to demographics

- [ ] Relationship of factors with epistemic behaviour
