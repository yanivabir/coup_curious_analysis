# coup_curious_analysis
 Analysis code for coup experiement.
 This code runs on a Docker container, so requires an installation of Docker. To launch RStudio on the container use the following command in your terminal (assumes that your terminal path is this folder, if not, use `cd` to change your path):

 ```
 docker run --rm -p 8787:8787 -e PASSWORD=democratia -v $(pwd):/home/rstudio/analysis -v $(dirname `pwd`)/data:/home/rstudio/data -v $(dirname `pwd`)/saved_models:/home/rstudio/saved_models yanivabir/coup:v1.0
 ```
 
## Progress log:

### 06/27/2023
- Looking at waiting and knowing - it is hard to plot anything but wait v. skip and know v. skip. Trying know v. rest looks messier, plotting all three together is not interpertable, unless you use model fits (at least I hope so).

### 06/21/2023
- Decided with DS to start develop analysis pipelines.

- Tentative plan:

- [x] Apply exclusions

- [x] Relationship between ratings and waiting task

- [x] Factor analyse motivation and affect questionnaires

-- [ ] Represent mixed results

- [ ] Observe development of factors over time, and relationship to demographics

- [x] Relationship of factors with epistemic behaviour

- Looking at bpca of questionnaires. Whatever you do, you get 2-3 factors once you rotate. So if you put just the coup relevance questionnaire in, you get a factor for support, with all support questions, a factor for relevance, and a third factor with the last three relevance questions (coup_rel5-7), which are a little different. I don't see the commonality to the three, so they may be just bad. With 2 factors, you get support vs. relevance, with coup_rel05 (relevant only to politician) loading on support rather than relevance (weak loading). When you put everything in, you get affect (stai + gallup), coup support (mostly support items, relevance items have mediocre loadings, and some of them weak), and motivation (apathy + reg mode). When you put coup relevance, stai and gallup in, you get a factor for stai + gallup, a factor that is mostly coup support, a junk factor, and a fourth factor including only gallup items (these share loadings on 1st and 4th factors). So it seems that 3 and 4 are about how you use questionnaires.
