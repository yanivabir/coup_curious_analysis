# coup_curious_analysis
 Analysis code for coup experiement.
 This code runs on a Docker container, so requires an installation of Docker. To launch RStudio on the container use the following command in your terminal (assumes that your terminal path is this folder, if not, use `cd` to change your path):

 ```
 docker run --rm -p 8787:8787 -e PASSWORD=democratia -v $(pwd):/home/rstudio/analysis -v $(dirname `pwd`)/data:/home/rstudio/data -v $(dirname `pwd`)/saved_models:/home/rstudio/saved_models yanivabir/coup:v1.1ma
 ```
 
## Progress log:

### 07/05/2023
- Plotting with me term and quadratic predictor is broken in brms, and doing it on my own is not straightforward.

- Should probably just start working on one grand Stan model before plotting.

### 07/02/2023
- Models for ratings with measurement error fitting well. We do see an inverse u-shape for confidence, as evidenced by a negative quadratic term. Considerably larger for waiting than for knowing. Need to plot this - but waiting for model with interaction with block to fit, and then will plot based on that.

- A glance at conditional_effects confirms that as expected model predictions are much clearer than plotting raw data.

- Fitting individual differneces measure model. For now, no interaction with ratings.

- I think we cannot use brms for the full mediation model. It will have to be a very complicated stan one. :(

- Tentative plan:

- [ ] Plot model predictions for ratings

- [ ] Plot model predictions for naive ID measures.

### 06/27/2023
- Looking at waiting and knowing - it is hard to plot anything but wait v. skip and know v. skip. Trying know v. rest looks messier, plotting all three together is not interpertable, unless you use model fits (at least I hope so).

### 06/21/2023
- Decided with DS to start develop analysis pipelines.

- Tentative plan:

- [x] Apply exclusions

- [x] Relationship between ratings and waiting task

- [x] Factor analyse motivation and affect questionnaires

-- [ ] Represent mixed results

- [x] Observe development of factors over time, and relationship to demographics

- [x] Relationship of factors with epistemic behaviour

- Looking at bpca of questionnaires. Whatever you do, you get 2-3 factors once you rotate. So if you put just the coup relevance questionnaire in, you get a factor for support, with all support questions, a factor for relevance, and a third factor with the last three relevance questions (coup_rel5-7), which are a little different. I don't see the commonality to the three, so they may be just bad. With 2 factors, you get support vs. relevance, with coup_rel05 (relevant only to politician) loading on support rather than relevance (weak loading). When you put everything in, you get affect (stai + gallup), coup support (mostly support items, relevance items have mediocre loadings, and some of them weak), and motivation (apathy + reg mode). When you put coup relevance, stai and gallup in, you get a factor for stai + gallup, a factor that is mostly coup support, a junk factor, and a fourth factor including only gallup items (these share loadings on 1st and 4th factors). So it seems that 3 and 4 are about how you use questionnaires.
