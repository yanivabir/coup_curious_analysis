# This script contains plots for plotting model outputs

get_range <- function(v, res = 200){
  return(seq(min(v), max(v), length.out = res))
}

get_range_renorm <- function(v, mu, sigma, res = 200){
  z <- zscore(v)
  x <- mu + z * sigma
  return(get_range(x, res = res))
}
  
plot_rating_waiting(rw_me2, wait_ff) {
  fp <- as.data.table(expand.grid(list(Estimate_useful = get_range(wait_ff$Estimate_useful),
                   wait_duration = unique(wait_ff$wait_duration),
                   block = unique(wait_ff$block))))
  
  preds <- fitted(rw_me2, newdata = fp, 
                  re_formula = choice ~ 1 + Estimate_useful * block + wait_duration)
}

