# Plotting functions -----
# Function to plot by factor order rather than by geom
getGeoms <-
  function(p,
           dat,
           col = "block",
           outer = T,
           ls = 1,
           fill_legend = F) {
    dat[, iv := get(col)]
    levs <- levels(dat$iv)
    
    for (c in 1:length(levs)) {
      if (outer) {
        p <- p + geom_ribbon(
          data = dat[iv == levs[c]],
          aes(
            ymin = llb,
            ymax = uub,
            fill = iv
          ),
          alpha = .1,
          colour = NA,
          show.legend = fill_legend
        )
      }
      p <- p +
        geom_ribbon(
          data = dat[iv == levs[c]],
          aes(
            ymin = lb,
            ymax = ub,
            fill = iv
          ),
          alpha = .5,
          colour = NA,
          show.legend = F
        ) +
        geom_line(data = dat[iv == levs[c]],
                  aes(color = iv,
                      linetype = iv),
                  linewidth = ls)
    }
    
    return(p)
  }

# Functions that prints pretty numbers

printnum <- function(x) {
  if (abs(x) < .001) {
    return(sprintf("%0.2e", x))
  }
  else if (abs(x) < .01) {
    return(sprintf("%0.3f", x))
  }
  else {
    return(sprintf("%0.2f", x))
  }
}

printp <- function(x) {
  if (x >= .001) {
    return(paste0("=", printnum(x)))
  } else {
    return("<0.001")
  }
}

# Function to set common y-axis limits for a list of ggplot objects
set_common_y_limits <- function(...) {
  # Combine all ggplot objects into a list
  plots_list <- list(...)
  
  # Extract the y-axis limits from each plot
  y_limits <- lapply(plots_list, function(p) {
    y_range <- layer_scales(p)$y$range$range
    return(y_range)
  })
  
  # Find the overall y-axis limits
  common_y_limits <-
    c(min(sapply(y_limits, "[[", 1)), max(sapply(y_limits, "[[", 2)))
  
  # Update the y-axis limits for each plot
  plots_list <- lapply(plots_list, function(p) {
    p + ylim(common_y_limits[1], common_y_limits[2])
  })
  
  # Return the modified plots
  return(plots_list)
}

# Return sequence for plotting
plot_seq <- function(x) {
  return(seq(min(x), max(x), length.out = res))
}

# Summarize draws
llb <- function(x)
  quantile(x, 0.025)
lb <- function(x)
  quantile(x, 0.25)
ub <- function(x)
  quantile(x, 0.75)
uub <- function(x)
  quantile(x, 0.975)
sum_draws <- function(x)
  data.table(
    m = apply(x, 2, median),
    llb = apply(x, 2, llb),
    lb = apply(x, 2, lb),
    ub = apply(x, 2, ub),
    uub = apply(x, 2, uub)
  )