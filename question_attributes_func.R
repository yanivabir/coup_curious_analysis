plot_wait_know_attr <- function(model,
                                col,
                                label,
                                attributes = c("useful", "confidence", "affect"),
                                quadratic = T,
                                combine = T,
                                title = NULL,
                                plot_questions = T,
                                model_name = deparse(substitute(model))) {
  
  print(paste("Plotting for", model_name))
  
  # Prepare values for prediction
  plot_dat <- rbindlist(lapply(unique(model$data$block),
                               function(x)
                                 data.table(col = plot_seq(model$data[col][model$data$block == x, ]),
                                            block = x)))
  
  colnames(plot_dat) <- c(col, "block")
  
  # Set all additional predictors to median
  plot_dat[, wait_s := 0]
  
  other_attr <- attributes[attributes != col]
  plot_dat[, (other_attr) := lapply(other_attr, function(x)
    median(model$data[x][[1]]))]
  
  if (quadratic) {
    plot_dat[, `I(confidence^2)` := confidence ^ 2]
    plot_dat[, `I(affect^2)` := affect ^ 2]
    plot_dat[, `I(useful^2)` := useful ^ 2]
  }
  
  # Predict - this returns ndraws x npoints x 3
  # Fit only if not saved
  pred_file <- file.path(cacheDir,
                         paste0(model_name,
                                "_",
                                col,
                                "_line_predictions.rda"))
  if (!file.exists(pred_file)) {
    pred <- fitted(
      model,
      newdata = plot_dat,
      re_formula = NA,
      summary = F,
      draw_ids = sample(1:4000, n_draws)
    )
    save(pred, file = pred_file)
  } else {
    load(pred_file)
  }
  
  
  # Summarize as wait, know vs. skip
  list[pred_wait, pred_know] <- sum_cat(pred)
  
  pred_wait <- cbind(plot_dat, pred_wait)
  
  if (plot_questions) {
    # Predict per question
    qcoefs_data <- unique(as.data.table(model$data)[, .(questionId,
                                                        block,
                                                        confidence,
                                                        useful,
                                                        affect)], by = "questionId")
    # Take residuals over other attributres
    qcoefs_data[, (other_attr) := lapply(other_attr, function(x)
      median(model$data[x][[1]]))]
    qcoefs_data[, wait_s := 0]
    
    if (quadratic) {
      qcoefs_data[, `I(confidence^2)` := confidence ^ 2]
      qcoefs_data[, `I(affect^2)` := affect ^ 2]
      qcoefs_data[, `I(useful^2)` := useful ^ 2]
    }
    
    # Fit only if not saved
    qcoef_file <- file.path(cacheDir,
                            paste0(model_name,
                                   "_",
                                   col,
                                   "_point_predictions.rda"))
    if (!file.exists(qcoef_file)) {
      qcoefs <- fitted(
        model,
        newdata = qcoefs_data,
        re_formula = ~ (1 + wait_s | questionId),
        summary = F,
        draw_ids = sample(1:4000, n_draws)
      )
      save(qcoefs, file = qcoef_file)
    } else {
      load(qcoef_file)
    }
    
    # Summarize as wait, know vs. skip
    list[qcoefs_wait, qcoefs_know] <- sum_cat(qcoefs)
    
    qcoefs_wait <- cbind(qcoefs_data, qcoefs_wait)
    qcoefs_know <- cbind(qcoefs_data, qcoefs_know)
    
    qcoefs_wait <- merge(qcoefs_wait, quest_text, by = "questionId")
    qcoefs_know <- merge(qcoefs_know, quest_text, by = "questionId")
    
    qcoefs_wait[, block := factor(block,
                                  levels = block_levels,
                                  labels = block_labels)]
    qcoefs_wait[, x := get(col)]
    
    qcoefs_know[, block := factor(block,
                                  levels = block_levels,
                                  labels = block_labels)]
    qcoefs_know[, x := get(col)]
    
  }
  
  
  # Plot waiting
  pred_wait[, block := factor(block,
                              levels = block_levels,
                              labels = block_labels)]
  pred_wait[, x := get(col)]
  
  p_wait <- ggplot(pred_wait, aes(
    x = x,
    y = m,
    fill = block,
    color = block
  )) +
    labs(
      x = label,
      y = "Prop. waited vs. skipped",
      color = "Block",
      fill = "",
      linetype = "Block",
      title = title
    ) +
    scale_x_continuous(expand = expansion(mult = c(0, 0))) +
    scale_fill_manual(breaks = block_labels,
                      values = block_colors) +
    scale_color_manual(breaks = block_labels,
                       values = block_colors) +
    scale_linetype_manual(breaks = block_labels,
                          values = block_lines) +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5))
  
  if (plot_questions) {
    p_wait <- p_wait + geom_point(data = qcoefs_wait,
                                  aes(x = x,
                                      y = m,
                                      text = question),
                                  alpha = 0.3)
  }
  
  p_wait <- getGeoms(p_wait,
                     pred_wait,
                     col = "block",
                     fill_legend = T)
  
  
  # Plot know
  pred_know <- cbind(plot_dat, pred_know)
  pred_know[, block := factor(block,
                              levels = block_levels,
                              labels = block_labels)]
  
  pred_know[, x := get(col)]
  
  p_know <- ggplot(pred_know, aes(
    x = x,
    y = m,
    fill = block,
    color = block
  )) +
    labs(
      x = label,
      y = "Prop. known vs. skipped",
      color = "Question type",
      fill = "Question type",
      linetype = "Question type",
      title = title
    ) +
    scale_x_continuous(expand = expansion(mult = c(0, 0))) +
    scale_fill_manual(breaks = block_labels,
                      values = block_colors) +
    scale_color_manual(breaks = block_labels,
                       values = block_colors) +
    scale_linetype_manual(breaks = block_labels,
                          values = block_lines) +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5))
  
  if (plot_questions) {
    p_know <- p_know + geom_point(data = qcoefs_know,
                                  aes(
                                    x = x,
                                    y = m,
                                    color = block,
                                    text = question
                                  ),
                                  alpha = 0.3)
  }
  
  p_know <- getGeoms(p_know,
                     pred_know,
                     col = "block",
                     fill_legend = T)
  
  
  # This might help with memory usage
  gc()
  
  if (combine){
    return(combine_wait_know_plotly(p_wait, p_know))
  }else{
    return(list(p_wait, p_know))
  }
}

combine_wait_know_plotly <- function(...) {
  # Set common y axes
  ps <- set_common_y_limits(...)
  
  # Plotlyfy
  ps <- lapply(ps, function(p) ggplotly(p, tooltip = "question"))

  # Remove legend from anything but lines
  for (ii in c(1:4, 6,7)) {
    ps[[1]]$x$data[[ii]]$showlegend <- F
  }
  
  # Remove legend from anything but first subplot
  for (i in 2:length(ps)){
    for(j in 1:length(ps[[i]]$x$data)){
      ps[[i]]$x$data[[j]]$showlegend <- F
    }
  }
  
  
  # Create one plot
  p <- subplot(ps,
               titleX = T,
               titleY = T,
               margin = 0.05) %>%
    layout(
      showlegend = T,
      legend = list(
        x = 0,
        xanchor = 'left',
        yanchor = 'top'
      )
    )
  
  return(p)
}

prep_text <- function() {
  sampleName <- "v1.01"
  source("load_data_and_exclude.R")
  list[wait, rating_clps, know_test,
       prob_judge, quest, quality] = load_exclude(sampleName)
  
  quest_text <- unique(wait[, .(questionId, question)])
  quest_text[, question := gsub('""""', '"', question)]
  
  return(quest_text)
}
