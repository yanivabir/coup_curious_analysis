prepare_pred_dat_group_mean <- function(model,
                                        col,
                                        measures) {
  # Prepare values for group mean prediction ----
  plot_dat_line <- rbindlist(lapply(unique(model$data$block),
                                    function(x)
                                      data.table(col = plot_seq(model$data[col][model$data$block == x, ]),
                                                 block = x)))
  
  colnames(plot_dat_line) <- c(col, "block")
  
  # Set all additional predictors to median
  other_measures <- measures[measures != col]
  plot_dat_line[, (other_measures) := lapply(other_measures, function(x)
    median(model$data[x][[1]]))]
  plot_dat_line[, wait_s := 0]
  
  return(plot_dat_line)
}

plot_choice_ID <- function(model,
                           col,
                           label,
                           qmeasures = c("affect",
                                         "confidence",
                                         "useful"),
                           pmeasures = c("coup_relevance_s",
                                         "coup_attitude_s",
                                         "affect_s",
                                         "motivation_s"),
                           tmeasures = c("wait_s"),
                           quadratic = c("coup_attitude_s",
                                         "affect",
                                         "confidence"),
                           nbins = 8) {
  measures <- c(qmeasures, pmeasures, tmeasures)
  model_name <- deparse(substitute(model))
  
  # Predict for group mean ----
  # Predict - this returns ndraws x npoints x 5
  print("Predicting population effect")
  
  plot_dat_line <- prepare_pred_dat_group_mean(model, col, measures)
  
  pred_file <- file.path(savedModelsDir,
                         "ID_measures",
                         paste(model_name, col, "preds.rda",
                               sep = "_"))
  if (!file.exists(pred_file)) {
    pred_line <- fitted(
      model,
      newdata = plot_dat_line,
      re_formula = NA,
      summary = F,
      draw_ids = sample(1:4000, n_draws)
    )
    
    # Summarize into mean raiting
    pred_lines <- sum_cat(pred_line)
    
    save(pred_lines, file = pred_file)
  } else {
    load(pred_file)
  }
  
  # Combine with predictors
  pred_lines <-
    lapply(pred_lines, function(dt)
      cbind(plot_dat_line, dt))
  
  # Prepare variables
  pred_lines <-
    lapply(pred_lines, function(dt)
      dt[, block := factor(block,
                           levels = block_levels,
                           labels = block_labels)])
  pred_lines <- lapply(pred_lines, function(dt)
    dt[, x := get(col)])
  
  # Predict for participants ----
  print("Computing per participants predictions")
  
  # Prepare data for participants-wise predictions
  plot_dat_points <-
    unique(as.data.table(model$data)[, c("PID", "block",
                                         pmeasures),
                                     with = F])
  
  # Set all additional predictors to median
  other_measures <- c(qmeasures, tmeasures)
  plot_dat_points[, (other_measures) := lapply(other_measures, function(x)
    median(model$data[x][[1]]))]
  plot_dat_points[, wait_s := 0]
  plot_dat_points[, questionId := NA]

  # Compute quadratics
  if (!is.null(quadratic)) {
    lapply(quadratic, function(x) {
      plot_dat_points[, paste0("I(", x, "^2)") := .(get(x) ^ 2)]
    })
  }
  
  # Extract PID formula from model
  re_formula <- as.character(model$formula)[[1]]
  re_formula <- regmatches(re_formula,
                           gregexpr("\\(1.*?\\| PID\\)", re_formula))[[1]]
  re_formula <- as.formula(paste0("~ ", re_formula))
  
  # Predict values
  pred_points_file <- file.path(
    savedModelsDir,
    "ID_measures",
    paste(model_name,
          col,
          "PID",
          "preds.rda",
          sep = "_")
  )
  if (!file.exists(pred_points_file)) {
    pred_points <- fitted(
      model,
      newdata = plot_dat_points,
      summary = F,
      draw_ids = sample(1:4000, n_draws)
    )
    
    # Summarize into mean raiting
    pred_points <- sum_cat(pred_points)
    
    save(pred_points, file = pred_points_file)
  } else {
    load(pred_points_file)
  }
  
  # Combine with predictors
  pred_points <-
    lapply(pred_points, function(dt)
      cbind(plot_dat_points, dt))
  
  # Prepare variables
  pred_points <-
    lapply(pred_points, function(dt)
      dt[, block := factor(block,
                           levels = block_levels,
                           labels = block_labels)])
  pred_points <-
    lapply(pred_points, function(dt)
      dt[, x := get(col)])
  
  # Bin x axis
  pred_points <- lapply(pred_points, function(dt) {
    # Compute precision
    dt[, prec := 1 / sd]
    
    # Compute bins
    dt[, bin_n := factor(dplyr::ntile(get(col), nbins))]
    dt[, bin := mean(get(col)), by = bin_n]
    
    dt_sum <-
      dt[, .(
        m = Hmisc::wtd.mean(m, weights = prec, normwt = T),
        se = sqrt(Hmisc::wtd.var(
          m, weights = prec, normwt = T
        )
        / .N)
      ),
      by = c("bin", "block")]
    
    return(dt_sum)
  })
  
  plot_pred <- function(resp,
                        resp_label) {
    print(paste0("Plotting ", resp_label, " by ", label))
    
    p <- ggplot(pred_lines[[(resp == "know") + 1]], aes(
      x = x,
      y = m,
      fill = block,
      color = block
    )) +
      labs(
        x = label,
        y = resp_label,
        color = "Block",
        fill = "Block",
        linetype = "Block"
      ) +
      scale_x_continuous(expand = expansion(mult = c(0, 0))) +
      scale_fill_manual(breaks = block_labels,
                        values = block_colors) +
      scale_color_manual(breaks = block_labels,
                         values = block_colors) +
      scale_linetype_manual(breaks = block_labels,
                            values = block_lines) +
      theme(legend.position = "none") +
      geom_pointrange(
        data = pred_points[[(resp == "know") + 1]],
        aes(
          x = bin,
          y = m,
          ymin = m - se,
          ymax = m + se,
          color = block
        ),
        alpha = 0.3,
        show.legend = F
      )
    
    p <- getGeoms(p,
                  pred_lines[[(resp == "know") + 1]],
                  col = "block",
                  fill_legend = T)
    
    # This might help with memory usage
    gc()
    
    return(p)
    
  }
  
  p_wait <- plot_pred("wait",
                        "Prop. waited")
  
  p_know <- plot_pred("know",
                            "Prop. known")
  

  return(list(p_wait, p_know))
}

plot_rating_ID <- function(model,
                           col,
                           label,
                           measures = c("coup_relevance_s",
                                        "coup_attitude_s",
                                        "affect_s",
                                        "motivation_s"),
                           quadratic = c("coup_attitude_s"),
                           nbins = 8) {
  plot_dat_line <- prepare_pred_dat_group_mean(model, col, measures)
  
  plot_pred <- function(resp,
                        resp_label) {
    print(paste0("Plotting ", resp_label, " by ", label))
    # Predict for group mean ----
    # Predict - this returns ndraws x npoints x 5
    print("Predicting population effect")
    pred_line <- fitted(
      model,
      newdata = plot_dat_line,
      re_formula = NA,
      summary = F,
      resp = resp
    )
    
    # Summarize into mean raiting
    pred_line <- sum_ordinal(pred_line)
    
    # Combine with predictors
    pred_line <- cbind(plot_dat_line, pred_line)
    
    # Prepare variables
    pred_line[, block := factor(block,
                                levels = block_levels,
                                labels = block_labels)]
    pred_line[, x := get(col)]
    
    # Predict for participants ----
    print("Computing raws means")
    
    # Bin and summarize
    rm <- as.data.table(model$data)
    rm <- rm[, .(m = mean(get(resp)),
                 sd = sd(get(resp)),
                 prec = 1 / sd(get(resp))), by = c("PID", "block", col)]
    rm[, bin_n := factor(dplyr::ntile(get(col), nbins))]
    rm[, bin := mean(get(col)), by = bin_n]
    rm <- rm[prec < Inf]
    rm_sum <-
      rm[, .(
        m = Hmisc::wtd.mean(m, weights = prec, normwt = T),
        se = sqrt(Hmisc::wtd.var(
          m, weights = prec, normwt = T
        )
        / .N)
      ),
      by = c("bin", "block")]
    rm_sum[, block := factor(block,
                             levels = block_levels,
                             labels = block_labels)]
    
    print("Plotting")
    p <- ggplot(pred_line, aes(
      x = x,
      y = m,
      fill = block,
      color = block
    )) +
      labs(
        x = label,
        y = resp_label,
        color = "Block",
        fill = "Block",
        linetype = "Block"
      ) +
      scale_x_continuous(expand = expansion(mult = c(0, 0))) +
      scale_fill_manual(breaks = block_labels,
                        values = block_colors) +
      scale_color_manual(breaks = block_labels,
                         values = block_colors) +
      scale_linetype_manual(breaks = block_labels,
                            values = block_lines) +
      theme(legend.position = "none") +
      geom_pointrange(
        data = rm_sum,
        aes(
          x = bin,
          y = m,
          ymin = m - se,
          ymax = m + se,
          color = block
        ),
        alpha = 0.3,
        show.legend = F
      )
    
    p <- getGeoms(p,
                  pred_line,
                  col = "block",
                  fill_legend = T)
    
    # This might help with memory usage
    gc()
    
    return(p)
    
  }
  
  p_affect <- plot_pred("affect",
                        "Expected answer affect")
  
  p_confidence <- plot_pred("confidence",
                            "Confidence")
  
  p_useful <- plot_pred("useful",
                        "Expected ansewr usefulness")
  
  return(list(p_useful, p_confidence, p_affect))
}

plot_rating_grid <- function(...) {
  plots_list <- list(...)
  
  l <- get_legend(plots_list[[1]] + theme(
    legend.position = "top",
    legend.justification = "center"
  ))
  
  p <- plot_grid(
    l,
    plot_grid(plotlist = plots_list,
              nrow = 1),
    ncol = 1,
    rel_heights = c(.1, 1)
  )
  
  return(p)
}

plot_rating_ID_by_q <- function(model,
                                col,
                                label,
                                resp,
                                resp_label,
                                type = "traces",
                                measures = c("coup_relevance_s",
                                             "coup_attitude_s",
                                             "affect_s",
                                             "motivation_s"),
                                quadratic = c("coup_attitude_s")) {
  # Prepare values for group mean prediction ----
  plot_dat <- as.data.table(expand.grid(
    col = plot_seq(model$data[col]),
    questionId = unique(model$data$questionId)
  ))
  
  colnames(plot_dat) <- c(col, "questionId")
  
  plot_dat <- merge(plot_dat,
                    unique(as.data.table(model$data)[, .(questionId, block)]))
  
  
  # Set all additional predictors to median
  other_measures <- measures[measures != col]
  plot_dat[, (other_measures) := lapply(other_measures, function(x)
    median(model$data[x][[1]]))]
  
  # Compute quadratics
  if (!is.null(quadratic)) {
    lapply(quadratic, function(x) {
      plot_dat[, paste0("I(", x, ")^2") := .(get(x) ^ 2)]
    })
  }
  
  # Predict - this returns ndraws x npoints x 5
  pred_line <- fitted(
    model,
    newdata = plot_dat,
    re_formula = as.formula(paste0("~ (1 + ", col, " | questionId)")),
    summary = F,
    resp = resp,
    draw_ids = sample(1:4000, n_draws)
  )
  
  # Summarize into mean raiting
  pred_line <- sum_ordinal(pred_line)
  
  # Combine with predictors
  pred_line <- cbind(plot_dat, pred_line)
  
  # Prepare variables
  pred_line[, x := get(col)]
  
  # Add question text
  pred_line <-
    merge(pred_line, quest_text, by = "questionId", all.x = T)
  
  # Block labels
  pred_line[, block := factor(block,
                              levels = block_levels,
                              labels = block_labels)]
  
  if (type == "traces") {
    d <- highlight_key(pred_line, ~ questionId)
    p <- ggplot(d,
                aes(
                  x = x,
                  y = m,
                  group = questionId,
                  text = question,
                  color = block
                )) +
      geom_line(linewidth = 0.3) +
      facet_wrap("block") +
      labs(x = label,
           y = resp_label,) +
      scale_x_continuous(expand = expansion(mult = c(0, 0))) +
      scale_color_manual(breaks = block_labels,
                         values = block_colors) +
      scale_linetype_manual(breaks = block_labels,
                            values = block_lines) +
      theme(legend.position = "none")
    
    p <- ggplotly(p,
                  tooltip = "text") %>%
      highlight(on = "plotly_hover", off = "plotly_deselect")
    
    
  } else{
    which.extrema <- function(x)
      ifelse(which.min(x) %in% c(1, res),
             which.max(x),
             which.min(x))
    
    pred_line[, ext_ind := which.extrema(m), by = .(questionId, block)]
    
    rn <-
      unique(pred_line[, .(questionId, block, ext_ind)])[order(ext_ind)]
    
    rn[, rn := 1:.N, by = .(block)]
    
    pred_line <- merge(pred_line, rn[, .(questionId, rn)],
                       by = "questionId")
    
    pred_line[, m_s := scale(m), by = .(questionId)]
    
    d <- highlight_key(pred_line, ~ questionId)
    
    p <- ggplot(d, aes(
      x = x,
      y = rn,
      fill = m_s,
      text = question
    )) +
      geom_tile() +
      facet_wrap("block", ncol = 1, strip.position = "right") +
      scale_fill_gradient2(
        low = "#075AFF",
        mid = "#FFFFCC",
        high = "#FF0000"
      ) +
      labs(y = "Question",
           x = label,
           title = resp_label) +
      scale_x_continuous(expand = expansion(mult = c(0, 0))) +
      scale_y_continuous(expand = expansion(mult = c(0, 0))) +
      theme(
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5)
      )
  }
  
  
  # This might help with memory usage
  gc()
  
  return(p)
}