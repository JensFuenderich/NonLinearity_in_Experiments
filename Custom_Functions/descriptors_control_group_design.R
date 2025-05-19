### custom function for plots in the manuscript 
### check "Analyses_Manuscrip.qmd" for its application  

descriptors_control_group_design <- function(name, brms_data, background_color, two_stage_MA_CM, two_stage_MA_MD){
  
  ## evaluation of CM & ES heterogeneity 
  CM_het <- ifelse(
    two_stage_MA_CM[[name]]$QEp <= 0.05, 
    yes = "tau[CM] > 0", 
    no = "tau[CM]~`=`~0")
  
  MD_het <- ifelse(
    two_stage_MA_MD[[name]]$QEp <= 0.05, 
    yes = "tau[MD] > 0", 
    no = "tau[MD]~`=`~0")
  
  ## create summary file (for easy access)
  summary_file <- summary(brms_data[[name]])$random$Data_Collection_Site
  
  ## remove 0 before decimal point (for p-values)
  remove_zero <- function(x){
    sub("^(-?)0.", "\\1.", sprintf("%.2f", x)) 
  }
  
  ## identify if the interval includes the null 
  non_zero_ISC <- ifelse(sign(
    summary_file["cor(Intercept,Group)", "l-95% CI"]) == 
      sign(summary_file["cor(Intercept,Group)", "u-95% CI"]), 
    yes = "yes",
    no = "no")
  
  ## create ISC character vector
  ISC <- paste("ISC = ", 
        remove_zero(unlist(
          round(
            summary_file["cor(Intercept,Group)", "Estimate"],
            digits = 2)
        )), 
        "(",
        remove_zero(unlist(
          round(
            summary_file["cor(Intercept,Group)", "l-95% CI"],
            digits = 2)
        )), 
        ",",
        remove_zero(unlist(
          round(
            summary_file["cor(Intercept,Group)", "u-95% CI"],
            digits = 2)
        )), 
        ")",
        sep = "")
  
  
  ## store posterior draws as df
  posterior_draws <- as.data.frame(brms_data[[name]]$fit)
  
  ## calculate spearman correlation (ranks) between MD and pooled SD 
  r_spearman <- confintr::ci_cor(
    x = mean(posterior_draws$b_Intercept) + 
      colMeans(posterior_draws[,grep(pattern = ",Intercept]", x = names(posterior_draws))]),
    y = mean(posterior_draws$b_Intercept) + 
      colMeans(posterior_draws[,grep(pattern = ",Intercept]", x = names(posterior_draws))]) + 
      mean(posterior_draws$b_Group) + 
      colMeans(posterior_draws[,grep(pattern = ",Group]", x = names(posterior_draws))]),
    method = "spearman", 
    probs = c(0.025, 0.975), 
    type = "bootstrap"
  )
  
  ## identify if the interval includes the null 
  non_zero_rank <- ifelse(
    sign(r_spearman$interval[1]) == sign(r_spearman$interval[2]), 
    yes = "yes", 
    no = "no")
  
  ## create rank correlation character vector
  r_rank <- paste(
    ifelse(non_zero_rank == "yes", 
           yes = "paste(r[s], ' = ',", 
           no = "italic(paste(r[s], ' = ',"),
    "'",
    ifelse(
      round(r_spearman$estimate, digits = 2) == 1,
      yes = "1", 
      no = remove_zero(unlist(r_spearman$estimate))
    )
    , 
    "(",
    ifelse(
      round(r_spearman$estimate, digits = 2) == 1,
      yes = "1", 
      no = remove_zero(unlist(r_spearman$interval[1]))
    ), 
    ",",
    ifelse(
      round(r_spearman$estimate, digits = 2) == 1,
      yes = "1", 
      no = remove_zero(unlist(r_spearman$interval[2]))
    ), 
    ifelse(non_zero_rank == "yes", 
           yes = ")'", 
           no = ")')"),
    ")",
    sep = "")
  
  
  
  ## create plot with the results 
  ggplot() + 
    ggplot2::xlim(1,5) + 
    ggplot2::ylim(1,5) +
    theme_void() +
    annotate(
      "text", label = CM_het,
      x = 3, y = 4.5, size = 4, colour = "black", parse = TRUE, 
      fontface = 2
    ) +
    annotate(
      "text", label = MD_het,
      x = 3, y = 3.5, size = 4, colour = "black", parse = TRUE
    ) +
    annotate(
      "text", label = ISC,
      x = 3, y = 2.5, size = 4, colour = "black", 
      fontface = ifelse(non_zero_ISC == "yes", yes = 0, no = 3) 
    ) + 
    annotate(
      "text", label = r_rank,
      x = 3, y = 1.5, size = 4, colour = "black", parse = TRUE
    ) + 
    theme(
      plot.background = element_rect(fill = background_color, color = "#CCCCCC00")
    )
  
}