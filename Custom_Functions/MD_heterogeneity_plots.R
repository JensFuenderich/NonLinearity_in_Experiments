### custom function for plots in the manuscript 
### check "Analyses_Manuscrip.qmd" for examples 
### simulated data: Fig. 2, Fig. 3 -> use the individual data inputs (Data_Collection_Site, CM, TM)
### brms object: Fig. 7, Fig. 8 --> use the brms objects input (brms_object)

MD_heterogeneity_plots <- function(
    Data_Collection_Site = NULL, 
    CM = NULL, 
    TM = NULL, 
    brms_object = NULL, 
    return_plot_list = FALSE, 
    plot_1_title = "a)", 
    plot_2_title = "b)"){
  
  if(is.null(brms_object)) {
    
    ## create plot df 
    plot_data <- data.frame(
      Data_Collection_Site = Data_Collection_Site, 
      CM = CM, 
      TM = TM
    )
    
  } else {
    
    ## export random effects data 
    randef <- as.data.frame(brms::ranef(brms_object))
    
    ## export fixed effect data 
    fixedef <- summary(brms_object)
    fixedef <- fixedef$fixed
    
    ## create plot df 
    plot_data <- data.frame(
      Data_Collection_Site = labels(randef)[[1]], 
      CM = (fixedef["Intercept","Estimate"] + randef$Data_Collection_Site.Estimate.Intercept), 
      TM = (fixedef["Intercept","Estimate"] + randef$Data_Collection_Site.Estimate.Intercept) + (fixedef["Group","Estimate"] + randef$Data_Collection_Site.Estimate.Group)
    )
    
  }
  
  ## add MD 
  plot_data$MD <- plot_data$TM - plot_data$CM
  
  ## reorder df according to size of MD 
  plot_data <- plot_data[order(plot_data$MD),]
  
  ## turn Data_Collection_Site into factor 
  plot_data$Data_Collection_Site <- as.character(plot_data$Data_Collection_Site)
  plot_data$Data_Collection_Site <- factor(plot_data$Data_Collection_Site, 
                                           levels = unique(plot_data$Data_Collection_Site))
  
  ## create factor for negative effects 
  plot_data$negative_effects <- as.factor(ifelse(plot_data$MD > 0, 1, 0))
  
  # ## remove NA
  # plot_data <- na.omit(plot_data)
  
  ## create plot_1 df 
  plot_1_data <- data.frame(
    Y = c(plot_data$CM, plot_data$TM), 
    X = rep(c(0,1), each = length(plot_data$CM)), 
    Data_Collection_Site = rep(plot_data$Data_Collection_Site, times = 2)
  )
  
  ## create plot 1
  plot_1 <- ggplot(aes(x = X, 
                       y = Y, 
                       group = Data_Collection_Site), 
                   data = plot_1_data) +
    geom_point() + # create layer with group mean dots 
    geom_line() + # create layer with MD lines 
    ggtitle(plot_1_title) +
    xlab(label = "control & treatment group") +
    ylab(label = "dependent variable") + 
    theme_minimal() + 
    theme(legend.position = "none", 
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
  
  ## create plot_2 df 
  plot_2_data <- plot_data
  
  ## reorder df according to size of CM (high CM first) 
  plot_2_data <- plot_2_data[order(-plot_2_data$CM),]
  plot_2_data$Data_Collection_Site <- factor(plot_2_data$Data_Collection_Site, levels = plot_2_data$Data_Collection_Site)
  
  ## create plot 2
  plot_2 <- ggplot(aes(x = Data_Collection_Site, 
                       ymin = CM, 
                       ymax = TM, 
                       color = if(length(unique(negative_effects)) > 1) {negative_effects}else{}),
                   data = plot_2_data) + 
    geom_linerange() + 
    coord_flip() +
    ggtitle(plot_2_title) +
    xlab(label = "sample") +
    ylab(label = "dependent variable") + 
    theme_minimal() + 
    theme(legend.position = "none", 
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
  
  ## combine both plots
  if (return_plot_list == TRUE) {
    list(plot_1, plot_2)
  } else {
    gridExtra::grid.arrange(grobs = list(plot_1, plot_2), 
                            ncol = 2)
  }
  
}