### custom function for plots in the manuscript 
### check "Analyses_Manuscrip.qmd" for examples 
### simulated data: Fig. 2, 3, 5 and 6 -> use the individual data inputs (Data_Collection_Site, CM, TM)
### brms object: Fig. 7 and 8 --> use the brms objects input (brms_object)

## MD_heterogeneity_plot_2
######

MD_heterogeneity_plot_2 <- function(
    Data_Collection_Site = NULL, 
    CM = NULL, 
    TM = NULL, 
    brms_object = NULL, 
    return_plot_list = FALSE, 
    DV_lim_lower = NULL, 
    DV_lim_upper = NULL, 
    label_short = FALSE, 
    background_color = "white", 
    y_lab_text_short = "DV"){
  
  if (label_short == FALSE) {
    label_DV <- "dependent variable"
    label_group <- "control & treatment group"
  } else if (label_short == TRUE) {
    label_DV <- y_lab_text_short
    label_group <- "group"
  }
  
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
  
  ## create plot_2 df 
  plot_2_data <- plot_data
  
  ## reorder df according to size of CM (high CM first) 
  plot_2_data <- plot_2_data[order(-plot_2_data$CM),]
  plot_2_data$Data_Collection_Site <- factor(plot_2_data$Data_Collection_Site, levels = plot_2_data$Data_Collection_Site)
  
  
  if (is.null(DV_lim_lower) & is.null(DV_lim_upper)) {
    
    ## create plot 2
    plot_2 <- ggplot(aes(x = Data_Collection_Site, 
                         ymin = CM, 
                         ymax = TM, 
                         color = if(0 %in% negative_effects) {negative_effects}else{}),
                     data = plot_2_data) + 
      geom_linerange() + 
      coord_flip() +
      xlab(label = "replication") +
      ylab(label = label_DV) + 
      theme_minimal() + 
      theme(legend.position = "none", 
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(), 
            plot.background = element_rect(fill = background_color, color = "#CCCCCC00"), 
            panel.grid.major = element_line(color = "#AAAAAA70"), 
            panel.grid.minor = element_line(color = "#AAAAAA00"), 
            panel.grid.major.y = element_blank())
    
  } else {
    
    ## create plot 2
    plot_2 <- ggplot(aes(x = Data_Collection_Site, 
                         ymin = CM, 
                         ymax = TM, 
                         color = if(0 %in% negative_effects) {negative_effects}else{}),
                     data = plot_2_data) + 
      geom_linerange() + 
      ylim(c(DV_lim_lower, DV_lim_upper)) +
      coord_flip() +
      xlab(label = "replication") +
      ylab(label = label_DV) + 
      theme_minimal() + 
      theme(legend.position = "none", 
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(), 
            plot.background = element_rect(fill = background_color, color = "#CCCCCC00"), 
            panel.grid.major = element_line(color = "#AAAAAA70"),
            panel.grid.minor = element_line(color = "#AAAAAA00"), 
            panel.grid.major.y = element_blank()) 
    
  }
  
  ## output
  if (return_plot_list == TRUE) {
    list(plot_2)
  } else {
    plot_2
  }
  
}



#####