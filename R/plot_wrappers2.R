


scatter_plot <- function(data, 
                         xvar, 
                         yvar, 
                         groupvar = NULL,
                         xlab = NULL,
                         ylab = NULL,
                         glab = NULL){
  
  data$x <- data[,xvar]
  data$y <- data[,yvar]
  data$g <- data[,groupvar]
  if(!is.factor(data$g))data$g <- as.factor(data$g)
  
  if(is.null(xlab))xlab <- xvar
  if(is.null(ylab))ylab <- yvar
  if(is.null(glab))glab <- groupvar
  
  ggplot(data, aes(x = x, y = y, col = g)) +
    geom_point() + 
    theme_minimal() +
    labs(x = xlab, y = ylab, color = glab)
    
}



grouped_barplot <- function(data, 
                         xvar, 
                         yvar, 
                         statfun = "count",
                         groupvar = NULL,
                         position = c("grouped", "stacked"),
                         xlab = NULL,
                         ylab = NULL,
                         glab = NULL){
  
  data$x <- data[,xvar]
  data$y <- data[,yvar]
  
  position <- match.arg(position)
  position <- switch(position,
                     grouped = position_dodge,
                     stacked = position_fill)
  
  if(!is.null(groupvar)){
    data$g <- data[,groupvar]
    if(!is.factor(data$g))data$g <- as.factor(data$g)
    data <- data[complete.cases(data[,c("x","y","g")]),]
  } else {
    data <- data[complete.cases(data[,c("x","y")]),]
  }

  if(is.null(xlab))xlab <- xvar
  if(is.null(ylab))ylab <- yvar
  if(is.null(glab))glab <- groupvar
  
  stat_fun <- get(statfun)
  
  
  if(is.null(groupvar)){
    
    data_agg <- group_by(data, x) %>%
      summarize(y = stat_fun(y))
    
    ggplot(data_agg, aes(x = x, y = y, .drop = FALSE)) +
      geom_bar(stat="identity", position = position()) + 
      theme_minimal() +
      labs(x = xlab, y = ylab) 
    } else {
        
    data_agg <- group_by(data, x, g, .drop = FALSE) %>%
        summarize(y = stat_fun(y))
      
    ggplot(data_agg, aes(x = x, y = y, fill = g)) +
      geom_bar(stat="identity", position = position()) + 
      theme_minimal() +
      labs(x = xlab, y = ylab, fill = glab)
        
   }
  
}



if(FALSE){
  
  library(dplyr)
  library(ggplot2)
  
  library(lgrdata)
  data(automobiles)
  
  data <- automobiles
  
  group_var <- "cylinders"
  x_var <- "engine_volume"
  y_var <- "fuel_efficiency"
  
  
  scatter_plot(automobiles, "engine_volume", "fuel_efficiency", "cylinders")
  
  
  grouped_barplot(data, "cylinders", "fuel_efficiency", statfun="mean")
  grouped_barplot(data, "cylinders", "fuel_efficiency", "origin", statfun="mean")
  
  
  
  group_by(data, cylinders, origin) %>%
    summarize(y = mean(fuel_efficiency, na.rm=T))
  
}
