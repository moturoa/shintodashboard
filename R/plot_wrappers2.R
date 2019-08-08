


scatter_plot <- function(data, 
                         xvar, 
                         yvar, 
                         groupvar = NULL,
                         xlab = NULL,
                         ylab = NULL,
                         glab = NULL){
  
  data$x <- data[,xvar]
  data$y <- data[,yvar]
  
  if(!is.null(groupvar)){
    data$g <- data[,groupvar]
    if(!is.factor(data$g))data$g <- as.factor(data$g)
  }
  
  if(is.null(xlab))xlab <- xvar
  if(is.null(ylab))ylab <- yvar
  if(is.null(glab))glab <- groupvar
  
  
  if(!is.null(groupvar)){
    ggplot(data, aes(x = x, y = y, col = g)) +
      geom_point() + 
      theme_minimal() +
      labs(x = xlab, y = ylab, color = glab)
  } else {
    ggplot(data, aes(x = x, y = y)) +
      geom_point() + 
      theme_minimal() +
      labs(x = xlab, y = ylab)
  }
    
}



grouped_barplot <- function(data, 
                         xvar, 
                         yvar, 
                         statfun = "count",
                         groupvar = NULL,
                         position = c("grouped", "stacked", "filled"),
                         xlab = NULL,
                         ylab = NULL,
                         glab = NULL){
  
  data$x <- data[,xvar]
  data$y <- data[,yvar]
  
  position <- match.arg(position)
  position <- switch(position,
                     grouped = position_dodge,
                     stacked = position_stack,
                     fill = position_fill)
  
  if(!is.null(groupvar)){
    data$g <- data[,groupvar]
    if(!is.factor(data$g))data$g <- as.factor(data$g)
    data <- data[complete.cases(data[,c("x","y","g")]),]
  } else {
    data <- data[complete.cases(data[,c("x","y")]),]
  }

  if(is.null(xlab))xlab <- xvar
  if(is.null(ylab))ylab <- glue("{statfun}({yvar})")
  if(is.null(glab))glab <- groupvar
  
  stat_fun <- get(statfun)
  
  
  if(is.null(groupvar)){
    
    if(statfun != "count"){
      data_agg <- group_by(data, x, .drop = FALSE) %>%
        summarize(y = stat_fun(y))
    } else {
      data_agg <- group_by(data, x, .drop = FALSE) %>%
        summarize(y = n())
    }
    
    ggplot(data_agg, aes(x = x, y = y)) +
      geom_bar(stat="identity", position = position()) + 
      theme_minimal() +
      labs(x = xlab, y = ylab) 
    } else {
        
      
    if(statfun != "count"){
      data_agg <- group_by(data, x, g, .drop = FALSE) %>%
        summarize(y = stat_fun(y))
    } else {
      data_agg <- group_by(data, x, g, .drop = FALSE) %>%
        summarize(y = n())
    }
      
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
  scatter_plot(automobiles, "engine_volume", "fuel_efficiency")
  
  grouped_barplot(data, "cylinders", "fuel_efficiency", statfun="mean")
  grouped_barplot(data, "cylinders", "fuel_efficiency", "origin", statfun="mean")
  
  
  grouped_barplot(data, "cylinders", "fuel_efficiency", "origin",statfun="n",
                  position = "stacked")
  
  
  group_by(data, cylinders, origin) %>%
    summarize(y = mean(fuel_efficiency, na.rm=T))
  
}
