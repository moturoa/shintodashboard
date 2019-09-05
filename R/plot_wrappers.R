

# wrapper functions:
# custom_plot --> calls scatter_plot or grouped_barplot
# both of those are wrappers around ggplot

# a is a list of arguments (plot settings)
custom_plot <- function(a){
  
  dataset <- get(a$dataset)
  
  if(a$factor_x){
    dataset[,a$xvar] <- as.factor(dataset[,a$xvar])
  }
  if(a$factor_y){
    dataset[,a$yvar] <- as.factor(dataset[,a$yvar])
  }
  
  apply_filter <- function(data, column, f){
    
    if(is_empty(column))return(data)
    if(is.numeric(data[,column])){
      if(is.null(f[[1]]) | is.null(f[[2]]))return(data)
      data <- filter(data, !!sym(column) >= f[[1]], !!sym(column) <= f[[2]])
    } else {
      if(is.null(f[[3]]))return(data)
      data <- filter(data, !!sym(column) %in% f[[3]])
    }
  
  return(data)  
  }
  
  dataset <- apply_filter(dataset, a$xvar, a$filters[1:3]) %>% 
    apply_filter(a$yvar, a$filters[4:6]) %>%
    apply_filter(a$groupvar, a$filters[7:9])
  
  pal <- a$palette
  
  theme <- get(a$theme)
  
  make_null <- function(x){
    if(x == "")x <- NULL
    x
  }
  
  a$xlab <- make_null(a$xlab)
  a$ylab <- make_null(a$ylab)
  a$glab <- make_null(a$glab)
  
  if(!a$usegroup){
    a$groupvar <- NULL
  }
  if(a$plottype == "Scatter"){
    p <- scatter_plot(dataset, 
                      a$xvar, 
                      a$yvar, 
                      a$groupvar, 
                      xlab=a$xlab, 
                      ylab=a$ylab, 
                      glab=a$glab,
                      shape = a$shape,
                      colour_ = pal[1])
    
  }
  if(a$plottype == "Barplot"){
    p <- grouped_barplot(dataset, a$xvar, a$yvar, a$groupvar, 
                         statfun=a$statfun,
                         xlab=a$xlab, 
                         ylab=a$ylab, glab=a$glab)
    
  }
  if(a$plottype == "Stacked barplot"){
    p <- grouped_barplot(dataset, a$xvar, a$yvar, a$groupvar, 
                         statfun=a$statfun,
                         xlab=a$xlab, ylab=a$ylab, glab=a$glab,
                         position = "stacked")
    
  }
  
  if(is_empty(a$groupvar) & a$plottype == "Scatter"){
    pal <- rep(pal[1], nrow(dataset))
  }
  p <- p + scale_fill_manual(values = rep(pal,100)) + 
    scale_color_manual(values = rep(pal,100))
  
  p <- p + 
    theme(base_size = a$labelsize) +
    ggplot2::theme(axis.title.x = element_text(margin = margin(t = a$labelmargin, r=0,b=0,l=0)),
                   axis.title.y = element_text(margin = margin(t = 0, r=a$labelmargin, b=0,l=0)),
                   axis.text.x=element_text(angle = as.numeric(a$labelanglex)),
                   axis.text.y=element_text(angle = as.numeric(a$labelangley)))
  
  if(a$nolabelsx){
    p <- p + ggplot2::theme(axis.text.x = element_blank())
  }
  
  if(a$nolegend){
    p <- p + ggplot2::theme(legend.position = "none")
  }
  
  
  # p <- ggplotly(p) %>%
  #   config(displaylogo = FALSE, 
  #          modeBarButtonsToRemove = c(
  #            'sendDataToCloud', 'autoScale2d', 'resetScale2d', 'toggleSpikelines',
  #            'hoverClosestCartesian', 'hoverCompareCartesian',
  #            'zoom2d','pan2d','select2d','lasso2d','zoomIn2d','zoomOut2d'
  #          ))
  
  print(p)
}



scatter_plot <- function(data, 
                         xvar, 
                         yvar, 
                         groupvar = NULL,
                         xlab = NULL,
                         ylab = NULL,
                         glab = NULL,
                         shape=NULL,
                         colour_=NULL){
  
  data$x <- data[,xvar]
  data$y <- data[,yvar]
  
  if(!is.null(groupvar)){
    data$g <- data[,groupvar]
    if(!is.factor(data$g))data$g <- as.factor(data$g)
  }
  
  if(is.null(xlab))xlab <- xvar
  if(is.null(ylab))ylab <- yvar
  if(is.null(glab))glab <- groupvar
  
  if(is.null(shape)){
    shape_ <- 19
  } else {
    shape_ <- switch(shape, 
                    circles = 19,
                    squares = 15
                    )
  }
  
  
  if(!is.null(groupvar)){
    ggplot(data, aes(x = x, y = y, col = g)) +
      geom_point(shape = shape_) + 
      labs(x = xlab, y = ylab, color = glab)
  } else {
    ggplot(data, aes(x = x, y = y)) +
      geom_point(shape = shape_, colour = colour_) + 
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
  if(!is.factor(data$x))data$x <- as.factor(data$x)

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
    
    ggplot(data_agg, aes(x = x, y = y, fill = x)) +
      geom_bar(stat="identity", position = position()) + 
      labs(x = xlab, y = ylab, fill = glab) 
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
