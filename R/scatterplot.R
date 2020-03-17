
scatter_plot <- function(data,
                         xvar,
                         yvar,
                         groupvar = NULL,
                         xlab = NULL,
                         ylab = NULL,
                         glab = NULL,
                         shape=NULL,
                         colour_=NULL){


  data <- as.data.frame(data)

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

