reorder_within <- function(fac, fac_within, level_within, decreasing=TRUE){
  tab <- table(fac_within, fac)
  props <- prop.table(tab, margin = 2)
  collist <- names(sort(props[level_within,], decreasing=decreasing))
  factor(fac,levels=collist)
}

random_word <- function(n = 6){
  
  x <- c(letters, LETTERS)
  paste(sample(x,n), collapse="")
  
}

custom_plot <- function(plot_arguments){
  
  a <- plot_arguments
  
  
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
    p <- scatter_plot(automobiles, 
                      a$xvar, 
                      a$yvar, 
                      a$groupvar, 
                      xlab=a$xlab, 
                      ylab=a$ylab, 
                      glab=a$glab)
    print(p)
  }
  if(a$plottype == "Barplot"){
    p <- grouped_barplot(automobiles, a$xvar, a$yvar, a$groupvar, 
                         statfun=a$statfun,
                         xlab=a$xlab, 
                         ylab=a$ylab, glab=a$glab)
    print(p)
  }
  if(a$plottype == "Stacked barplot"){
    p <- grouped_barplot(automobiles, a$xvar, a$yvar, a$groupvar, 
                         statfun=a$statfun,
                         xlab=a$xlab, ylab=a$ylab, glab=a$glab,
                         position = "stacked")
    print(p)
  }
  
  
}
