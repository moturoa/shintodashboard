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

side_by_side <- function(...){
  
  mc <- list(...)
  lapply(mc, function(x){
    
    tags$div(style="display: inline-block;", x)  
    
  })
  
}


side_by_side(
  checkboxInput("chk_colorbrewer", "Use", value = TRUE), 
  selectInput("select_palette", 
              "Color palette (Color Brewer) (>8 colors)",
              choices = rownames(brewer.pal.info), 
              selected = "Set3", width = "150px")
)

custom_plot <- function(a){
  
  dataset <- get(a$dataset)
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
  
  p <- tryCatch(p + scale_fill_manual(values = pal), error = p)
  p <- tryCatch(p + scale_color_manual(values = pal), error = p)

  p <- p + theme()
  
print(p)
}
