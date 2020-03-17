#' A parameterized interface to ggplot2
#' @description This function is for internal use only and very limited in scope.
#' Makes a ggplot2 plot based on a list of inputs, which may be gathered from a shiny application.
#' @param plotarguments A list of plot settings. Inspect code to see full list.
#' @param interactive A list of settings from interactive elements in the plot widget.
#' @return A ggplot2 object
#' @export
#' @importFrom dplyr filter
#' @importFrom dplyr summarize
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 expand_limits
# a is a list of arguments (plot settings)
custom_plot <- function(plotarguments, data, interactive){

  a <- plotarguments
  data <- as.data.frame(data)

  if(!is.null(a$factor_x) && a$factor_x){
    data[,a$xvar] <- as.factor(data[,a$xvar])
  }
  if(!is.null(a$factor_y) && a$factor_y) {
    data[,a$yvar] <- as.factor(data[,a$yvar])
  }

  apply_filter <- function(data, column, class, value){

    if(is_empty(column))return(data)
    if(is_empty(value[1]))return(data)

    if(class %in% c("Date","numeric")){

      data <- dplyr::filter(data, !!sym(column) >= value[1], !!sym(column) <= value[2])

    } else if(class %in% c("character","factor")){

      data <- dplyr::filter(data, !!sym(column) %in% value)
    }

  return(data)
  }

  # if all filters are NULL, JSON makes it an empty dataframe instead of a null list
  if((is.data.frame(a$filters) && ncol(a$filters) == 0) || is.null(a$filters)){
    # do nothing
  } else {

    # 1 filter wordt door toJSON verpest naar dataframe
    if(is.data.frame(a$filters) && nrow(a$filters) == 1){

      a$filters <- list(list(class = a$filters$class[[1]],
                        column = a$filters$column[[1]],
                        value = a$filters$value[[1]])
      )
    }

    for(i in seq_along(a$filters)){
      f <- a$filters[[i]]

      if(length(f$value) > 0 && !is.null(f$class)){
        data <- apply_filter(data, f$column, f$class, f$value)
      }

    }


  }

  # #
  interactive_filter <- function(data, interactive, i){

    varname <- a$interactive[[paste0("variable",i)]]
    vals <- interactive[[i]]
    elname <- a$interactive[[paste0("element",i)]]

    if(!is.null(varname) && !is_empty(vals)){

      if(elname == "sliderInput"){
        data <- filter(data, !!sym(varname) >= vals[1],
                          !!sym(varname) <= vals[2])
      }
      if(elname == "selectInput"){
        data <- filter(data, !!sym(varname) %in% vals)
      }
      if(elname == "dateRangeInput"){
        data <- filter(data, !!sym(varname) >= vals[1],
                          !!sym(varname) <= vals[2])
      }

    }
  return(data)

  }

  data <- interactive_filter(data, interactive, 1)
  data <- interactive_filter(data, interactive, 2)

  pal <- a$palette

  mytheme <- get(a$theme)

  a$xlab <- empty_string_to_null(a$xlab)
  a$ylab <- empty_string_to_null(a$ylab)
  a$glab <- empty_string_to_null(a$glab)

  if(!a$usegroup){
    a$groupvar <- NULL
  }

  if(a$plottype == "Scatter"){
    p <- scatter_plot(data,
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

    p <- grouped_barplot(data, a$xvar, a$yvar, a$groupvar,
                         statfun=a$statfun,
                         xlab=a$xlab,  ylab=a$ylab, glab=a$glab,
                         position = ifelse(a$bar_position == "Stacked", "stacked","grouped"),
                         annotate = a$annotate_bars)

  }

  if(a$plottype == "Pie chart"){

    p <- piechart(data,
                  a$xvar,
                  a$yvar,
                  xlab = a$xlab,
                  ylab = a$ylab,
                  glab = a$glab,
                  type = a$pietype, na.rm=a$pienarm)
  }

  if(is_empty(a$groupvar) & a$plottype == "Scatter"){
    pal <- rep(pal[1], nrow(data))
  }

  # Colors, themes, text formatting
  p <- p +
    ggplot2::scale_fill_manual(values = rep(pal,100)) +
    ggplot2::scale_color_manual(values = rep(pal,100)) +
    mytheme(base_size = a$labelsize) +
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

  if(a$includezerox){
    p <- p + ggplot2::expand_limits(x=0)
  }

  if(a$includezeroy){
    p <- p + ggplot2::expand_limits(y=0)
  }

  if(!is.null(a$title)){
    p <- p + ggplot2::labs(title = a$title)
  }

  if(!is.null(a$subtitle)){
    p <- p + ggplot2::labs(subtitle = a$subtitle)
  }

  if(!is.null(a$annotation_type)){


    annotation <- switch(a$annotation_type,

                         "Horizontal line" = geom_hline(yintercept = a$line_coordinate, color = a$line_colour),
                         "Vertical line" = geom_vline(xintercept = a$line_coordinate, color = a$line_colour)

                         )

    p <- p + annotation
  }


  print(p)
}



