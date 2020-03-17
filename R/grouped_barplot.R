
#' @importFrom dplyr n
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 geom_point
#' @importFrom stats complete.cases
#' @importFrom ggplot2 position_dodge
#' @importFrom ggplot2 position_stack
#' @importFrom ggplot2 position_fill
#' @export
grouped_barplot <- function(data,
                            xvar,
                            yvar,
                            statfun = "count",
                            groupvar = NULL,
                            position = c("grouped", "stacked", "filled"),
                            xlab = NULL,
                            ylab = NULL,
                            glab = NULL,
                            annotate = FALSE){

  data <- as.data.frame(data)

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

  #if(!is.factor(data$x))data$x <- as.factor(data$x)

  if(is.null(xlab))xlab <- xvar
  if(is.null(ylab))ylab <- glue("{statfun}({yvar})")
  if(is.null(glab))glab <- groupvar

  if(statfun == "count")statfun <- "dplyr::n"
  stat_fun <- get(statfun)


  # Barplot without grouping
  if(is.null(groupvar)){

    data_agg <- group_by(data, x, .drop = FALSE) %>%
        summarize(y = stat_fun(y))

    p <- ggplot(data_agg, aes(x = x, y = y, fill = as.factor(x))) +
      geom_bar(stat="identity", position = position()) +
      labs(x = xlab, y = ylab, fill = glab)

    if(!is.null(annotate) && annotate){

      y_nudge <-  0.02 * max(data_agg$y)

      p <- p + geom_text(data = data_agg, aes(label=y), colour= "white",
                         fontface = "bold",  vjust = 1, nudge_y = -y_nudge)

    }

  # Barplot with grouping
  } else {

    data_agg <- group_by(data, x, g, .drop = FALSE) %>%
      summarize(y = stat_fun(y))

    p <- ggplot(data_agg, aes(x = x, y = y, fill = g)) +
      geom_bar(stat="identity", position = position()) +
      labs(x = xlab, y = ylab, fill = glab)

    if(!is.null(annotate) && annotate){

      totals <- group_by(data, x, .drop = FALSE) %>%
        summarize(y = stat_fun(y)) %>%
        mutate(g = NA)

      safemax <- function(x){
        x <- x[!is.na(x)]
        if(length(x)){
          max(x)
        } else {
          0
        }
      }
      y_nudge <-  0.02 * safemax(totals$y)

      p <- p + geom_text(aes(label=y), data=totals, vjust=1, colour= "white", fontface = "bold", nudge_y = -y_nudge)

    }


  }




return(p)
}

