# #' @importFrom waffle geom_waffle
# #' @importFrom waffle theme_enhance_waffle
piechart <- function(data,
                     xvar,
                     yvar,
                     xlab = NULL,
                     ylab = NULL,
                     glab = NULL,
                     type = "Pie",
                     na.rm=FALSE){

  data <- as.data.frame(data)

  data$xv <- data[,xvar]
  data$yv <- data[,yvar]
  if(na.rm){
    data <- dplyr::filter(data, !is.na(xv))
  }

  type <- match.arg(type)

  dat <- group_by(data, xv) %>%
    summarize(n = sum(yv, na.rm=TRUE))

  if(is.null(xlab))xlab <- xvar
  if(is.null(ylab))ylab <- ""
  if(is.null(glab))glab <- ""

  if(type == "Pie"){

    ggplot(dat, aes(fill = xv, y = n, x = "")) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start=0) +
      theme_minimal() +
      labs(x = xlab, y = ylab, fill = glab)

  }

  # else if(type == "Waffle"){
  #
  #   ggplot(dat, aes(fill = xv, values = n, x = "")) +
  #     geom_waffle(n_rows = 25, size = 0.33, colour = "white", flip = FALSE) +
  #     theme_minimal() +
  #     labs(x = xlab, y = ylab)
  #   #   theme_enhance_waffle() +
  #
  # }

}
