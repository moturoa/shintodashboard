

library(jsonlite)

l <- list(
  data = "automobiles",
  plotfunction = "scatter_plot",
  xvar = "fuel_efficiency",
  yvar = "cylinders"
)

toJSON(l)



reconstruct_plot <- function(l){
  
  data <- get(l$data)
  plot_fun <- get(l$plotfunction)
  
  do.call(l$plotfunction, c(list(data=data), l[c("xvar","yvar")]))
  
}

