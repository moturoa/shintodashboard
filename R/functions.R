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

is_empty <- function(x){
  is.null(x) || x == "" || length(x) == 0
}

side_by_side <- function(...){
  
  mc <- list(...)
  lapply(mc, function(x){
    
    tags$div(style="display: inline-block;", x)  
    
  })
  
}


save_dashboard <- function(dashboard, name){
  
  out <- jsonlite::toJSON(dashboard, pretty = TRUE)
  
  name <- gsub("[[:space:]]", "_", name)
  fn_out <- file.path("cache", paste0(name, ".json"))
  writeLines(out, fn_out)
  
}

load_dashboard <- function(id){
  fn_out <- file.path("cache", paste0(id, ".json"))
  jsonlite::fromJSON(fn_out)
}

list_dashboards <- function(){
  
  fns <- dir("cache", pattern = "[.]json$")
  tools::file_path_sans_ext(fns)
}







