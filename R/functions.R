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

null_to_empty <- function(x){
  if(is.null(x)) {
    ""
  } else {
    x
  }
}

is_empty <- function(x){
  is.null(x) || x == "" || length(x) == 0
}

side_by_side <- function(..., vertical_align = FALSE){
  
  css <- "display: inline-block;"
  if(vertical_align)css <- paste(css, "vertical-align: top;")
  
  mc <- list(...)
  lapply(mc, function(x){
    
    tags$div(style= css, x)  
    
  })
  
}


float_right <- function(...){
  tags$div(style = "float: right;", ...)
}

save_dashboard <- function(dashboard, name){
  
  out <- jsonlite::toJSON(dashboard, pretty = TRUE)
  
  name <- gsub("[[:space:]]", "_", name)
  fn_out <- file.path("cache", paste0(name, ".json"))
  writeLines(out, fn_out)
  
}

load_dashboard <- function(id){
  fn_out <- file.path("cache", paste0(id, ".json"))
  jsonlite::fromJSON(fn_out, simplifyDataFrame = FALSE)
}

list_dashboards <- function(){
  
  fns <- dir("cache", pattern = "[.]json$")
  tools::file_path_sans_ext(fns)
}


load_palette <- function(pal){
  
  if(pal %in% rownames(brewer.pal.info)){
    brewer.pal(8, pal)
  } else if(pal == "rich.colors"){
    gplots::rich.colors(12)
  } else {
    fn <- file.path("cache/palettes", paste0(pal, ".json"))
    fromJSON(fn)
  }
  
  
}




