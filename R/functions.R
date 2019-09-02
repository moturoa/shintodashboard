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
  is.null(x) || x == ""
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


save_dashboard <- function(dashboard, name){
  
  fn <- "cache/dashboards.rds"
  if(file.exists(fn)){
    dash <- readRDS(fn)
    dash[[name]] <- dashboard
  } else {
    dash <- list(dashboard)
    names(dash) <- name
  }
  
  saveRDS(dash, fn)
  
}

load_dashboard <- function(id){
  readRDS("cache/dashboards.rds")[[id]]
}

list_dashboards <- function(){
  fn <- "cache/dashboards.rds"
  if(!file.exists(fn))return("")
  names(readRDS("cache/dashboards.rds"))
}







