is_empty <- function(x){
  all(is.null(x) | as.character(x) == "" | length(x) == 0)
}


empty_string_to_null <- function(x){
  if(is.null(x)){
    return(x)
  }
  if(x == "")x <- NULL
  x
}
