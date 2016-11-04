cond <- function(inventor = NULL, query = NULL){
  if(!is.null(inventor) && !is.null(query)){
  out <- "one"
  } else if(is.null(inventor) && !is.null(query)){
  out <- "two"
  }
  print(out)
}