test_inv <- function(inventor = "NULL", inventor_boolean = "NULL", query = "NULL", type = "NULL", boolean = "NULL"){
  if(!is.null(inventor) && !is.null(query)){
    andlink <- "+%26%26+"
    inv_query <- lens_inventor(inventor)
    query <- lens_urls(query, type, boolean)
    query <- stringr::str_split(query, "=", n = 2)
    query <- paste0(query[[1]][[2]])
    out <- stringr::str_c(inv_query, andlink, query, collapse = TRUE)
  }
  out
}


