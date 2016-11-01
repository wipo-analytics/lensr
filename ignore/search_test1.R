search_test1 <- function(inventor, inventor_boolean = "NULL", query = "NULL", type = "NULL", boolean = "NULL"){
  andlink <- "+%26%26+"
if(!is.null(inventor) & !is.null(query)){
  andlink
  inv_query <- lens_inventor(inventor)
  query <- lens_urls(query, type, boolean)
  query <- paste0(inv_query, query)
}
#   query <- stringr::str_split(query, "=", n = 2)
#   query <- paste0(query[[1]][[2]])
#   out <- stringr::str_c(inv_query, andlink, query, collapse = TRUE)
# }
 inv_query
 query
}
