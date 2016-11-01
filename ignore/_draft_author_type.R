author_type_ <- function(query, author_type = "NULL"){
  baseurl <- "https://www.lens.org/lens/search?q="
  authors <- c("author%3A%28", "crossref_author%3A%28", "pubmed_author%3A%28")
  names(authors) <- c("default", "crossref", "pubmed")
  if(author_type == "default"){
  authors
  query <- stringr::str_c(baseurl, authors[[1]], query)
  } else if(author_type == "crossref"){
    query <- paste0(baseurl, authors[[2]], query)
  } else if(author_type == "pubmed"){
    query <- paste0(baseurl, authors[[3]], query, end)
  }
  return(query)
}

# #joining code for AND or OR
# author_link_ <- function(x){
#   #author_type_ provides inputs to this function as authors[[1]] etc.
# or <- c("%29+%7C%7C+author%3A%28", "%29+%7C%7C+crossref_author%3A%28", "%29+%7C%7C+pubmed_author%3A%28")
# or(names) <- c("default", "crossref", "pubmed")
# if(author_boolean == "OR"){
#   authors
#   end
#   if(author_type = "default"){
#     query <- stringr::str_c(query, collapse = or[[1]])
#     query <- stringr::str_c(authors[[1]], query, end)
#     query <- paste0(baseurl, query)
#   } else if(author_type =="crossref"){
#     query <- stringr::str_c(query, collapse = or[[2]])
#     query <- stringr::str_c(authors[[2]], query, end)
#     query <- paste0(baseurl, query)
#   } else if(author_type =="pubmed"){
#     query <- stringr::str_c(query, collapse = or[[3]])
#     query <- stringr::str_c(authors[[2]], query, end)
#     query <- paste0(baseurl, query)
#   }
# }
# query
# }
#
