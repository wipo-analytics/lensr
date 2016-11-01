#' @title single author url by type
#' @description used internally to format urls for single author names for default, crossref or pubmed. Used in author_search
#' @param query a single author name (e.g. Kirk James or Kirk J)
#' @param author_type default, crossref or pubmed
#' @return a url
#' @export
#'
#' @examples \dontrun{lens_authors("Kirk James", author_type = "crossref")}
author_type_ <- function(query, author_type = "NULL"){
  baseurl <- "https://www.lens.org/lens/search?q="
  authors <- c("author%3A%28", "crossref_author%3A%28", "pubmed_author%3A%28")
  names(authors) <- c("default", "crossref", "pubmed")
  if(author_type == "default"){
    authors
    query <- stringr::str_c(baseurl, authors[[1]], query)
  } else if(author_type == "crossref"){
    query <- stringr::str_c(baseurl, authors[[2]], query)
  } else if(author_type == "pubmed"){
    query <- stringr::str_c(baseurl, authors[[3]], query)
  } else if (author_type == "NULL"){
    query <- stringr::str_c(baseurl, authors[[1]], query)
  }

}