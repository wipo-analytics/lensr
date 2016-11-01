#' (internal) Generate boolean links between author names for lens_authors
#' @description Generates the links between multiple authors using OR or AND for authors, crossref and pubmed.
#' @param author a character vector of author names
#' @param author_boolean OR or AND
#' @param author_type default, crossref or pubmed
#' @return a url
#' @export
#'
#' @examples \dontrun{lens_authors(auth, author_boolean = "AND", author_type = "crossref")}
author_link_ <- function(author, author_boolean = "NULL", author_type = "NULL") {
  authors <- c("author%3A%28", "crossref_author%3A%28", "pubmed_author%3A%28")
  names(authors) <- c("default", "crossref", "pubmed")
  orlink <- c("%29+%7C%7C+author%3A%28", "%29+%7C%7C+crossref_author%3A%28", "%29+%7C%7C+pubmed_author%3A%28")
  names(orlink) <- c("default", "crossref", "pubmed")
  andlink <- c("%29+%26%26+author%3A%28", "%29+%26%26+crossref_author%3A%28", "%29+%26%26+pubmed_author%3A%28")
  names(andlink) <- c("default", "crossref", "pubmed")
  start <- c("author%3A%28", "crossref_author%3A%28", "pubmed_author%3A%28")
  names(start) <- c("default", "crossref", "pubmed")
  end <- "%29"
  baseurl <- "https://www.lens.org/lens/search?q="
  if(author_boolean == "OR") {
    authors
    start
    end
    if(author_type == "default"){
      query <- stringr::str_c(author, collapse = orlink[[1]])
      query <- stringr::str_c(start[[1]], query, end)
    } else if(author_type == "crossref"){
      query <- stringr::str_c(author, collapse = orlink[[2]])
      query <- stringr::str_c(start[[2]], query, end)
    } else if(author_type == "pubmed"){
      query <- stringr::str_c(author, collapse = orlink[[3]])
      query <- stringr::str_c(start[[3]], query, end)
    } else if(author_type == "NULL"){
      query <- stringr::str_c(author, collapse = orlink[[1]])
      query <- stringr::str_c(start[[1]], query, end)
    }
  }
  if(author_boolean == "AND") {
    authors
    start
    end
    if(author_type == "default"){
      query <- stringr::str_c(author, collapse = andlink[[1]])
      query <- stringr::str_c(start[[1]], query, end)
    } else if(author_type == "crossref"){
      query <- stringr::str_c(author, collapse = andlink[[2]])
      query <- stringr::str_c(start[[2]], query, end)
    } else if(author_type == "pubmed"){
      query <- stringr::str_c(author, collapse = andlink[[3]])
      query <- stringr::str_c(start[[3]], query, end)
    } else if(author_type == "NULL"){
      query <- stringr::str_c(author, collapse = andlink[[1]])
      query <- stringr::str_c(start[[1]], query, end)
  }

  }
  query <- paste0(baseurl, query)
}