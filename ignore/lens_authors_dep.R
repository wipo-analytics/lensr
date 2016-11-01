#' @title Search the lens with author names
#' @description Generate urls to carry out a search of Lens patent documents using author names including crossref and pubmed names.
#' @param author A character vector containing one or more author name.
#' @param author_boolean Choose from "OR" or "AND"
#' @param author_type choose from authors (general), crossref or pubmed author name searches.
#'
#' @return a url
#' @export
#' @details The urls serve as inputs into ops_iterate and ops_parse (see ops_search)
#' @examples \dontrun{lens_authors("Venter Craig")}
#' @examples \dontrun{lens_authors("Venter Craig")}
lens_authors_dep <- function(author, author_boolean = "NULL", author_type = "NULL"){
  #author types can be added as a vector but note that the collapse for OR and AND also then needs to become a list of vectors respectively for the three categories.
  baseurl <- "https://www.lens.org/lens/search?q="
  author_length <- length(author)
  default <- "author%3A%28"
  end <- "%29"
  if(author_length == 1){
    default
    end
    query <- stringr::str_replace_all(author, " ", "+")
    query <- paste0(baseurl, default, query, end)
    }
  if(author_length > 1){
    query <- stringr::str_replace_all(author, " ", "+")
  }
  if(author_boolean == "OR"){
    default
    end
    query <- stringr::str_c(query, collapse = "%29+%7C%7C+author%3A%28")
    query <- stringr::str_c(default, query, end)
    query <- paste0(baseurl, query)
  }
  if(author_boolean == "AND"){
  default
  end
  query <- stringr::str_c(query, collapse = "%29+%26%26+author%3A%28")
  query <- stringr::str_c(default, query, end)
  query <- paste0(baseurl, query)
  }
  query
}