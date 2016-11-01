#' @title Search the Lens using inventor names
#' @description Search the lens using one or more inventor names
#' @param inventor an inventor name (e.g. Kirk James) or vector of inventor names (quoted).
#' @param inventor_boolean Either AND or OR (default) for use with multiple inventor names (quoted).
#' @return a url
#' @export
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_c
#' @examples \dontrun{(lens_inventor("Venter Craig"))}
#' @examples \dontrun{lens_inventor(three_authors)}
lens_inventor <- function(inventor = "NULL", inventor_boolean = "NULL"){
  baseurl <- "https://www.lens.org/lens/search?q="
  start <- "inventor%3A%22"
  end <- "%22~2"
  orlink <- "%22~2+%7C%7C+inventor%3A%22"
  andlink <- "%22~2+%26%26+inventor%3A%22"
  inventor_length <- length(inventor)
  if(inventor_length == 1){
    start
    end
    query <- stringr::str_replace_all(inventor, " ", "+")
    query <- stringr::str_c(baseurl, start, query, end)
  }
  if(inventor_length > 1){
    query <- stringr::str_replace_all(inventor, " ", "+")
  }
  if(inventor_boolean == "OR"){
    start
    orlink
    end
    query <- stringr::str_c(query, collapse = orlink)
    query <- stringr::str_c(query, end)
    query <- paste0(baseurl, start, query)
  }
  if(inventor_boolean == "AND"){
    start
    andlink
    end
    query <- stringr::str_c(query, collapse = andlink)
    query <- stringr::str_c(query, end)
    query <- paste0(baseurl, start, query)
  }
  if(inventor_length > 1 & inventor_boolean == "NULL"){ # defaults to OR
    start
    orlink
    end
    query <- stringr::str_c(query, collapse = orlink)
    query <- stringr::str_c(query, end)
    query <- paste0(baseurl, start, query)
    # warning("boolean defaulting to 'OR'")
  }
  query
}