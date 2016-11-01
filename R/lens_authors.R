#' Search using author names
#' @description search for author names in the text of patent documents using the Lens.
#' @param author a single author name or vector of author names (quoted)
#' @param author_boolean OR or AND
#' @param author_type default, crossref or pubmed
#' @return a url
#' @export
#' @importFrom stringr str_replace_all
#' @examples \dontrun{lens_authors("Venter Craig")}
lens_authors <- function(author, author_boolean = "NULL", author_type = "NULL"){
  baseurl <- "https://www.lens.org/lens/search?q="
  end <- "%29"
  author_length <- length(author)
  # end <- "%29"
  # authors <- c("author%3A%28", "crossref_author%3A%28", "pubmed_author%3A%28")
  # names(authors) <- c("default", "crossref", "pubmed")
  #load author type function
  if(author_length == 1){
    end
    query <- stringr::str_replace_all(author, " ", "+")
    query <- author_type_(query, author_type) # see author_type_ internal
    query <- paste0(query, end)
  }
  if(author_length > 1){
    query <- stringr::str_replace_all(author, " ", "+")
    query <- author_link_(query, author_boolean, author_type) #see author_link_ internal
  return(query)
  }
  query
  }