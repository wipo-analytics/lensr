#' @title Retrieve patent family counts from a query to the Lens
#' @description A short version lf lens_search that returns the results from a query to the Lens. Useful for working out the overall number of results from a query for refinement or download.
#' @param query One or more terms to search in the patent database (one or two words only at present)
#' @param boolean Select the type of boolean ("OR" or "AND") where using multiple search terms.
#' @param type Either fulltext (default), title, abstract, claims, or title,
#'   abstract and claims (tac). Quoted.
#' @param rank_citing Whether to sort the Lens results by the top citing
#'   (descending). Useful for retrieving important documents. See details.
#' @param rank_family Whether to sort the Lens results by the number of family
#'   members (descending). Useful for retrieving important documents. See
#'   details.
#' @param rank_sequences Rank results on whether the documents contain a dna or amino
#'   acid sequence. See details.
#' @param latest_publication Sort results by the latest publication date.
#'   Useful for retrieving the most recent documents. See details.
#' @param latest_filing Sort results by the latest filing (priority) date.
#'   Useful for identifying the latest filings (note that they are the latest
#'   filings that have a publication). See details.
#' @param earliest_publication Sort results by the earliest publication date.
#'   See details.
#' @param earliest_filing Sort results by the earliest publication date. See
#'   details.
#' @param families The function returns both the publication count and family count (default). If
#'   families = TRUE return the number of patent families will be returned (deduplicates a set of
#'   publications to the first publication of the root "priority" or first
#'   filing).
#' @return data.frame
#' @export
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes
#' @importFrom rvest html_text
#' @importFrom magrittr %>%
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_trim
#' @importFrom tibble tibble
#' @examples \dontrun{lens_count("drones")}
#' @examples \dontrun{lens_count("synthetic biology", type = "tac")}
#' @examples \dontrun{query <- "https://www.lens.org/lens/search?q=%22drones%22&n=50&f=true"}
#' @examples \dontrun{query <- "https://www.lens.org/lens/search?q=%22synthetic+biology%22&n=50&f=true"}
lens_count <- function(query, boolean = "NULL", type = "NULL", rank_family = "NULL", rank_citing = "NULL", rank_sequences = "NULL", latest_publication = "NULL", earliest_publication = "NULL", latest_filing = "NULL", earliest_filing = "NULL", families = TRUE){
  myquery <- lens_urls(query, boolean, type, rank_family, rank_citing, rank_sequences, latest_publication, earliest_publication, latest_filing, earliest_filing, families) # note it was lens search.
  html <- xml2::read_html(myquery)
  publications <- rvest::html_nodes(html, ".resultCount") %>%
    rvest::html_text() %>%
    stringr::str_extract_all("[[:digit:]]+") %>%
    as.numeric()
  search <- rvest::html_nodes(html, "#previousSearchText") %>%
    rvest::html_text() %>%
    as.character()
  if(families == TRUE){ #swapped round from FALSE to TRUE
    families <- rvest::html_nodes(html, ".breadnum:nth-child(4)") %>%
      rvest::html_text() %>%
      stringr::str_extract_all("[[:digit:]]+") %>%
      as.numeric()
  }
  if(families == FALSE){ #swapped FALSE to TRUE. Is returning publication numbers as families.
    families <- rvest::html_nodes(html, ".resultCount") %>%
      rvest::html_text() %>%
      stringr::str_extract_all("[[:digit:]]+") %>%
      as.numeric()
  }
  df <- tibble::tibble(publications, families, search) %>%
    print()

}
