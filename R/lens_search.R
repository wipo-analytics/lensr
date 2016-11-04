#' @title Search the Lens Patent Database
#' @description This function allows for the construction of complex queries to search and retrieve data from the Lens. The default search groups documents by family and will return up to 50 results per page. The maximum number of results that can be retrieved is 500 (10 pages). For larger results sets use the free Lens online Collection facility to download upto 10,000 records. See details for information on the use of ranking and date measures to sort the data.
#' @param query One or more terms to search in the patent database.
#' @param boolean Select the type of boolean ("OR" or "AND") where using multiple search terms.
#' @param type Either fulltext (default), title, abstract, claims, or "tac" for 'title or abstract or claims'.
#'   abstract and claims (tac). Quoted.
#' @param pub_date_start Publication date limit to start at as YYYMMDD (numeric).
#' @param pub_date_end Publication date limit to end at as YYYMMDD (numeric).
#' @param filing_date_start Filing date limit to start at as YYYMMDD (numeric).
#' @param filing_date_end Filing date limit to end at as YYYMMDD (numeric).
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
#' @param families Either return the publication count and family numbers or if
#'   TRUE (default) return the patent families (deduplicates a set of
#'   publications to the first publication of the root "priority" or first
#'   filing).
#' @param results The number of results to return, either 50 or 500 (maximum).
#' @param timer Where retrieving over 50 results, the delay between sending requests to the Lens (default is 30 seconds, used internally by ops_iterate()).
#' @param inventor An inventor name or vector of inventor names. Use the convention surname and first name (family name and given name) for best results.
#' @param inventor_boolean Either "OR" or "AND"
#' @details Only one ranking measure may be used per query. For example, it is
#'   possible to rank by family scores but not family scores and latest
#'   publications or earliest publications. The suggested work flow is to
#'   retrieve the latest publications, then rank by family and then rank_citing.
#'   This will allow the most recent and the most important documents to be
#'   retrieved in three steps for a given query. In patent documents the convention is to list the surname (family name) and then the first names (given names). Name reversals can and do occur but best practice is to use e.g "Kirk James T" rather than "James T Kirk".
#' @return a data.frame or tibble
#' @export
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes
#' @importFrom rvest html_text
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_trim
#' @importFrom tibble as_tibble
#' @importFrom dplyr %>%
#' @examples \dontrun{lens_search("synthetic biology", timer = 30)}
#' @examples \dontrun{lens_search(synbio, boolean = "OR", timer = 30)}
#' @examples \dontrun{lens_search(synbio, boolean = "AND", timer = 30)}
#' @examples \dontrun{lens_search(synbio, boolean = "OR", type = "title", rank_family = TRUE, timer = 30)}
#' @examples \dontrun{lens_search(synbio, boolean = "OR", type = "abstract", rank_family = TRUE, timer = 30)}
#' @examples \dontrun{lens_search(synbio, boolean = "OR", type = "tac", rank_family = TRUE, timer = 30)}
#' @examples \donrun{lens_search(synbio, boolean = "OR", type = "tac", rank_citing = TRUE, timer = 30)}
lens_search <- function(query, boolean = "NULL", type = "NULL", pub_date_start = NULL, pub_date_end = NULL, filing_date_start = NULL, filing_date_end = NULL, rank_family = "NULL", rank_citing = "NULL", rank_sequences = "NULL", latest_publication = "NULL", earliest_publication = "NULL", latest_filing = "NULL", earliest_filing = "NULL", families = "TRUE", results = NULL, timer = 30, inventor = NULL, inventor_boolean = "NULL"){
  #cond <- cond(inventor, query)
  if(!is.null(inventor) && !is.null(query)){
    andlink <- "+%26%26+"
    inv_query <- lens_inventor(inventor)
    query <- lens_urls(query, type, boolean)
    query <- stringr::str_split(query, "=", n = 2)
    query <- paste0(query[[1]][[2]])
    out <- stringr::str_c(inv_query, andlink, query, collapse = TRUE)#works to here but not as null on inventor
  } else if(is.null(inventor) && !is.null(query)){
     out <- lens_urls(query, boolean, type, pub_date_start , pub_date_end, filing_date_start, filing_date_end, rank_family, rank_citing, rank_sequences, latest_publication, earliest_publication, latest_filing, earliest_filing, families, results, timer)
     #%>% lens_iterate(timer) #this part works in isolation but doesnt work in combination. So it is something to do with the if statement or the conditionals.
  }
out %>% lens_iterate(timer)
}