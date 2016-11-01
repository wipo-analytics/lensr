#' @title Search the Lens Patent Database
#' @description This function builds urls to search the Lens patent database. The data is processed using lens_parse(). The default search groups documents by family and will return up to 50 results per page. The maximum number of results that can be retrieved is 500 (10 pages). For larger results sets use the free Lens online Collection facility to download upto 10,000 records. See details for information on the use of ranking and date measures to sort the data.
#' @param query One or more terms to search in the patent database (one or two
#'   words only at present)
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
#' @param families Either return the publication count and family numbers or if
#'   TRUE (default) return the patent families (deduplicates a set of
#'   publications to the first publication of the root "priority" or first
#'   filing).
#' @param results The number of results to return, either 50 or 500 (maximum).
#' @details Only one ranking measure may be used per query. For example, it is
#'   possible to rank by family scores but not family scores and latest
#'   publications or earlies publications. The suggested work flow is to
#'   retrieve the latest publications, then rank by family and then rank_citing.
#'   This will allow the most recent and the most important documents to be
#'   retrieved in three steps for a given query.
#' @return a data.frame or tibble
#' @export
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes
#' @importFrom rvest html_text
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_trim
#' @importFrom tibble as_tibble
#' @examples \dontrun{lens_search(synbio, boolean = "OR")}
#' @examples \dontrun{lens_search(synbio, boolean = "AND")}
#' @examples \dontrun{lens_search(synbio, boolean = "OR", type = "title", rank_family = TRUE)}
#' @examples \dontrun{lens_search(synbio, boolean = "OR", type = "abstract", rank_family = TRUE)}
#' @examples \dontrun{lens_search(synbio, boolean = "OR", type = "tac", rank_family = TRUE)}
#' @examples \donrun{lens_search(synbio, boolean = "OR", type = "tac", rank_citing = TRUE)}
lens_search <- function(query, boolean = "NULL", type = "NULL", rank_family = "NULL", rank_citing = "NULL", rank_sequences = "NULL", latest_publication = "NULL", earliest_publication = "NULL", latest_filing = "NULL", earliest_filing = "NULL", families = "TRUE", results = 50){
  baseurl <- "https://www.lens.org/lens/search?q="
  #english <- "&l=en"
  # Format the query string depending on presence of space, length & boolean choices
  space <- stringr::str_detect(query, " ")
  length <- length(query)
  if(length > 1){
    query <- stringr::str_replace(query, " ", "+") #works ok, need an option if multiple terms
  }
  if(boolean == "OR"){
    query <- stringr::str_c(query, collapse = "%22+%7C%7C+%22")
    # quote <- "%22"
    # query <- paste0(quote, query, quote)
  }
  if(boolean == "AND"){
    query <- stringr::str_c(query, collapse = "%22+%26%26+%22")
    # quote <- "%22"
    # query <- paste0(quote, query, quote)
  }
  if(type == "NULL") {
    query <- paste0("%22", query, "%22", "&n=50")
  }
  if(type == "title") {
    query <- paste0("title%3A%28%22", query, "%22%29","&n=50")
  }
  if(type == "abstract") {
    query <- paste0("abstract", "%3A%28%22", query, "%22%29", "&n=50")
  }
  if(type == "claims") {
    query <- paste0("claims", "%3A%28%22", query, "%22%29", "&n=50")
  }
  if(type == "fulltext") {
    query <- paste0("%22", query, "%22", "&n=50")
  }
  if(type == "tac") {
    query <- paste0("%28title%3A%28%22", query, "%22%29+%7C%7C+abstract%3A%28%22", query, "%22%29+%7C%7C+claims%3A%28%22", query, "%22%29%29", "&n=50")
  }
  # Until here we only have the query, and the remaining arguments below are added to the query without affecting the baseurl. So, move them up
  query <- paste0(baseurl, query) ##adds base url here. Renaming from query to url
  #a new if statement, if results = 50 then carries on as before. if results = 500 then it runs paste and adds the loop.
  if(results == 50){
    lens_parse(query)
  }
  if(results == 500){

  }

  # Add ranking arguments to the search string
  if(rank_citing == TRUE){
    rank_citing <- "&s=citing_pub_key_count&d=-"
    query <- paste0(query, rank_citing)
  }
  if(rank_family == TRUE){
    rank_family <- "&s=simple_family_size&d=-"
    query <- paste0(query, rank_family)
  }
  if(rank_sequences == TRUE){
    rank_sequences <- "&s=sequence_count&d=-"
    query <- paste0(query, rank_sequences)
  }
  if(latest_publication == TRUE){
      latest_publication <- "&s=pub_date&d=-"
      query <- paste0(query, latest_publication)
    }
  if(earliest_publication == TRUE){
    earliest_publication <- "&s=pub_date&d=%2B"
    query <- paste0(query, earliest_publication)
  }
  if(latest_filing == TRUE){
    latest_filing <- "&s=filing_date&d=-"
    query <- paste0(query, latest_filing)
  }
  if(earliest_filing == TRUE){
    earliest_filing <- "&s=filing_date&d=%2B"
    query <- paste0(query, earliest_filing)
  }
  if(families == TRUE){
    families <- "&f=true"
    query <- paste0(query, families)
  }
  print(query)
}

