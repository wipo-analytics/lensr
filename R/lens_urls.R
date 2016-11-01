#' @title Generate urls to search the Lens Patent Database
#' @description This function builds urls to search the Lens patent database. It is used internally in the lens_search() function. The default search groups documents by family and will return up to 50 results per page. The maximum number of results that can be retrieved is 500 (10 pages). For larger results sets use the free Lens online Collection facility to download upto 10,000 records. See details for information on the use of ranking and date measures to sort the data.
#' @param query One or more terms to search in the patent database (one or two
#'   words only at present)
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
#' @details Only one ranking measure may be used per query. For example, it is
#'   possible to rank by family scores but not family scores and latest
#'   publications or earliest publications. The suggested work flow is to
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
#' @examples \dontrun{lens_urls("synthetic biology")}
#' @examples \dontrun{lens_urls(synbio, boolean = "OR")}
#' @examples \dontrun{lens_urls(synbio, boolean = "AND")}
#' @examples \dontrun{lens_urls(synbio, boolean = "OR", type = "title", rank_family = TRUE)}
#' @examples \dontrun{lens_urls(synbio, boolean = "OR", type = "abstract", rank_family = TRUE)}
#' @examples \dontrun{lens_urls(synbio, boolean = "OR", type = "tac", rank_family = TRUE)}
#' @examples \donrun{lens_urls(synbio, boolean = "OR", type = "tac", rank_citing = TRUE)}
lens_urls <- function(query, boolean = "NULL", type = "NULL", pub_date_start = NULL, pub_date_end = NULL, filing_date_start = NULL, filing_date_end = NULL, rank_family = "NULL", rank_citing = "NULL", rank_sequences = "NULL", latest_publication = "NULL", earliest_publication = "NULL", latest_filing = "NULL", earliest_filing = "NULL", families = "NULL", results = NULL, timer = 30){
  baseurl <- "https://www.lens.org/lens/search?q="
  # To add document_type="NULL")
  #add Filing date ranges
  # add patent type searches (use lens name) and use applications and grants as default
  # add control for the number of results as results <- paste0("&n=", results). Keep 50 as the default
  #english <- "&l=en"

  # Format the query string depending on presence of space, length & boolean choices
  length <- length(query)
  if(length == 1){
    space <- stringr::str_detect(query, " ") # why is this here.
    query <- stringr::str_replace_all(query, " ", "+")
  }
  if(length > 1){
    query <- stringr::str_replace_all(query, " ", "+")
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
    query <- paste0("%22", query, "%22")
  }
  if(type == "title") {
    query <- paste0("title%3A%28%22", query, "%22%29")
  }
  if(type == "abstract") {
    query <- paste0("abstract", "%3A%28%22", query, "%22%29")
  }
  if(type == "claims") {
    query <- paste0("claims", "%3A%28%22", query, "%22%29")
  }
  if(type == "fulltext") {
    query <- paste0("%22", query, "%22")
  }
  if(type == "tac") {
    query <- paste0("%28title%3A%28%22", query, "%22%29+%7C%7C+abstract%3A%28%22", query, "%22%29+%7C%7C+claims%3A%28%22", query, "%22%29%29")
  }
  # add date ranges to the search string
  #deal with cases where not a full date by counting characters. working
  calcch <- function(x){
    n <- nchar(x)
    v <- n < 8
  }
  # pubs_char <- calcch(pub_date_start)
  # pube_char <- calcch(pub_date_end)
  # pub_fils <- calcch(filing_date_start)
  # pub_filed <- calcch(filing_date_end)
  # if(pubs_char == TRUE){
  #   pubs_char
  #   pub_date_start <- paste0(pub_date_start, "0101")
  #   query <- paste0(query, "&dates=%2Bpub_date%3A", pub_date_start)
  # }
  # if(pube_char == TRUE){
  #   pube_char
  #   pub_date_end <- paste0(pub_date_end, "1231")
  #   query <- paste0(query, "-", pub_date_end)
  # } #below is going wrong somehow
  # if(pub_fils == TRUE){
  #   pub_fils
  #   filing_date_start <- paste0(filing_date_start, "0101")
  #   query <- paste0(query, "&dates=%2Bpub_date%3A", filing_date_start)
  # }
  # if(pub_filed == TRUE){
  #   pub_filed
  #   filing_date_end <- paste0(filing_date_end, "1231")
  #   query <- paste0(query, "-", filing_date_end)
  # }
# full date sequence
  if(is.numeric(pub_date_start)){
    query <- paste0(query, "&dates=%2Bpub_date%3A", pub_date_start)
  }
  if(is.numeric(pub_date_end)){
    query <- paste0(query, "-", pub_date_end)
  }
  if(is.numeric(filing_date_start)){
  query <- paste0(query, "&dates=%2Bfiling_date%3A", filing_date_start)
  }
  if(is.numeric(filing_date_end)){
  query <- paste0(query, "-", filing_date_end)
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
  if(is.numeric(results)){
    results <- paste0("&n=", results)
    query <- paste0(query, results)
  } else if(is.null(results)){
    fifty <- paste0("&n=", 50) #this line is causing a problem with lens_count
    query <- paste0(query, fifty)
    }
  if(families == TRUE){
    families <- "&f=true"
    query <- paste0(query, families)
  }

  # adds base url here. Renaming from query to lens_pages
  #a new if statement, if results = 50 then carries on as before. if results = 500 then it runs paste and adds the loop.
  ##calculate number of results
  # test <- lens_count(query) %>% dplyr::select(., families)
  # print(test)
  # Surely this should simply add the page numbers to the URL to call based on the number of pages up to a maximum of 500 results. Then the urls can be passed to iterate and lens_parse.
  lens_pages <- paste0(baseurl, query)
  # test <- lens_count(lens_pages) %>% dplyr::select(., families)
  # lens_pages
  # test
  # lens_pages <- lens_parse(lens_pages)
  # lens_pages
  # if(results <= 50){ # test less than or equal to.
  #   lens_pages <- paste0(baseurl, query)
  #   # lens_pages <- lens_parse(lens_pages)
  #   lens_pages
  # }
  # if(results == 500){
  #   baseurl_pages <- "https://www.lens.org/lens/search?p="
  #   q <- "&q="
  #   #there will need to be a counter here for cases where less than 500 results even if only 10 pages can be fetched.
  #   #qtotal <-
  #   lens_pages <- paste0(baseurl_pages, seq(0,10), q, query)
  #   lens_pages <- lens_iterate(lens_pages, lens_parse, Sys.sleep(timer))
  #   lens_pages
  # }
}