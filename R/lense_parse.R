#' @title Parse data from lens_search to a data.frame or tibble
#' @description Used internally to parse the results from lens_search.
#' @param data html from the lens retrieved using rvest read_html
#' @return a tibble.
#' @export
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes
#' @importFrom rvest html_text
#' @importFrom magrittr %>%
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_trim
#' @importFrom tibble as_tibble
#' @examples \dontrun{data <- lens_urls(synbio, boolean = "OR") %>% lens_parse()}
lens_parse <- function(data){
##process the results into a data.frame with rvest and stringr
# To do add in publication counts and families counts as seq additions to the tibbl
  html <- xml2::read_html(data)
  # results <- rvest::html_nodes(html, ".resultCount") %>%
  #   rvest::html_text() %>%
  #   as.numeric()
  # families <- rvest::html_nodes(html, ".breadnum:nth-child(4)") %>%
  #   rvest::html_text() %>%
  #   as.numeric() # Will not work with f=true because only results present.
  #returns can be of different lengths so add an id column to arrive at equal length?
  publication_numbers <- rvest::html_nodes(html, ".link span:nth-child(2)") %>%
    rvest::html_text() %>%
    stringr::str_replace_all(" ", "") %>%
    stringr::str_replace_all("/", "") %>%
    as.character()
  lens_id <- rvest::html_nodes(html, ".lens-id a") %>%
    rvest::html_text() %>%
    as.character()
  # applicants <- rvest::html_nodes(html, "#results .owner .sup-result") %>%
  #   rvest::html_text() %>%
  #   print()
  # addresses cases where text field .sup=result is blank
  applicants <- rvest::html_nodes(html, ".owner") %>%
    rvest::html_text() %>%
    stringr::str_split(":")
  applicants <- lapply(applicants, "[[", 2) %>%
    stringr::str_trim(side = "both")  %>%
    as.character()
  titles <- rvest::html_nodes(html, ".title a") %>%
    rvest::html_text() %>%
    as.character()
  doc_type <- rvest::html_nodes(html, ".doc-type .sup-result") %>%
    rvest::html_text() %>%
    stringr::str_replace_all("       \n     \t\t", "") %>%
    stringr::str_replace_all("\n    \t", "") %>%
    as.character()
  publication_date <- rvest::html_nodes(html, ".sup-item:nth-child(1) .sup-result") %>%
    rvest::html_text() %>%
    stringr::str_trim(side = "both") %>%
    as.character()
  family_count <- rvest::html_nodes(html, ".sup-item:nth-child(2) .sup-result") %>%
    rvest::html_text() %>%
    as.numeric()
  citing_count <- rvest::html_nodes(html, ".sup-item:nth-child(3) a") %>%
    rvest::html_text() %>%
    as.numeric()
  df <- tibble::as_tibble(data.frame(publication_numbers, lens_id, titles, applicants, doc_type, publication_date, family_count, citing_count)) # problem of factors in tibble.
}
