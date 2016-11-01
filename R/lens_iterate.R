#' @title Iterate over lens urls and return a data.frame
#' @description Used internally in lens_search. Takes a set of urls generared by lens_urls() and uses lapply to fetch the raw data and converts it to a data.frame using lens_parse(). The lists of data.frames are bound using dplyr::bind_rows().
#' @param x A vector of urls (see lens_urls)
#' @param timer (numeric) Delay between each request to the Lens. 20 seconds as the default which may be a little slow.
#'
#' @return a data.frame
#' @export
#' @importFrom dplyr bind_rows
#' @examples \dontrun{df <- lens_iterate(three_urls, timer = 30)}
lens_iterate <- function(x, timer = 20) {
  lens_results <- lapply(x, lens_parse)
  Sys.sleep(timer)
  lens_results <- dplyr::bind_rows(lens_results)
  return(lens_results)
}
#To do:
#1 Note that factor issue continues to throw warnings so needs to convert to character.
#2. Where timer is NULL throws an invalid warning if no entry made. Changed default to 20. Create warning message?