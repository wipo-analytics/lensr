#' @title Search the Lens using International Patent Classification (IPC) codes
#' @description Search the Lens using individual or combinations of IPC Codes.
#' @param ipc An ipc code or vector of codes (character)
#' @param ipc_boolean "AND" or "OR" (quoted)
#' @details IPC codes on the group level must include a forward slash e.g.
#'   A61K31/00 or C12N15/82 or the Lens will not display results. A forward
#'   slash is not necessary on the subclass level e.g. A61K or C12N.
#' @return a url
#' @export
#' @examples \dontrun{lens_ipc(A61K31/00)}
#' @examples \dontrun{lens_ipc(c("A61K31/00", "C12N15/82"), boolean = "AND")}
#' @examples \dontrun{lens_ipc(c("A61K31/00", "C12N15/82"), boolean = "OR")}
lens_ipcs <- function(ipc, ipc_boolean = "NULL"){
  baseurl <- "https://www.lens.org/lens/search?q="
  start <- "classification_ipcr%3A"
  #end <- ""
  #slash <- "%5C%2F"
  andlink <- "+%26%26+classification_ipcr%3A"
  orlink <- "+%7C%7C+classification_ipcr%3A"
  ipc_length <- length(ipc)
  ipc_test <- stringr::str_detect(ipc, "/")
  #test if ipc at group and subgroup level contains /. Not working at present
  if(ipc_length > 4 && ipc_test == FALSE){
    ipc_length
    ipc_test
    message <- "ipc at group or subgroup level must contain a forward slash /"
    print(message)
  }
  if(ipc_length == 1){
    baseurl
    start
    ipc <- stringr::str_replace_all(ipc, "/", "%5C%2F")
    query <- paste0(baseurl, start, ipc)
  }
  if(ipc_length > 1){
    query <- stringr::str_replace_all(ipc, "/", "%5C%2F")
  }
  if(ipc_boolean == "OR"){
    baseurl
    start
    orlink
    query <- stringr::str_c(query, collapse = orlink)
    query <- paste0(baseurl, start, query)
    }
  if(ipc_boolean == "AND"){
    baseurl
    start
    andlink
    query <- stringr::str_c(query, collapse = andlink)
    query <- paste0(baseurl, start, query)
  }
  query
}