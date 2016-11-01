#' @title Search the Lens using inventor names and key terms.
#' @description Use a single inventor name or vector of inventor names with one or more search terms. Inventor names should be in the format "Surname First Name".
#' @param inventor An inventor name or vector of inventor names.
#' @param inventor_boolean OR or AND
#' @param query A search term or set of search terms
#' @param type fulltext = default, "ti" = title, "ab" = abstract, "tac" = title or abstract or claims. See ops_urls for details.
#' @param boolean for vector of search terms OR or AND
#' @return a url for use in ops_iterate
#' @export
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_c
#' @examples \dontrun{lens_inventor_keyword("Kerr Russell", query = "bahamas")}
#' @examples \dontrun{lens_inventor_keyword(inventor = "Pomponi Shirley", query = "bahamas")}
#' @examples \dontrun{lens_inventor_keyword(inventor = auth, inventor_boolean = "OR", query = synbio, type = "tac", boolean = "OR")}
#' @examples \dontrun{lens_inventor_keyword(inventor = "Venter Craig", query = c("synthetic genomes", "synthetic genomics"), type = "tac", boolean = "OR")}
#' @examples \dontrun{lens_inventor_keyword(inventor = "Venter Craig", query = country$country_name, boolean = "OR")}
lens_inventor_keyword <- function(inventor = "NULL", inventor_boolean = "NULL", query = "NULL", type = "NULL", boolean = "NULL"){
   andlink <- "+%26%26+"
   # build the inventor query and add the bridge link to text search
   if(!is.null(inventor)){
     andlink
     inv_query <- lens_inventor(inventor)
     inv_query <- paste0(inv_query, andlink)
   }
   # add query using nested if statement from lens_urls
   length <- length(query)
   if(!is.null(query)){ # from lens_urls
     if(length == 1){
       space <- stringr::str_detect(query, " ") # why is this here.
       query <- stringr::str_replace_all(query, " ", "+")
     }
     if(length > 1){
       query <- stringr::str_replace_all(query, " ", "+")
     }
     if(boolean == "OR"){
       query <- stringr::str_c(query, collapse = "%22+%7C%7C+%22")
     }
     if(boolean == "AND"){
       query <- stringr::str_c(query, collapse = "%22+%26%26+%22")
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
   }
   end <- "&n=50"
   out <- paste0(inv_query, query, end)
}

# should be something like
#
# if(!is.null(inventor) && !is.null(query)){
#   inv_query <- lens_inventor(inventor)
#   query <- lens_urls(query, type, boolean)
#   query <- stringr::str_split(query, "=", n = 2)
#   query <- paste0(query[[1]][[2]])
#   out <- stringr::str_c(inv_query, query, collapse = TRUE)
# }

#concept
#1. paste inventors first
#2. then paste the search query and any additional controls onto the inventor URL. So first I need the inventor URL as my basis. Then I work out how to add the text element and controls.
# add applicant option