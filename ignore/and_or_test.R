and_or_test <- function(query, boolean = "NULL", type = "NULL"){
  baseurl <- "https://www.lens.org/lens/search?q="
  space <- stringr::str_detect(query, " ")
  length <- length(query)
  if(length > 1){
    query <- stringr::str_replace(query, " ", "+")
  }
    if(boolean == "OR"){
    query <- stringr::str_c(query, collapse = "%22+%7C%7C+%22")
    quote <- "%22"
    query <- paste0(quote, query, quote)
  }
  if(boolean == "AND"){
    query <- stringr::str_c(query, collapse = "%22+%26%26+%22")
    quote <- "%22"
    query <- paste0(quote, query, quote)
  }
  query <- paste0(baseurl, query)


}

#
# query <- c("synthetic biology", "synthetic genomics", "biological parts")
# space <- stringr::str_detect(query, " ")
# length <- length(query)
# if(length > 1){
#   query <- stringr::str_replace(query, " ", "+")
#   return(query)
# }
# or_query <- stringr::str_c(query, collapse = "%22%+%7C%7C+%22%") #Adds in the or separator and replaces terminal quotes. The question now is how to add the %22 at the beginning (also what will work with the title, abstract and claims as that is a bit more complicated).
# and_query <- stringr::str_c(query, collapse = "%22+%26%26+%22")
# quote <- "%22%"
# paste0(quote, or_query, quote) %>% print()
# paste0(quote, and_query, quote) %>% print()
# print(query)