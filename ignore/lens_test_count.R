lens_test_count <- function(query, type = "NULL"){
  baseurl <- "https://www.lens.org/lens/search?q="
  families <- "&f=true"
  english <- "&l=en"
  # Add an AND/OR option.
  space <- stringr::str_detect(query, " ")
  length <- length(query)
  if(length > 1){
    query <- stringr::str_replace(query, " ", "+")
    query <- paste0(baseurl, query) #needs some more thought. See example above, basically calculate length and loop over it adding the queries for each term as needed.
  }
}

# At present this produces three separate searches when what I actually want is:

# OR QUERY https://www.lens.org/lens/search?q=%22synthetic+biology%22+%7C%7C+%22synthetic+genomics%22+%7C%7C+%22biological+parts%22&l=en
# AND query. https://www.lens.org/lens/search?q=%22synthetic+biology%22+%26%26+%22synthetic+genomics%22+%26%26+%22biological+parts%22&l=en


# Because the query could be pretty long, I need to calculate the length using length(query) and then maybe add the operator after the text, then replace.
#Step 1. detect spaces in query and str_replace
#Step 2. lapply
#for each item, paste "%22" at the beginning and the end of the search term
#for OR paste "+%7C%7C+" between the "%22% markers or find %22%22 and replace with %22+%7C%7C+%22
#for AND paste "+%26%26+" between the "%22% markers

#could maybe use sprintf

query <- c("synthetic biology", "synthetic genomics", "biological parts")
space <- stringr::str_detect(query, " ")
length <- length(query)
if(length > 1){
  query <- stringr::str_replace(query, " ", "+")
  return(query)
}
or_query <- stringr::str_c(query, collapse = "%22%+%7C%7C+%22%") #Adds in the or separator and replaces terminal quotes. The question now is how to add the %22 at the beginning (also what will work with the title, abstract and claims as that is a bit more complicated).
and_query <- stringr::str_c(query, collapse = "%22+%26%26+%22")
quote <- "%22%"
paste0(quote, or_query, quote) %>% print()
paste0(quote, and_query, quote) %>% print()
print(query)

