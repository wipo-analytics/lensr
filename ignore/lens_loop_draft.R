# #generate a set of urls taking lens_search as the input and retrieve 10 pages containing 500 results as a data.frame using lens_parse()
# #lens_urls <_ function(x)
# #https://www.lens.org/lens/search?p=0&q=%22synthetic+biology%22+%7C%7C+%22synthetic+genomics%22+%7C%7C+%22biological+parts%22&f=true&v=table&n=50
# #https://www.lens.org/lens/search?p=1&q=%22synthetic+biology%22+%7C%7C+%22synthetic+genomics%22+%7C%7C+%22biological+parts%22&f=true&v=table&n=50
#
# # Generate the urls (note needs a container for output)
# baseurl <- "https://www.lens.org/lens/search?p="
# q <- "&q="
# lens_pages <- paste0(baseurl, seq(0,10), q) #OK, now need add the urls to lense parse, and then add the loop. Or does it simply generate the urls and leave the user to decide how many to retrieve. e.g results = 50 (with the alternative being 500)

# urls <- lens_search(synbio, boolean = "OR", results = 500)

three_urls
lens_loop <- function(x, timer = 30) {
  lens_results <- vector(mode = "list", length = length(x))
  for(i in 1:length(lens_results)) {
    lens_urls <- x[[i]]
    lens_parsed <- lens_parse(lens_urls)
    lens_results[[i]] <- lens_parsed # problem here
    message("getting_data")
    Sys.sleep(time = timer)
  }
  lens_results <- dplyr::bind_rows(lens_results)
  return(lens_results)
}

test <- lens_loop(three_urls, timer = 30) # that works
# warning messages on converting factors to character. In lens_parse handle that in advance.
# create shorter urls for testthat purposes so will run fater.
lens_iterate <- function(x, timer = NULL) {
  lens_results <- lapply(x, lens_parse)
  Sys.sleep(timer)
  lens_results <- dplyr::bind_rows(lens_results)
  return(lens_results)
}
#"https://www.lens.org/lens/search?q=%22synthetic+biology%22&n=2" #will work for testing
# test_lappy <- lens_iterate(three_urls, timer = 30)
#
# testf <- function(x, timer = NULL){
#   lapply(x, function(i) Sys.sleep(timer)) #testing timer
# }


lapply(1:4, function(i) Sys.sleep(5))