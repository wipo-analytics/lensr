#note NULL rather than "NULL" issue for later with lens search

#' @title Search patent applicant names in the Lens
#' @description Search for patent applicant names in the Lens patent database.
#' @param applicant an applicant name or vector of applicant names (character)
#' @param applicant_boolean OR or AND operators
#'
#' @return a URL
#' @export
#'
#' @examples \dontrun{lens_applicant("Synthetic Genomics")}
lens_applicants <- function(applicant = NULL, applicant_boolean = "NULL"){
  baseurl <- "https://www.lens.org/lens/search?q="
  start <- "applicant%3A%28%22"
  end <- "%22%29"
  orlink <- "%22%29+%7C%7C+applicant%3A%28%22"
  andlink <- "%22%29+%26%26+applicant%3A%28%22"
  applicant_length <- length(applicant)
  if(applicant_length == 1){
    start
    end
    query <- stringr::str_replace_all(applicant, " ", "+")
    query <- stringr::str_c(baseurl, start, query, end)
  }
  if(applicant_length > 1){
    query <- stringr::str_replace_all(applicant, " ", "+")
  }
  if(applicant_boolean == "OR"){
    start
    orlink
    end
    query <- stringr::str_c(query, collapse = orlink)
    query <- stringr::str_c(query, end)
    query <- paste0(baseurl, start, query)
  }
  if(applicant_boolean == "AND"){
    start
    andlink
    end
    query <- stringr::str_c(query, collapse = andlink)
    query <- stringr::str_c(query, end)
    query <- paste0(baseurl, start, query)
  }
  if(applicant_length > 1 & applicant_boolean == "NULL"){ # defaults to OR
    start
    orlink
    end
    query <- stringr::str_c(query, collapse = orlink)
    query <- stringr::str_c(query, end)
    query <- paste0(baseurl, start, query)
    # warning("boolean defaulting to 'OR'")
  }
  query
}