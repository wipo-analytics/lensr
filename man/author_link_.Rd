% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/author_link_.R
\name{author_link_}
\alias{author_link_}
\title{(internal) Generate boolean links between author names for lens_authors}
\usage{
author_link_(author, author_boolean = "NULL", author_type = "NULL")
}
\arguments{
\item{author}{a character vector of author names}

\item{author_boolean}{OR or AND}

\item{author_type}{default, crossref or pubmed}
}
\value{
a url
}
\description{
Generates the links between multiple authors using OR or AND for authors, crossref and pubmed.
}
\examples{
\dontrun{lens_authors(auth, author_boolean = "AND", author_type = "crossref")}
}

