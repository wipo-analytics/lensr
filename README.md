---
title: "Access the Lens Patent Database using R"
author: "Paul Oldham"
date: "26 October 2016"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, cache = TRUE)
```
[![Travis-CI Build Status](https://travis-ci.org/poldham/lensr.svg?branch=master)](https://travis-ci.org/poldham/lensr)

## Introduction

The is a pre-release of an R package to provide access to the basic functions of the [Lens Patent Database](https://www.lens.org/lens/) using R. The package is in an early stage of development. 

For example, we might want to search for patent activity known to be associated with a particular country such as Kenya. In this case we will bring back only the first 50 results

```{r tidy=TRUE}
species <- c("Actinomadura kijaniata", "Natrialba magadii", "Natronobacterium magadii", "Glossina brevipalpis", "Phlebotomus duboscqi")
kenya <- lens_search(query = species, boolean = "OR", families = TRUE, timer = 10)
```

The Lens provides access to millions of patent records from around the world and allows for searches of the full text (title, abstract, description and claims) of patent documents. We can also search using applicants, inventors and author names and combine searches for say inventors and text terms.

The Lens allows those who register for a free account to save, share and download Collections of upto 10,000 records. If you are seeking access to large amounts of patent data we suggest that you follow this route by registering for the database and creating Collections online. 

`lensr` is intended for light weight exploratory use of patent data using the Lens. The maximum number of records that will presently be returned by a search is 500. If you would like more records than that please use the Lens database and Collections directly. 

You do not need to know R to use this package and the main functions are covered below. 

The package was developed as part of a wider initiative to make patent data more accessible for analytics purposes by patent offices and researchers in developing countries. For further information on open source patent analytics see the [WIPO Manual on Open Source Patent Analytics](https://wipo-analytics.github.io/) and the [repository of related materials](https://github.com/wipo-analytics). 

### Background

The package is written using a combination of `rvest` and packages in the [`tidyverse`](https://blog.rstudio.org/2016/09/15/tidyverse-1-0-0/). The package is built in RStudio using `roxygen2`. `testthat` is used for unit testing and the package is tested using Travis. Package development follows the [ropensci guide](https://github.com/ropensci/onboarding/blob/master/packaging_guide.md) and may one day make it into the `ropensci` list of packages. At present the package is in early development.

###Getting started

`lensr` is not on CRAN but can be installed using devtools. If you need to install devtools in RStudio use:

```{r devtools, eval=FALSE}
install.packages("devtools")
```

Then use:

```{r install, eval=FALSE}
devtools::install_github("poldham/lensr")
```

## Using lensr

`lensr` involves three main functions and one overall function.

1. `lens_count()`. Get counts of patent families and publications from different kinds of searches.
2. `lens_urls()`. Generates urls for use in searches.
3. `len_iterate()`. Fetch the data and parses it into a data.frame table using `lens_parse()`.
4. `lens_search()` brings the above together in one place with some sensible defaults. 

The main function that you will probably want to use is `lens_search()` but a typical workflow will start by using `lens_count()` to bring back some numbers from a query followed by the use of `lens_search()`.

###lens_count

Use lens_count() to get an idea of the results for different queries. Note that you can control whether to search the full text (default), title, abstract or claims as separate fields or the title or abstract or claims (tac) at the same time. 

```{r count_default}
lens_count("drones")
```

To search titles we would use:

```{r count_title}
lens_count("drones", type = "title")
```

and the title or abstract or claims (tac)

```{r count_tac}
lens_count("drones", type = "tac")
```

By default the lens returns the number of patent families and the number of publications across the database. The number of patent families refers to first filings of patent applications. Publications refers additional republications (as applications, as grants, with search reports etc. in multiple countries and is always higher than the families count). When retrieving patent data from the lens the `lens_search()` return patent families (to avoid duplicate records).

###lens_search()

`lens_search()` is the main package function and allows you to conduct searches using key terms or combinations of inventor names and key terms. In future the function will be expanded to include applicant names and author names (for literature cited). The following assumes we are interested in synthetic biology and sets up a couple of inventor names and some search terms associated with synthetic biology. The search will return 50 results and in future will return up to a maximum of 500. 

```{r lens_search_terms}
inventor <- c("Venter Craig", "Smith Hamilton")
synbio <- c("synthetic biology",  "synthetic genomics", "synthetic genome", "synthetic genomes", "biological parts", "genetic circuit", "genetic circuits")
```

We can then run a search for the inventor names and the key terms as follows.

```{r lens_search_eg}
df <- lens_search(inventor = auth, inventor_boolean = "OR", query = synbio, boolean = "OR", type = "tac", timer = 20)
```

[Note: Note that families is not added on here because of the lack of links with lens_urls inside this part of lens_search. So the function requires some reformulation, maybe after applicants and ipcs are done.] 

We can also use `lens_search()` simply with a list of key terms

```{r lens_urls}
library(dplyr)
df1 <- lens_search(query = synbio, boolean = "OR", type = "tac")
```

The package is under active early stage development and will be updated to address consistency and errors.