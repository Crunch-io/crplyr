# This is here instead of in testthat because `manage_cases` doesn't
# seem to load testthat.R
library(vdiffr)

Sys.setlocale("LC_COLLATE", "C") ## What CRAN does; affects sort order
set.seed(999) ## To ensure that tests that involve randomness are reproducible

"%>%" <- magrittr::`%>%`

crplyr:::.onLoad()
