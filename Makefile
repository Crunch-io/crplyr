VERSION = $(shell grep ^Version DESCRIPTION | sed s/Version:\ //)

doc:
	R --slave -e 'devtools::document()'
	-git add --all man/*.Rd

pkgdown: doc
	R -e 'library(crplyr); pkgdown::build_site()'

test:
	R CMD INSTALL --install-tests .
	R --slave -e 'Sys.setenv(NOT_CRAN="true"); library(httptest); setwd(file.path(.libPaths()[1], "crplyr", "tests")); system.time(test_check("crplyr", filter="${file}", reporter=ifelse(nchar("${r}"), "${r}", "summary")))'

deps:
	R --slave -e 'install.packages(c("codetools", "httptest", "devtools", "roxygen2", "knitr"), repo="http://cran.at.r-project.org", lib=ifelse(nchar(Sys.getenv("R_LIB")), Sys.getenv("R_LIB"), .libPaths()[1]))'

build: doc
	R CMD build .

check: build
	-export _R_CHECK_CRAN_INCOMING_REMOTE_=FALSE && R CMD check --as-cran crplyr_$(VERSION).tar.gz
	if grep "* checking examples ... NONE" crplyr.Rcheck/00check.log; then echo 'Must add examples before submitting to CRAN!'; fi
	rm -rf crplyr.Rcheck/

release: build
	-unset INTEGRATION && R CMD CHECK --as-cran crplyr_$(VERSION).tar.gz
	rm -rf crplyr.Rcheck/

man: doc
	R CMD Rd2pdf man/ --force

md:
	R CMD INSTALL --install-tests .
	mkdir -p inst/doc
	R -e 'setwd("vignettes"); lapply(dir(pattern="Rmd"), knitr::knit, envir=globalenv())'
	mv vignettes/*.md inst/doc/
	-cd inst/doc && ls | grep .md | xargs -n 1 sed -i '' 's/.html)/.md)/g'
	-cd inst/doc && ls | grep .md | xargs -n 1 egrep "^.. Error"

build-vignettes: md
	R -e 'setwd("inst/doc"); lapply(dir(pattern="md"), function(x) markdown::markdownToHTML(x, output=sub("\\\\.md", ".html", x)))'
	cd inst/doc && ls | grep .html | xargs -n 1 sed -i '' 's/.md)/.html)/g'

covr:
	R --slave -e 'library(covr); package_coverage()'
