Sys.setlocale("LC_COLLATE", "C") ## What CRAN does; affects sort order
set.seed(999) ## To ensure that tests that involve randomness are reproducible

"%>%" <- magrittr::`%>%`

with_mock_crunch <- function (expr) {
    with(temp.option(crunch.api="https://app.crunch.io/api/"), {
        ## TODO: Move the test.api switch to with_test_authentication
        suppressMessages(trace("mockRequest", quote({
            if (!file.exists(f)) {
                ## Look for mock in inst/
                crunchfile <- system.file(f, package="crunch")
                if (nchar(crunchfile)) {
                    f <- crunchfile
                }
            }
            }),
            at=4,
            print=FALSE,
            where=without_internet))
        on.exit(suppressMessages(untrace("mockRequest", where=without_internet)))
        suppressMessages(trace("mockDownload", quote({
            if (!file.exists(f)) {
                ## Look for mock in inst/
                crunchfile <- system.file(f, package="crunch")
                if (nchar(crunchfile)) {
                    f <- crunchfile
                }
            }
            }),
            at=3,
            print=FALSE,
            where=without_internet))
        on.exit(suppressMessages(untrace("mockDownload", where=without_internet)))
        with_mock_API({
            crunch:::warmSessionCache()
            eval.parent(expr)
        })
    })
}

with_POST <- function (resp, expr) {
    ## Mock a POST that returns something, like a Location header pulled from 201
    force(resp)
    with_mock(`crunch::crPOST`=function (...) resp, eval.parent(expr))
}
