context("'summarize' on CrunchDataset")

with_mock_crunch({
    ds <- loadDataset("test ds")

    d2f <- function (...) dots_to_formula(lazyeval::lazy_dots(...))
    test_that("dots_to_formula", {
        expect_equal(d2f(avg=mean(birthyr), ct=n()),
            list(avg=mean(birthyr), ct=n()) ~ 1)
    })

    test_that("summarize makes a cube request", {
        expect_GET(summarize(ds, mean=mean(birthyr)))
    })
    test_that("summarize can handle multiple measures", {
        expect_GET(summarize(ds, mean=mean(birthyr), sd=sd(starttime)))
        expect_GET(summarize(ds, mean=mean(birthyr), count=n()))
    })

    ## TODO: construct some complete tests; make fixtures from mtcars or whatever
})
