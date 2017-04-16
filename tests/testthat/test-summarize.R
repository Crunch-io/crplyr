context("'summarize' on CrunchDataset")

with_mock_crunch({
    ## Fixture dataset created from mtcars
    ds <- loadDataset("https://app.crunch.io/api/datasets/mtcars/")

    d2f <- function (...) dots_to_formula(lazyeval::lazy_dots(...))
    test_that("dots_to_formula", {
        expect_equal(d2f(avg=mean(birthyr), ct=n()),
            list(avg=mean(birthyr), ct=n()) ~ 1)
    })

    test_that("summarize makes a cube request", {
        tbl1 <- summarize(ds, hp=mean(hp))
        expect_is(tbl1, "tbl_df")
        expect_identical(dim(tbl1), c(1L, 1L))
        expect_identical(names(tbl1), "hp")
        expect_equal(as.numeric(tbl1[1,1]), 146.6875)
    })

    test_that("summarize can handle multiple measures", {
        tbl2 <- summarize(ds, hp=mean(hp), sd_hp=sd(hp), count=n())
        expect_is(tbl2, "tbl_df")
        expect_identical(dim(tbl2), c(1L, 3L))
        expect_identical(names(tbl2), c("hp", "sd_hp", "count"))
        expect_equal(as.numeric(tbl2[1,1]), 146.6875)
        expect_equal(as.numeric(tbl2[1,3]), 32)
    })

    test_that("summarize after filter", {
        tbl3 <- ds %>%
                filter(cyl == 6) %>%
                summarize(hp=mean(hp), sd_hp=sd(hp), count=n())
        expect_is(tbl3, "tbl_df")
        expect_identical(dim(tbl3), c(1L, 3L))
        expect_identical(names(tbl3), c("hp", "sd_hp", "count"))
        expect_equal(as.numeric(tbl3[1,3]), 7)
    })
})
