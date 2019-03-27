context("'mutate' on CrunchDataset (not implemented)")

with_mock_crunch({
    ds <- loadDataset("test ds")
    test_that("mutate errors nicely", {
        expect_error(
            mutate(ds, men=gender == "Male"),
            "You can, however, derive"
        )
    })

    test_that("When group_by calls mutate, it also errors nicely", {
        expect_error(
            group_by(ds, men=gender == "Male"),
            "You can, however, derive"
        )
    })
})
