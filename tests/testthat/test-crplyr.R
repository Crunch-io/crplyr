context("crplyr misc")

with_mock_crunch({
    test_that("we add the crplyr user agent", {
        cfg <- crunch:::get_crunch_config()
        expect_true(grepl("rcrunch", cfg$headers[["user-agent"]]))
    })
})

