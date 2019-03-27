context("Variable plotting")

test_that("generate_colors overflows to viridis", {
    expect_equal(generate_colors(mtcars$mpg),
        c("#316395", "#cf3e3e", "#fcb73e", "#37ad6c", "#9537b5", "#17becf",
            "#e377c2", "#fdae6b", "#0099c6", "#ed5487", "#3366cc", "#440154FF",
            "#481D6FFF", "#453581FF", "#3D4D8AFF", "#34618DFF", "#2B748EFF",
            "#24878EFF", "#1F998AFF", "#25AC82FF", "#40BC72FF", "#67CC5CFF",
            "#97D83FFF", "#CBE11EFF", "#FDE725FF")
    )
})

with_mock_crunch({
    ds <- loadDataset("test ds")
    test_that("crunch variables produce ggplots", {
        expect_is(autoplot(ds$birthyr), "ggplot")
        expect_is(autoplot(ds$starttime), "ggplot")
    })

    test_that("autoplot triggers correct GETs", {
        expect_GET(autoplot(ds$gender),
            'https://app.crunch.io/api/datasets/1/cube/?query=%7B%22dimensions%22%3A%5B%7B%22variable%22%3A%22https%3A%2F%2Fapp.crunch.io%2Fapi%2Fdatasets%2F1%2Fvariables%2Fgender%2F%22%7D%5D%2C%22measures%22%3A%7B%22count%22%3A%7B%22function%22%3A%22cube_count%22%2C%22args%22%3A%5B%5D%7D%7D%2C%22weight%22%3Anull%7D&filter=%7B%7D'
        )
        expect_GET(autoplot(ds$catarray),
            'https://app.crunch.io/api/datasets/1/cube/?query=%7B%22dimensions%22%3A%5B%7B%22each%22%3A%22https%3A%2F%2Fapp.crunch.io%2Fapi%2Fdatasets%2F1%2Fvariables%2Fcatarray%2F%22%7D%2C%7B%22variable%22%3A%22https%3A%2F%2Fapp.crunch.io%2Fapi%2Fdatasets%2F1%2Fvariables%2Fcatarray%2F%22%7D%5D%2C%22measures%22%3A%7B%22count%22%3A%7B%22function%22%3A%22cube_count%22%2C%22args%22%3A%5B%5D%7D%7D%2C%22weight%22%3Anull%7D&filter=%7B%7D'
            )
        expect_GET(autoplot(ds$mymrset),
            'https://app.crunch.io/api/datasets/1/cube/?query=%7B%22dimensions%22%3A%5B%7B%22each%22%3A%22https%3A%2F%2Fapp.crunch.io%2Fapi%2Fdatasets%2F1%2Fvariables%2Fmymrset%2F%22%7D%2C%7B%22function%22%3A%22as_selected%22%2C%22args%22%3A%5B%7B%22variable%22%3A%22https%3A%2F%2Fapp.crunch.io%2Fapi%2Fdatasets%2F1%2Fvariables%2Fmymrset%2F%22%7D%5D%7D%5D%2C%22measures%22%3A%7B%22count%22%3A%7B%22function%22%3A%22cube_count%22%2C%22args%22%3A%5B%5D%7D%7D%2C%22weight%22%3Anull%7D&filter=%7B%7D'
        )
    })
})