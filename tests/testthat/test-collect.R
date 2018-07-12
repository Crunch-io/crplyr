context("'collect' on CrunchDataset")

with_mock_crunch({
    ds <- loadDataset("test ds")
    browser()
    test_that("collect generates expected POST request", {
        expect_POST(
            ds %>% 
                select(gender, birthyr) %>% 
                collect(),
            'https://app.crunch.io/api/datasets/1/export/csv/',
            '{"filter":null,"where":{"function":"select","args":[{"map":{"66ae9881e3524f7db84970d556c34552":{"variable":"https://app.crunch.io/api/datasets/1/variables/gender/"},"f78ca47313144b57adfb495893968e70":{"variable":"https://app.crunch.io/api/datasets/1/variables/birthyr/"}}}]},"options":{"use_category_ids":true}}'
        )
    })
    test_that("collect doesn't affect dplyr methods", {
        expect_identical(mtcars, collect(mtcars))
    })
})