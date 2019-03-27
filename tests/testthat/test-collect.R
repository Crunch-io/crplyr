context("'collect' on CrunchDataset")

test_that("transfer groups between datasets", {
    grouped_df <- group_by(mtcars, cyl, wt)
    df2 <- mtcars %>% select(-gear) %>% transfer_groups(grouped_df)
    expect_identical(group_vars(df2), group_vars(grouped_df))
})

with_mock_crunch({
    ds <- loadDataset("test ds")
    test_that("collect generates expected POST request", {
        expect_POST(
            ds %>%
                select(gender, birthyr) %>%
                collect(),
            'https://app.crunch.io/api/datasets/1/export/csv/',
            '{"filter":null,"where":{"function":"select","args":[{"map":{"66ae9881e3524f7db84970d556c34552":{"variable":"https://app.crunch.io/api/datasets/1/variables/gender/"},"f78ca47313144b57adfb495893968e70":{"variable":"https://app.crunch.io/api/datasets/1/variables/birthyr/"}}}]},"options":{"use_category_ids":true}}'
        )
    })
    test_that("collect on grouped dataset", {
        expect_POST(
            ds %>%
                select(gender, birthyr) %>%
                group_by(gender) %>%
                collect(),
            'https://app.crunch.io/api/datasets/1/export/csv/',
            '{"filter":null,"where":{"function":"select","args":[{"map":{"66ae9881e3524f7db84970d556c34552":{"variable":"https://app.crunch.io/api/datasets/1/variables/gender/"},"f78ca47313144b57adfb495893968e70":{"variable":"https://app.crunch.io/api/datasets/1/variables/birthyr/"}}}]},"options":{"use_category_ids":true}}'
        )
    })
    ds3 <- loadDataset("ECON.sav")
    test_that("collect can pull hidden variables", {
        expect_POST(
            ds3 %>%
                select(gender, birthyr) %>%
                collect(),
            'https://app.crunch.io/api/datasets/3/export/csv/',
            '{"filter":null,"where":{"function":"select","args":[{"map":{"66ae9881e3524f7db84970d556c34552":{"variable":"https://app.crunch.io/api/datasets/3/variables/gender/"},"f78ca47313144b57adfb495893968e70":{"variable":"https://app.crunch.io/api/datasets/3/variables/birthyr/"}}}]},"options":{"use_category_ids":true}}'
        )
    })

    test_that("collect doesn't affect dplyr methods", {
        expect_identical(mtcars, collect(mtcars))
    })
})
