context("Cube to tibble")

with_mock_crunch({
    ds <- loadDataset("test ds")
    ## Load a bunch of different cubes
    with_POST("https://app.crunch.io/api/datasets/1/multitables/apidocs-tabbook.json", {
        book <- tabBook(multitables(ds)[[1]], data=ds)
    })

    test_that("Loading a bunch of different cube fixtures", {
        expect_is(book, "TabBookResult")
    })

    test_that("as_tibble method on a basic Cube", {
        country_x_pet <- book[[8]][[3]]
        expect_is(country_x_pet, "CrunchCube")
        arr <- as.array(country_x_pet)
        #         q1
        # country     Cat Dog Bird
        # Argentina   0   0    0
        # Australia   3   0    0
        # Austria     1   0    1
        # Belgium     2   1    1
        # Brazil      0   3    1
        tbl <- as_tibble(country_x_pet)
        # # A tibble: 15 × 3
        #      country     q1 count
        #       <fctr> <fctr> <dbl>
        # 1  Argentina    Cat     0
        # 2  Australia    Cat     3
        # 3    Austria    Cat     1
        # 4    Belgium    Cat     2
        # 5     Brazil    Cat     0
        # 6  Argentina    Dog     0
        # 7  Australia    Dog     0
        # 8    Austria    Dog     0
        # 9    Belgium    Dog     1
        # 10    Brazil    Dog     3
        # 11 Argentina   Bird     0
        # 12 Australia   Bird     0
        # 13   Austria   Bird     1
        # 14   Belgium   Bird     1
        # 15    Brazil   Bird     1
        expect_is(tbl, "tbl_df")
        expect_identical(dim(arr), c(5L, 3L))
        expect_identical(dim(tbl), c(15L, 3L))
        ## TODO: assert values are correct
    })

    test_that("as_tibble with categorical array", {
        skip("TODO")
        print(book[[3]][[3]])
        # , , petloc = Home
        #
        #       q1
        # petloc Cat Dog Bird
        #   Cat    3   1    0
        #   Dog    1   1    1
        #   Bird   1   0    0
        #
        # , , petloc = Work
        #
        #       q1
        # petloc Cat Dog Bird
        #   Cat    3   1    1
        #   Dog    1   2    0
        #   Bird   1   1    0
        print(as_tibble(book[[3]][[3]]))
    })

    test_that("as_tibble when repeated dimension vars", {
        skip("TODO")
        print(book[[2]][[3]])
        #         q1
        # q1     Cat Dog Bird
        # Cat    6   0    0
        # Dog    0   4    0
        # Bird   0   0    3
        print(as_tibble(book[[2]][[3]]))
    })

    test_that("If weighted, the '.unweighted_counts' are included", {
        skip("Need to load a weighted fixture. Also need Cube to know if it is weighted")
    })
})
