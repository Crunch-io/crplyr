context("Cube to tibble")

check_cube_match <- function(arr, tibble) {
    dims <- setdiff(names(tibble), c("count", ".unweighted_counts", "is_missing"))
    test <- lapply(seq_along(tibble$count), function(i) {
        args <- tibble[i, dims, drop = TRUE]
        args <- lapply(args, as.character)
        do.call(`[`, c(list(arr), args)) == tibble$count[i]
    })
    return(all(unlist(test)))
}

with_mock_crunch({
    ds <- loadDataset("test ds")
    ## Load a bunch of different cubes
    with_POST("https://app.crunch.io/api/datasets/1/multitables/apidocs-tabbook/", {
        book <- tabBook(multitables(ds)[[1]], data=ds)
    })
    
    test_that("Loading a bunch of different cube fixtures", {
        expect_is(book, "TabBookResult")
    })
    
    test_that("as_tibble method on a basic Cube", {
        cat_cat <- loadCube("cubes/cat-x-cat.json")
        cat_tibble <- as_tibble(cat_cat)
        expect_is(cat_tibble, "tbl_df")
        expect_equal(dim(cat_tibble), c(12, 5))
        expect_equal(names(cat_tibble), c("v4", "v7", "count", ".unweighted_counts", "is_missing"))
        expect_true(check_cube_match(cat_cat@arrays$count, cat_tibble))
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
    
    test_that("as_tibble on a cat_mr_mr cube", {
        cat_mr_mr <- loadCube("cubes/cat-x-mr-x-mr.json")
        cat_mr_mr_tibble <- as_tibble(cat_mr_mr)
        expect_is(cat_mr_mr_tibble, "tbl_df")

        expect_equal(dim(cat_mr_mr_tibble), c(162, 8))

        expect_equal(
            names(cat_mr_mr_tibble), 
            c("animal", "opinion_mr_items", "opinion_mr_selections", "feeling_mr_items", 
                "feeling_mr_selections", "count", ".unweighted_counts", "is_missing"
            ))
    })
})
