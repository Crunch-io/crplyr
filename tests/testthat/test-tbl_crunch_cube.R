context("tbl_crunch_cube methods")

cube <- loadCube("cubes/cat-x-mr-x-mr.json")
tbl <- as_tibble(cube)

test_that("tbl_crunch_cube object is generated", {
    expect_is(tbl, "tbl_crunch_cube")
    expect_equal(
        names(tbl), 
        c("animal", "opinion_mr_items", "opinion_mr_selections", "feeling_mr_items", 
          "feeling_mr_selections", "is_missing", "count", "row_count")
    )
})

test_that("tbl_crunch_cube metadata is accessible", {
    expect_equivalent(
        dim_types(tbl), 
        c("categorical", "mr_items", "mr_selections", "mr_items", 
          "mr_selections", "missing", "measure", "measure")          
    )
    expect_equal(names(dim_types(tbl)), names(tbl))
    aliases <- cube_attribute(tbl, "alias")
    expect_equivalent(
        aliases, 
        c("animal", "opinion_mr", "opinion_mr", "feeling_mr", 
          "feeling_mr", NA, NA, NA)
    )
    expect_equal(names(aliases), names(tbl))

    metadata <- cube_attribute(tbl, "all")
    expect_is(metadata[1], "list")
    expect_equal(metadata$animal$alias, "animal")
    expect_equal(names(metadata), names(tbl))
})

check_subset <- function(x) {
    type_name <- names(dim_types(x))
    meta_name <- names(cube_attribute(x, "alias"))
    test <- all.equal(type_name, meta_name, names(x))
    return(test)
}

test_that("attributes are subset along with table with `[`", {
    tbl2 <- tbl[1:2, 1:2]
    expect_true(check_subset(tbl2))
    expect_equal(tbl2, as.data.frame(tbl)[1:2, 1:2])
    
    tbl2 <- tbl[2, 1:2]
    expect_true(check_subset(tbl2))
    expect_equal(tbl2, as.data.frame(tbl)[2, 1:2])
    
    tbl2 <- tbl[2, ]
    expect_true(check_subset(tbl2))
    expect_equal(tbl2, as.data.frame(tbl)[2, ])
    
    tbl2 <- tbl[, c(3, 2, 4)]
    expect_true(check_subset(tbl2))
    expect_equal(tbl2, as.data.frame(tbl)[, c(3, 2, 4)])
    
    tbl2 <- tbl[, names(tbl) == "animal"]
    expect_true(check_subset(tbl2))
    expect_equal(tbl2, as.data.frame(tbl)[, names(tbl) == "animal", drop = FALSE])
    
    tbl2 <- tbl[, c("animal", "count", "opinion_mr_items")]
    expect_true(check_subset(tbl2))
    expect_equal(tbl2, as.data.frame(tbl)[, c("animal", "count", "opinion_mr_items")])
})

test_that("attributes are subset with `[[`", {
    tbl2 <- tbl[[2:3]]
    expect_true(check_subset(tbl2))
    
    v <- tbl[["opinion_mr_items"]]
    expect_is(v, "factor")
    
    tbl2 <- tbl[[grepl("opinion", names(tbl))]]
    expect_true(check_subset(tbl2))
})
