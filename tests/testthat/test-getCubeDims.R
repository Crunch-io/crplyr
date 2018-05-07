context("getDimType")
#TODO Remove when moved to rcrunch
test_that("getDimType returns the expected cube dimension types", {
    ca_mr <- loadCube("cubes/catarray-x-mr.json")
    expect_equivalent(getDimType(ca_mr@dims),
        c("ca_items", "ca_categories", "mr_items", "mr_selections"
        ))
    cat_cat <- loadCube("cubes/cat-x-cat.json")
    expect_equivalent(getDimType(cat_cat@dims), c("categorical", "categorical"))
    cat_mr_mr <- loadCube("cubes/cat-x-mr-x-mr.json")
    expect_equivalent(getDimType(cat_mr_mr@dims),
        c("categorical", "mr_items", "mr_selections", "mr_items",
            "mr_selections")
    )
    cattarray_cat <- loadCube("cubes/catarray-x-cat.json")
    expect_equivalent(getDimType(cattarray_cat@dims),
        c("ca_items", "ca_categories", "categorical")
    )
})