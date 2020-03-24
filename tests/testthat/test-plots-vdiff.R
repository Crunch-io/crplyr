context("autoplot")

test_that("autplot matches mocked plots", {
    # cube1 <- loadCube("cubes/univariate-categorical.json")
    # expect_doppelganger("univariate-dot", autoplot(cube1, "dot"))
    # expect_doppelganger("univariate-tile", autoplot(cube1, "tile"))
    # expect_doppelganger("univariate-bar", autoplot(cube1, "bar"))

    cube2 <- loadCube("cubes/cat-x-cat.json")
    expect_doppelganger("cat-cat-dot", autoplot(cube2, "dot"))
    # expect_doppelganger("cat-cat-tile", autoplot(cube2, "tile"))
    # expect_doppelganger("cat-cat-bar", autoplot(cube2, "bar"))

    # prop_cube <- crunch::prop.table(cube, 2)
    # expect_doppelganger("cat-cat_prop_dot", autoplot(prop_cube, "dot"))
    # expect_doppelganger("cat-cat_prop_bar", autoplot(prop_cube, "bar"))
    # expect_doppelganger("cat-cat_prop_tile", autoplot(prop_cube, "tile"))

    # cube3 <- loadCube("cubes/cat-x-mr-x-mr.json")
    # expect_doppelganger("cat-x-mr-x-mr-dot", autoplot(cube3, "dot"))
    # expect_doppelganger("cat-x-mr-x-mr-tile", autoplot(cube3, "tile"))
    # expect_doppelganger("cat-x-mr-x-mr-bar", autoplot(cube3, "bar"))
    # expect_doppelganger("cat_x_mr_x_mr_prop", autoplot(crunch::prop.table(cube3, 1:2)))
    # 
    # cube4 <- loadCube("cubes/catarray-x-cat.json")
    # expect_doppelganger("catarray-x-cat-dot", autoplot(cube4, "dot"))
    # expect_doppelganger("catarray-x-cat-tile", autoplot(cube4, "tile"))
    # expect_doppelganger("catarray-x-cat-bar", autoplot(cube4, "bar"))
    # expect_doppelganger("catarray_x_cat_prop", autoplot(crunch::prop.table(cube4, 1:2)))

    # cube5 <- loadCube("cubes/catarray-x-mr.json")
    # cube5@useNA <- "always"
    # expect_doppelganger("catarray-x-mr-dot", autoplot(cube5, "dot"))
    # expect_doppelganger("catarray-x-mr-tile", autoplot(cube5, "tile"))
    # expect_doppelganger("catarray-x-mr-bar", autoplot(cube5, "bar"))
    # 
    # tbl <- readRDS("tbl_crunch_mocks/4d_tbl_crunch.Rds")
    # expect_is(tbl, "tbl_crunch_cube")
    # expect_doppelganger("catarray-x-cat-x-cat-dot", autoplot(tbl, "dot"))
    # expect_doppelganger("catarray-x-cat-x-cat-tile", autoplot(tbl, "tile"))
    # expect_doppelganger("catarray-x-cat-x-cat-bar", autoplot(tbl, "bar"))
})

# test_that("autoplot errors when given too many dimensions", {
#     tbl <- readRDS("tbl_crunch_mocks/4d_tbl_crunch.Rds")
#     expect_error(
#         autoplot(tbl, measure = c("count", "unweighted_n")),
#         "Autoplot can only support one measure"
#     )
# })