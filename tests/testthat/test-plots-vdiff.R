context("autoplot")

test_that("autplot matches mocked plots", {
    cube <- loadCube("cubes/univariate-categorical.json")
    expect_doppelganger("univariate-dot", autoplot(cube, "dot"))
    expect_doppelganger("univariate-grid", autoplot(cube, "grid"))
    expect_doppelganger("univariate-bar", autoplot(cube, "bar"))

    cube <- loadCube("cubes/cat-x-cat.json")
    expect_doppelganger("cat-cat-dot", autoplot(cube, "dot"))
    expect_doppelganger("cat-cat-grid", autoplot(cube, "grid"))
    expect_doppelganger("cat-cat-bar", autoplot(cube, "bar"))

    cube <- loadCube("cubes/cat-x-mr-x-mr.json")
    expect_doppelganger("cat-x-mr-x-mr-dot", autoplot(cube, "dot"))
    expect_doppelganger("cat-x-mr-x-mr-grid", autoplot(cube, "grid"))
    expect_doppelganger("cat-x-mr-x-mr-bar", autoplot(cube, "bar"))

    cube <- loadCube("cubes/catarray-x-cat.json")
    expect_doppelganger("catarray-x-cat-dot", autoplot(cube, "dot"))
    expect_doppelganger("catarray-x-cat-grid", autoplot(cube, "grid"))
    expect_doppelganger("catarray-x-cat-bar", autoplot(cube, "bar"))

    cube <- loadCube("cubes/catarray-x-mr.json")
    expect_doppelganger("catarray-x-mr-dot", autoplot(cube, "dot"))
    expect_doppelganger("catarray-x-mr-grid", autoplot(cube, "grid"))
    expect_doppelganger("catarray-x-mr-bar", autoplot(cube, "bar"))
})
