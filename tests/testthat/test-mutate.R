context("'mutate' on CrunchDataset")

with_mock_crunch({
    ds <- loadDataset("test ds")
    
    test_that("single var mutate: single var", {
        query <- mutate(ds, zzz = test_create_single_var_cmd(birthyr))
        
        expect_equal(
            make_query_text(query), 
            "birthyr AS zzz;"
        )
    })
    
    test_that("single var + formula notation", {
        query <- mutate(
            ds, 
            zzz = test_create_single_var_cmd(birthyr, arg = ~titles(.))
        )
        
        expect_equal(
            make_query_text(query), 
            paste0("birthyr AS zzz ARG \"", name(ds$birthyr), "\";")
        )
    })

    test_that("multiple vars", {
        query <- mutate(ds, zzz = test_create_single_var_cmd(birthyr, gender))
        
        expect_equal(
            make_query_text(query), 
            "birthyr, gender AS zzz;"
        )
    })

    test_that("across works (outside function)", {
        query <- mutate(
            ds, 
            across(
                one_of(c("birthyr", "gender")), 
                test_create_single_var_cmd, .names = "zzz_{col}")
        )
        
        expect_equal(
            make_query_text(query), 
            "birthyr AS zzz_birthyr;\n\ngender AS zzz_gender;"
        )
    })
    
    test_that("across works (inside function)", {
        query <- mutate(
            ds, 
            zzz = test_create_single_var_cmd(across(one_of(c("birthyr", "gender"))))
        )
        
        expect_equal(
            make_query_text(query), 
            "birthyr, gender AS zzz;"
        )        
    })

    test_that("across (inside) + formula work together", {
        query <- mutate(
            ds, 
            zzz = test_create_single_var_cmd(
                across(one_of(c("birthyr", "gender"))),
                arg = ~titles(.)
            )
        )
        
        expect_equal(
            make_query_text(query), 
            paste0(
                "birthyr, gender AS zzz ARG \"", 
                name(ds$birthyr), "\", \"", 
                name(ds$gender), "\";"
            )
        )
    })

    test_that("ca keyword work", {
        query <- mutate(ds, zzz = test_create_single_var_cmd(ca$dots(birthyr, gender)))
        
        expect_equal(
            make_query_text(query), 
            "birthyr...gender AS zzz;"
        )
    })

    test_that("across (inside) + variable work", {
        query <- mutate(
            ds, 
            zzz = test_create_single_var_cmd(across(one_of(c("birthyr"))), gender)
        )
        
        expect_equal(
            make_query_text(query), 
            "birthyr, gender AS zzz;"
        )   
    })

    test_that("across + across work", {
        query <- mutate(
            ds, 
            zzz = test_create_single_var_cmd(
                across(one_of(c("birthyr"))), 
                across(one_of(c("gender")))
            )
        )
        
        expect_equal(
            make_query_text(query), 
            "birthyr, gender AS zzz;"
        )  
    })

    test_that("across + ca keyword", {
        query <- mutate(
            ds, 
            zzz = test_create_single_var_cmd(
                across(one_of(c("birthyr"))), 
                ca$like("A%")
            )
        )
        
        expect_equal(
            make_query_text(query), 
            "birthyr, LIKE(\"A%\") AS zzz;"
        )  
    })
    
    test_that("multiple commands", {
        query <- mutate(
            ds, 
            zzz = test_create_single_var_cmd(birthyr), 
            yyy = test_create_single_var_cmd(gender))

        expect_equal(
            make_query_text(query),
            "birthyr AS zzz;\n\ngender AS yyy;"
        )
    })
    
    test_that("multiple commands (same mutate), depends on previous", {
        query <- mutate(
            ds, 
            zzz = test_create_single_var_cmd(birthyr),
            yyy = test_create_single_var_cmd(zzz)
        )
        query <- mutate(query, )

        expect_equal(
            make_query_text(query),
            "birthyr AS zzz;\n\nzzz AS yyy;"
        )
    })
    
    test_that("multiple commands (new mutate), depends on previous", {
        query <- mutate(ds, zzz = test_create_single_var_cmd(birthyr))
        query <- mutate(query, yyy = test_create_single_var_cmd(zzz))
        
        expect_equal(
            make_query_text(query),
            "birthyr AS zzz;\n\nzzz AS yyy;"
        )
    })
    
    test_that("nested command", {
        query <- mutate(
            ds, 
            yyy = test_create_single_var_cmd(zzz = test_create_single_var_cmd(birthyr))
        )
        
        expect_equal(
            make_query_text(query),
            "birthyr AS zzz;\n\nzzz AS yyy;"
        )
    })
})
