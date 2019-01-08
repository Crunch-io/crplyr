context("API stuff")

with_mock_crunch({
    expect_header(crGET("https://app.crunch.io/api/"), "user-agent:.*crplyr/.*")
})
