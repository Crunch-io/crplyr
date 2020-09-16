context("Automation query text helpers")

test_that("fmt items only", {
  expect_equal(
    ca_list_to_text(items = list("aaa", noquote("bbb"))),
    '\n"aaa", bbb'
  )
})

test_that("fmt items with pre&post no new lines", {
  expect_equal(
    ca_list_to_text("PRE", items = list("aaa", noquote("bbb")), "POST"),
    '\nPRE "aaa", bbb POST'
  )
})

test_that("fmt items with pre&post explicit lines", {
  expect_equal(
    ca_list_to_text("PRE", items = list("aaa", noquote("bbb")), "POST", sep_newline = TRUE),
    '\nPRE\n  "aaa", \n  bbb\nPOST'
  )
})

test_that("fmt items with pre&post wrapped new lines", {
  expect_equal(
    ca_list_to_text("PRE", items = list("aaa", noquote("bbb")), "POST", line_wrap = 5),
    '\nPRE\n  "aaa", \n  bbb\nPOST'
  )
})

test_that("fmt items with pre&post no new lines + indent", {
  expect_equal(
    ca_list_to_text("PRE", items = list("aaa", noquote("bbb")), "POST", indent = 4),
    '\n    PRE "aaa", bbb POST'
  )
})

test_that("fmt items with pre&post explicit lines + indent", {
  expect_equal(
    ca_list_to_text(
      "PRE", 
      items = list("aaa", noquote("bbb")), 
      "POST", 
      sep_newline = TRUE, 
      indent = 4
    ),
    '\n    PRE\n      "aaa", \n      bbb\n    POST'
  )
})

test_that("fmt items with pre&post wrapped new lines + indent", {
  expect_equal(
    ca_list_to_text("PRE", items = list("aaa", noquote("bbb")), "POST", indent = 4, line_wrap = 4),
    '\n    PRE\n      "aaa", \n      bbb\n    POST'
  )
})

test_that("fmt items only, no newline", {
  expect_equal(
    ca_list_to_text(items = list("aaa", noquote("bbb")), start_newline = FALSE),
    '"aaa", bbb'
  )
})

test_that("fmt items only, different sep", {
  expect_equal(
    ca_list_to_text(items = list("aaa", noquote("bbb")), sep = "|"),
    '\n"aaa"|bbb'
  )
})
