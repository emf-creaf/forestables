# Verbose messaging ---------------------------------------------------------------------------

test_that("verbose_msg works as intended", {

  # .verbose with message works
  expect_message(
    verbose_msg(message("foo"), .verbose = TRUE), "foo"
  )
  expect_no_message(
    verbose_msg(message("foo"), .verbose = FALSE)
  )

  # cli::* works
  expect_message(
    verbose_msg(cli::cli_inform(".verbose is TRUE"), .verbose = TRUE), ".verbose is TRUE"
  )
  expect_no_message(
    verbose_msg(cli::cli_inform(".verbose is FALSE"), .verbose = FALSE)
  )

  # expect errors if something is wrong
  expect_error(
    verbose_msg(message("foo"), .verbose = 4)
  )
  expect_error(
    verbose_msg(message("foo"), .verbose = "4")
  )
})
