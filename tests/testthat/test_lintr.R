test_that("linting", {
  skip_on_covr()

  lints <- lintr::lint_package("../../",
    exclusions = list(
      "R/RcppExports.R"
    )
  )

  if (length(lints) != 0) {
    print("")
    print(lints)
    fail("Linting failed")
    return()
  }

  succeed()
})
