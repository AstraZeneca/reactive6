test_that("printReactive6Class", {
  cls <- create_class()
  out <- capture.output(print(cls))
  expect_equal(out,
    c(
    "<Base> object generator",
    "  Reactive:",
    "    y: 7",
    "  Public:",
    "    x: 5"
    )
  )

  cls <- create_derived_class()
  out <- capture.output(print(cls))
  expect_equal(out,
    c(
    "<Derived> object generator",
    "  Inherits from: <Base>",
    "  Reactive:",
    "    yd: 24",
    "  Public:",
    "    xd: 12"
    )
  )
})

test_that("printReactive6Object", {
  obj <- instantiate_obj()
  out <- capture.output(print(obj))
  expect_equal(out,
    c(
    "<Base>",
    "  Reactive:",
    "    y: 7",
    "  Public:",
    "    x: 5"
    )
  )

  obj <- instantiate_obj()
  obj$x <- 8
  obj$y <- 9
  out <- capture.output(print(obj))
  expect_equal(out,
    c(
    "<Base>",
    "  Reactive:",
    "    y: 9",
    "  Public:",
    "    x: 8"
    )
  )

  obj <- instantiate_derived_obj()
  out <- capture.output(print(obj))
  expect_equal(out,
    c(
    "<Derived>",
    "  Inherits from: <Base>",
    "  Reactive:",
    "    y: 7",
    "    yd: 24",
    "  Public:",
    "    x: 5",
    "    xd: 12"
    )
  )

})
