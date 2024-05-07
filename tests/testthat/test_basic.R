mock_create_reactive <- function(fields) {
  if (is.null(fields)) {
    fields <- list()
  }
  return(list2env(fields))
}

create_class <- function() {
  with_mocked_bindings({
    BaseClass <- Reactive6Class(
      "BaseClass",
      public = list(
        x = 5
      ),
      reactive = list(
        y = 7
      )
    )
  },
    create_reactive = mock_create_reactive
  )

  return(BaseClass)
}

instantiate_obj <- function() {
  with_mocked_bindings({
    Base <- Reactive6Class(
      "Base",
      public = list(
        x = 5
      ),
      reactive = list(
        y = 7
      )
    )

    obj <- Base$new()

    },
    create_reactive = mock_create_reactive
  )

  return(obj)
}

test_that("createReactive6Class", {
  cls <- create_class()
  expect_true(inherits(cls, "Reactive6ClassGenerator"))
  expect_true(inherits(cls, "R6ClassGenerator"))
})

test_that("createReactive6Object", {
  obj <- instantiate_obj()
  expect_true(inherits(obj, "Base"))
  expect_true(inherits(obj, "Reactive6"))
  expect_true(inherits(obj, "R6"))
})

test_that("dollarSucceedsForSetAndGet", {
  obj <- instantiate_obj()
  expect_equal(obj$`.__reactive__`$y, 7)
  expect_equal(obj$x, 5)
  obj$x <- 42
  expect_equal(obj$x, 42)
  expect_equal(obj$y, 7)
  obj$y <- 42
  expect_equal(obj$y, 42)
})


test_that("dollarFailsForUndefinedNames", {
  obj <- instantiate_obj()

  expect_error({
    obj$z
  }, "Undefined name 'z' in object of class Base during get operation")
  expect_error({
    obj$z <- 5
  }, "Undefined name 'z' in object of class Base during set operation")
})

test_that("handlingInheritance", {
  with_mocked_bindings({
    Base <- Reactive6Class(
      "Base",
      public = list(
        x = 5
      ),
      reactive = list(
        y = 7
      )
    )
    Derived <- Reactive6Class(
     "Derived",
      public = list(
        xd = 12
      ),
      reactive = list(
        yd = 24
      ),
      inherit = Base
    )
    obj <- Derived$new()
    },
    create_reactive = mock_create_reactive
  )

  expect_equal(obj$x, 5)
  expect_equal(obj$xd, 12)
  expect_equal(obj$y, 7)
  expect_equal(obj$yd, 24)
})

test_that("refuseMixedMetaclasses", {
  expect_error({
    with_mocked_bindings({
      Base <- R6::R6Class(
        "Base",
        public = list(
          x = 5
        )
      )
      Derived <- Reactive6Class(
      "Derived",
        public = list(
          xd = 12
        ),
        reactive = list(
          yd = 24
        ),
        inherit = Base
      )
      obj <- Derived$new()
    },
      create_reactive = mock_create_reactive
    )
  },
    "`inherit` must be a Reactive6ClassGenerator"
  )

})
