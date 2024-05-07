# Functions that generate testing fixtures.
# create_foo <- function() {
# # return foo
# }

create_class <- function() {
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
  },
    create_reactive = mock_create_reactive
  )

  return(Base)
}

create_derived_class <- function() {
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
    },
    create_reactive = mock_create_reactive
  )
  return(Derived)
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

instantiate_derived_obj <- function() {
  with_mocked_bindings({
    Derived <- create_derived_class()
    obj <- Derived$new()
    },
    create_reactive = mock_create_reactive
  )

  return(obj)
}


mock_create_reactive <- function(fields) {
  if (is.null(fields)) {
    fields <- list()
  }
  return(list2env(fields))
}
