create_reactive <- function(reactive_fields) {
  if (is.null(reactive_fields)) {
    reactive_fields <- list()
  }
  return(do.call(shiny::reactiveValues, reactive_fields))
}

#' Defines a reactive class.
#'
#' It is like a R6 class, but allows to specify a new field "reactive"
#' as a list of fields associated to an initialisation value.
#' The fields will be exported in the public interface and will behave
#' as reactive fields in a reactiveValues object.
#'
#' @examples
#' AppModel <- Reactive6Class(
#'   "AppModel",
#'   public = list(
#'     increment = function(value) {
#'       self$amount <- self$amount + value
#'     },
#'     asSquared = function() {
#'       return(self$amount * self$amount)
#'     },
#'     nonreactive_amount = 10
#'   ),
#'   reactive = list(
#'     amount = 10
#'   ),
#' )
#'
#' In the above example
#' @export
Reactive6Class <- function(classname = NULL, public = list(),
                           private = NULL, active = NULL, reactive = list(),
                           inherit = NULL,
                           parent_env = parent.frame(),
                           ...) {
    capsule <- R6:::capsule
    if (!capsule$all_named(reactive)) {
      stop("All elements of reactive must be named")
    }
    allnames <- c(names(public), names(private), names(active), names(reactive))
    if (any(duplicated(allnames))) {
      stop(
        paste("All items in public, private, active and reactive",
              "must have unique names.")
      )
    }

    if (any(
      c("self", "private", "super", "clone", "initialize", "finalize")
      %in% names(reactive)
      )) {
      stop("Cannot add a reactive with reserved name.")
    }
    if (length(capsule$get_functions(reactive)) != 0) {
      stop("All items of reactive must be non-functions")
    }

    generator <- R6::R6Class(
      classname = classname, public = public,
      private = private, active = active,
      lock_objects = FALSE, class = TRUE, portable = TRUE, lock_class = FALSE,
      cloneable = FALSE, parent_env = parent_env
    )

    generator$reactive_fields <- capsule$get_nonfunctions(reactive)
    generator$create_reactive <- create_reactive
    reactive_new <- function(...) {
      # Look up the parent env of the current environment _after_ it's been
      # reassigned, that is, at new() call. This will be the generator.
      gen <- parent.env(environment())
      inherit <- get_inherit()  # nolint
      if (!is.null(inherit)) {
        if (!inherits(inherit, "Reactive6ClassGenerator")) {
          stop("`inherit` must be a Reactive6ClassGenerator.")
        }
      }

      metaclass_new <- (R6:::generator_funs$new)
      environment(metaclass_new) <- gen
      self <- metaclass_new(...)
      reactive_fields <- gen$reactive_fields

      if (!is.null(inherit)) {
        recursive_merge <- function(obj, which) {
          if (is.null(obj)) return(NULL)
          capsule$merge_vectors(
            recursive_merge(obj$get_inherit(), which), obj[[which]]
          )
        }
        reactive_fields <- capsule$merge_vectors(
          recursive_merge(inherit, "reactive_fields"), reactive_fields
        )
      }
      self$`.__reactive__` <- gen$create_reactive(reactive_fields)

      # Need to update the class of the object to insert the "Reactive6Class"
      # baseclass just before "R6" otherwise it won't know how to perform
      # the S3 lookup for $
      prev_class <- class(self)
      class(self) <- c(
        prev_class[seq_len(length(prev_class) - 1)],
        "Reactive6",
        prev_class[length(prev_class)]
      )
      return(self)
    }
    generator_funs <- list(
      new = reactive_new
    )
    generator_funs <- capsule$assign_func_envs(generator_funs, generator)
    capsule$list2env2(generator_funs, generator)
    generator$inherit <- substitute(inherit)
    # Return the generator (that is, the actual class)
    class(generator) <- c("Reactive6ClassGenerator", class(generator))
    return(generator)
}

#' @export
`$.Reactive6` <- function(x, name) {
  reactive <- base::get(".__reactive__", envir = x)
  if (name %in% names(reactive)) {
    return(reactive[[name]])
  } else if (name %in% names(x)) {
    return(base::get(name, envir = x))
  }
  stop(paste0(
    "Undefined name '", name, "' in object of class ", class(x)[[1]]),
    " during get operation.")
}

#' @export
`$<-.Reactive6` <- function(x, name, value) {
  reactive <- base::get(".__reactive__", envir = x)
  if (name %in% names(reactive)) {
    reactive[[name]] <- value
    return(x)
  } else if (name %in% names(x)) {
    base::assign(name, value, envir = x)
    return(x)
  }
  stop(paste0(
    "Undefined name '", name, "' in object of class ", class(x)[[1]]),
    " during set operation.")
}
