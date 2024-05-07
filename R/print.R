#' @export
format.Reactive6 <- function(x, ...) {
  if (is.function(.subset2(x, "format"))) {
    .subset2(x, "format")(...)
  } else {
    ret <- paste0("<", class(x)[1], ">")

    # If there's another class besides first class and R6
    classes <- setdiff(class(x), c("Reactive6", "R6"))
    if (length(classes) >= 2) {
      ret <- c(ret, paste0("  Inherits from: <", classes[2], ">"))
    }

    # reactive objects don't print out the values using object_summaries,
    # because object summaries uses .subset2, which does not use the
    # resolution system. We therefore need to use it, however we also don't
    # want to introduce a dependency when invoking print on an object,
    # so we'll add a shiny::isolate() on each term.
    ret <- c(ret,
      "  Reactive:",
      R6:::indent(.reactive_object_summaries(x$`.__reactive__`), 4)
    )
    ret <- c(ret,
      "  Public:",
      R6:::indent(R6:::object_summaries(x,
        exclude = c(".__reactive__", ".__active__", ".__enclos_env__")), 4)
    )

    private <- .subset2(.subset2(x, ".__enclos_env__"), "private")
    if (!is.null(private)) {
      ret <- c(ret,
        "  Private:",
        R6:::indent(R6:::object_summaries(private), 4)
      )
    }
    paste(ret, collapse = "\n")
  }
}

#' @export
print.Reactive6 <- function(x, ...) {
  if (is.function(.subset2(x, "print"))) {
    .subset2(x, "print")(...)
  } else {
    cat(format(x, ...), sep = "\n")
  }
  invisible(x)
}

#' @export
format.Reactive6ClassGenerator <- function(x, ...) {
  classname <- x$classname
  if (is.null(classname)) classname <- "unnamed"
  ret <- paste0("<", classname, "> object generator")

  if (!is.null(x$inherit)) {
    ret <- c(ret, paste0("  Inherits from: <", deparse(x$inherit), ">"))
  }

  ret <- c(ret,
    "  Reactive:",
    R6:::indent(R6:::object_summaries(x$reactive_fields), 4)
  )
  ret <- c(ret,
    "  Public:",
    R6:::indent(R6:::object_summaries(x$public_fields), 4),
    R6:::indent(R6:::object_summaries(x$public_methods), 4)
  )

  if (!is.null(x$active)) {
    ret <- c(ret,
      "  Active:",
      R6:::indent(R6:::object_summaries(x$active), 4)
    )
  }

  if (!(is.null(x$private_fields) && is.null(x$private_methods))) {
    ret <- c(ret,
      "  Private:",
      R6:::indent(R6:::object_summaries(x$private_fields), 4),
      R6:::indent(R6:::object_summaries(x$private_methods), 4)
    )
  }

  paste(ret, collapse = "\n")
}

#' @export
print.Reactive6ClassGenerator <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
}

# Version of object summaries that handles reactivevalues
.reactive_object_summaries <- function(x) {
    if (length(x) == 0)
        return(NULL)
    # sort the names to resemble ls()
    obj_names <- sort(names(x))
    values <- vapply(obj_names, function(name) {
        if (is.environment(x) && bindingIsActive(name, x)) {
            "active binding"
        } else {
            obj <- shiny::isolate(x[[name]])
            if (is.function(obj))
                deparse(args(obj))[[1L]]
            else if (is.environment(obj) && identical(class(obj),
                "environment"))
                "environment"
            else if (is.null(obj))
                "NULL"
            else if (is.atomic(obj)) {
                txt <- as.character(utils::head(obj, 60))
                txt <- paste(txt, collapse = " ")
                R6:::trim(txt)
            } else {
              paste(class(obj), collapse = ", ")
            }
        }
    }, FUN.VALUE = character(1))
    return(paste0(obj_names, ": ", values, sep = ""))
}
