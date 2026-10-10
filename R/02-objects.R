# Standardized TSCI data and survey-design objects

.build_data_object <- function(data,
                               sample_type = c("primary", "auxiliary"),
                               id = NULL,
                               variables = NULL,
                               metadata = list()) {

  sample_type <- match.arg(sample_type)

  .validate_data(
    data,
    data_name = sample_type
  )

  if (!is.null(id)) {
    .validate_variables(
      data,
      id,
      data_name = sample_type
    )
  }

  if (is.null(variables)) {
    variables <- names(data)
  } else {
    .validate_variables(
      data,
      variables,
      data_name = sample_type
    )
  }

  if (!is.list(metadata)) {
    stop(
      "'metadata' must be a list.",
      call. = FALSE
    )
  }

  object <- list(
    data = data,
    sample_type = sample_type,
    id = id,
    variables = variables,
    metadata = metadata
  )

  class(object) <- c(
    "tsci_data",
    "list"
  )

  object
}

.build_design_object <- function(data_object,
                                 weights = NULL,
                                 id = NULL,
                                 strata = NULL,
                                 clusters = NULL,
                                 fpc = NULL) {

  if (!inherits(data_object, "tsci_data")) {
    stop(
      "'data_object' must be a 'tsci_data' object.",
      call. = FALSE
    )
  }

  n <- nrow(data_object$data)

  if (!is.null(weights)) {
    .validate_weights(
      weights,
      n = n
    )
  }

  if (!is.null(id)) {
    .validate_variables(
      data_object$data,
      id,
      data_name = data_object$sample_type
    )
  }

  if (!is.null(strata)) {
    .validate_variables(
      data_object$data,
      strata,
      data_name = data_object$sample_type
    )
  }

  if (!is.null(clusters)) {
    .validate_variables(
      data_object$data,
      clusters,
      data_name = data_object$sample_type
    )
  }

  if (!is.null(fpc)) {

    if (!is.numeric(fpc)) {
      stop(
        "'fpc' must be numeric.",
        call. = FALSE
      )
    }

    if (length(fpc) != n) {
      stop(
        "'fpc' must have the same length as the number of observations.",
        call. = FALSE
      )
    }

    if (anyNA(fpc) || any(!is.finite(fpc))) {
      stop(
        "'fpc' contains missing or non-finite values.",
        call. = FALSE
      )
    }

    if (any(fpc <= 0)) {
      stop(
        "'fpc' must contain strictly positive values.",
        call. = FALSE
      )
    }
  }

  object <- list(
    data = data_object,
    weights = weights,
    id = id,
    strata = strata,
    clusters = clusters,
    fpc = fpc
  )

  class(object) <- c(
    "tsci_design",
    "list"
  )

  object
}


#' Print a tsci_data object
#'
#' @param x A `tsci_data` object.
#' @param ... Additional arguments.
#'
#' @export
print.tsci_data <- function(x, ...) {

  cat("<tsci_data>\n")
  cat("Sample type:", x$sample_type, "\n")
  cat("Observations:", nrow(x$data), "\n")
  cat("Variables:", length(x$variables), "\n")

  invisible(x)
}


#' Print a tsci_design object
#'
#' @param x A `tsci_design` object.
#' @param ... Additional arguments.
#'
#' @export
print.tsci_design <- function(x, ...) {

  cat("<tsci_design>\n")
  cat("Sample type:", x$data$sample_type, "\n")
  cat("Observations:", nrow(x$data$data), "\n")

  if (is.null(x$weights)) {
    cat("Weights: not specified\n")
  } else {
    cat("Weights: specified\n")
  }

  invisible(x)
}
