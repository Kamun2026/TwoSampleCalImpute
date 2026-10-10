#' Validate survey-design information
#'
#' Checks the structural and numerical validity of a `tsci_design`
#' object.
#'
#' @param design A `tsci_design` object.
#'
#' @return A list of validation results with class
#'   `tsci_design_check`.
#' @export
tsci_design_check <- function(design) {

  if (!inherits(design, "tsci_design")) {
    stop("'design' must be a 'tsci_design' object.", call. = FALSE)
  }

  data_object <- design$data
  data_frame <- data_object$data
  n <- nrow(data_frame)

  checks <- list(
    data_object = inherits(data_object, "tsci_data"),
    observations = n > 0L,
    weights = !is.null(design$weights),
    weights_length = !is.null(design$weights) && length(design$weights) == n,
    weights_numeric = !is.null(design$weights) && is.numeric(design$weights),
    weights_finite = !is.null(design$weights) &&
      all(is.finite(design$weights)),
    weights_positive = !is.null(design$weights) &&
      all(design$weights > 0),
    strata = is.null(design$strata) ||
      (is.character(design$strata) &&
       length(design$strata) == 1L &&
       design$strata %in% names(data_frame)),
    fpc = is.null(design$fpc) ||
      (is.numeric(design$fpc) &&
       length(design$fpc) == n &&
       all(is.finite(design$fpc)) &&
       all(design$fpc > 0))
  )

  valid <- all(unlist(checks))

  result <- list(
    valid = valid,
    checks = checks,
    sample_type = data_object$sample_type,
    observations = n,
    id = design$id,
    strata = design$strata,
    has_weights = !is.null(design$weights),
    has_fpc = !is.null(design$fpc)
  )

  class(result) <- c("tsci_design_check", "list")

  result
}
