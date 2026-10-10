#' Extract survey weights
#'
#' Extracts the numeric survey weights from a `tsci_design` object.
#'
#' @param design A `tsci_design` object.
#'
#' @return A numeric vector containing the survey weights.
#' @export
tsci_weights <- function(design) {

  if (!inherits(design, "tsci_design")) {
    stop("'design' must be a 'tsci_design' object.", call. = FALSE)
  }

  if (is.null(design$weights)) {
    stop("The survey design does not contain survey weights.",
         call. = FALSE)
  }

  weights <- design$weights

  if (!is.numeric(weights)) {
    stop("Survey weights must be numeric.", call. = FALSE)
  }

  if (anyNA(weights) || any(!is.finite(weights))) {
    stop("Survey weights contain missing or non-finite values.",
         call. = FALSE)
  }

  if (any(weights <= 0)) {
    stop("Survey weights must be strictly positive.",
         call. = FALSE)
  }

  weights
}
