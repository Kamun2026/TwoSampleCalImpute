#' Extract the finite-population correction
#'
#' Extracts the finite-population-correction values from a
#' `tsci_design` object.
#'
#' @param design A `tsci_design` object.
#'
#' @return A numeric vector containing the finite-population-correction
#'   values, or `NULL` if no FPC was specified.
#'
#' @export
tsci_fpc <- function(design) {

  if (!inherits(design, "tsci_design")) {
    stop(
      "'design' must be a 'tsci_design' object.",
      call. = FALSE
    )
  }

  fpc <- design$fpc

  if (is.null(fpc)) {
    return(NULL)
  }

  if (!is.numeric(fpc)) {
    stop(
      "The stored FPC specification must be numeric.",
      call. = FALSE
    )
  }

  if (length(fpc) != nrow(design$data$data)) {
    stop(
      "The stored FPC must have the same length as the number of observations.",
      call. = FALSE
    )
  }

  if (anyNA(fpc) || any(!is.finite(fpc))) {
    stop(
      "The stored FPC contains missing or non-finite values.",
      call. = FALSE
    )
  }

  if (any(fpc <= 0)) {
    stop(
      "The stored FPC must contain strictly positive values.",
      call. = FALSE
    )
  }

  fpc
}
