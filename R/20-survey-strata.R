#' Extract the stratification variable
#'
#' Extracts the name of the stratification variable from a
#' `tsci_design` object.
#'
#' @param design A `tsci_design` object.
#'
#' @return A character string containing the stratification
#'   variable name, or `NULL` if no strata were specified.
#' @export
tsci_strata <- function(design) {

  if (!inherits(design, "tsci_design")) {
    stop("'design' must be a 'tsci_design' object.", call. = FALSE)
  }

  strata <- design$strata

  if (is.null(strata)) {
    return(NULL)
  }

  if (!is.character(strata) ||
      length(strata) != 1L ||
      is.na(strata) ||
      !nzchar(strata)) {
    stop("The stored strata specification is invalid.",
         call. = FALSE)
  }

  if (!strata %in% names(design$data$data)) {
    stop(
      sprintf(
        "The strata variable '%s' is not present in the design data.",
        strata
      ),
      call. = FALSE
    )
  }

  strata
}
