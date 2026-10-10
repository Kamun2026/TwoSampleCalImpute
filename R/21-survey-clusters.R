#' Extract the cluster variable
#'
#' Extracts the name of the cluster variable from a
#' `tsci_design` object.
#'
#' @param design A `tsci_design` object.
#'
#' @return A character string containing the cluster variable name,
#'   or `NULL` if no clusters were specified.
#' @export
tsci_clusters <- function(design) {

  if (!inherits(design, "tsci_design")) {
    stop(
      "'design' must be a 'tsci_design' object.",
      call. = FALSE
    )
  }

  clusters <- design$clusters

  if (is.null(clusters)) {
    return(NULL)
  }

  if (!is.character(clusters) ||
      length(clusters) != 1L ||
      is.na(clusters) ||
      !nzchar(clusters)) {
    stop(
      "The stored clusters specification is invalid.",
      call. = FALSE
    )
  }

  if (!clusters %in% names(design$data$data)) {
    stop(
      sprintf(
        "The cluster variable '%s' is not present in the design data.",
        clusters
      ),
      call. = FALSE
    )
  }

  clusters
}
