#' Subset a TSCI data object
#'
#' Subsets observations in a `tsci_data` object using a logical
#' condition while preserving the TSCI data-object structure.
#'
#' @param data A `tsci_data` object.
#' @param subset A logical vector identifying observations to retain.
#'
#' @return A `tsci_data` object containing the selected observations.
#' @export

tsci_subset_data <- function(data, subset) {
  if (!inherits(data, "tsci_data")) {
    stop("'data' must be a 'tsci_data' object.", call. = FALSE)
  }

  if (!is.logical(subset) || length(subset) != nrow(data$data) ||
      anyNA(subset)) {
    stop(
      "'subset' must be a logical vector of length equal to the number of observations and contain no NA values.",
      call. = FALSE
    )
  }

  subsetted_data <- data$data[subset, , drop = FALSE]

  .build_data_object(
    data = subsetted_data,
    sample_type = data$sample_type,
    id = data$id,
    variables = intersect(data$variables, names(subsetted_data)),
    metadata = data$metadata
  )
}
