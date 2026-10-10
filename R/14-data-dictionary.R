#' Create a data dictionary for a TSCI data object
#'
#' Produces a structured data dictionary for variables in a
#' `tsci_data` object.
#'
#' @param data A `tsci_data` object.
#'
#' @return A data frame containing variable-level metadata.
#' @export

tsci_data_dictionary <- function(data) {
  if (!inherits(data, "tsci_data")) {
    stop("'data' must be a 'tsci_data' object.", call. = FALSE)
  }

  variables <- names(data$data)
  metadata <- data$metadata

  description <- rep(NA_character_, length(variables))

  if (is.list(metadata) && !is.null(metadata$description)) {
    if (is.character(metadata$description) &&
        !is.null(names(metadata$description))) {
      matched <- variables %in% names(metadata$description)
      description[matched] <- unname(
        metadata$description[variables[matched]]
      )
    }
  }

  data.frame(
    variable = variables,
    class = vapply(
      data$data,
      function(x) paste(class(x), collapse = ", "),
      character(1L)
    ),
    n = nrow(data$data),
    missing = vapply(
      data$data,
      function(x) sum(is.na(x)),
      integer(1L)
    ),
    description = description,
    stringsAsFactors = FALSE
  )
}
