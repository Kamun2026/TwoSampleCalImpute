#' Export survey data
#'
#' Exports a `tsci_data` object to a CSV file.
#'
#' @param data A `tsci_data` object.
#' @param file Character path for the output CSV file.
#' @param row_names Logical; whether row names should be written.
#'
#' @return Invisibly returns the input `tsci_data` object.
#'
#' @export
tsci_export_data <- function(data,
                              file,
                              row_names = FALSE) {

  if (!inherits(data, "tsci_data")) {
    stop(
      "'data' must be a 'tsci_data' object.",
      call. = FALSE
    )
  }

  if (!is.character(file) || length(file) != 1L || is.na(file)) {
    stop(
      "'file' must be a single character path.",
      call. = FALSE
    )
  }

  if (!is.logical(row_names) || length(row_names) != 1L || is.na(row_names)) {
    stop(
      "'row_names' must be a single TRUE or FALSE value.",
      call. = FALSE
    )
  }

  extension <- tolower(tools::file_ext(file))

  if (extension != "csv") {
    stop(
      "Only CSV files are currently supported.",
      call. = FALSE
    )
  }

  utils::write.csv(
    data$data,
    file = file,
    row.names = row_names
  )

  invisible(data)
}

