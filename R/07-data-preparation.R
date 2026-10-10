#' Prepare survey data
#'
#' Prepares a `tsci_data` object for subsequent validation and
#' Two-Sample Calibrated Imputation analysis.
#'
#' @param data A `tsci_data` object.
#' @param remove_empty_rows Logical; whether completely empty rows
#' should be removed.
#' @param remove_empty_columns Logical; whether completely empty
#' columns should be removed.
#'
#' @return A prepared `tsci_data` object.
#'
#' @export
tsci_prepare_data <- function(data,
                               remove_empty_rows = FALSE,
                               remove_empty_columns = FALSE) {

  if (!inherits(data, "tsci_data")) {
    stop(
      "'data' must be a 'tsci_data' object.",
      call. = FALSE
    )
  }

  prepared_data <- tsci_clean_data(
    data = data,
    remove_empty_rows = remove_empty_rows,
    remove_empty_columns = remove_empty_columns
  )

  prepared_data
}

