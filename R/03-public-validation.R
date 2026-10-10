#' Validate a survey data set
#'
#' Performs the basic data-integrity checks required before
#' Two-Sample Calibrated Imputation analysis.
#'
#' @param data A data frame to validate.
#' @param data_name A character label used in validation messages.
#' @param variables Optional character vector of variables to validate.
#' @param weights Optional survey weights.
#' @param key Optional character vector identifying records.
#'
#' @return A list containing validation results.
#'
#' @export
tsci_validate_data <- function(data,
                                data_name = "data",
                                variables = NULL,
                                weights = NULL,
                                key = NULL) {

  .validate_data(
    data = data,
    data_name = data_name
  )

  if (!is.null(variables)) {
    .validate_variables(
      data = data,
      variables = variables,
      data_name = data_name
    )
  }

  if (!is.null(weights)) {
    .validate_weights(
      weights = weights,
      n = nrow(data),
      data_name = "weights"
    )
  }

  missing_summary <- .check_missing(data)

  duplicate_summary <- .check_duplicates(
    data = data,
    key = key
  )

  result <- list(
    valid = TRUE,
    data_name = data_name,
    n = nrow(data),
    p = ncol(data),
    variables = names(data),
    missing = missing_summary,
    duplicates = duplicate_summary
  )

  class(result) <- c(
    "tsci_validation",
    "list"
  )

  result
}

