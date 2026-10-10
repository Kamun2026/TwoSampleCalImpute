#' Summarize missing values in a TSCI data object
#'
#' Produces a variable-level summary of missing observations in a
#' `tsci_data` object.
#'
#' @param data A `tsci_data` object.
#' @param variables Optional character vector of variables to summarize.
#'   If `NULL`, all variables are summarized.
#'
#' @return A data frame containing missingness summaries.
#' @export

tsci_missing_summary <- function(data, variables = NULL) {
  if (!inherits(data, "tsci_data")) {
    stop("'data' must be a 'tsci_data' object.", call. = FALSE)
  }

  available_variables <- names(data$data)

  if (is.null(variables)) {
    variables <- available_variables
  } else {
    if (!is.character(variables) || length(variables) < 1L ||
        anyNA(variables) || any(!nzchar(variables))) {
      stop("'variables' must be a non-empty character vector or NULL.",
           call. = FALSE)
    }

    if (!all(variables %in% available_variables)) {
      missing_variables <- variables[!variables %in% available_variables]
      stop(
        sprintf(
          "Variables not found: %s.",
          paste(missing_variables, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  n <- nrow(data$data)

  summary_list <- lapply(
    variables,
    function(variable) {
      missing_count <- sum(is.na(data$data[[variable]]))

      data.frame(
        variable = variable,
        n = n,
        missing = missing_count,
        observed = n - missing_count,
        missing_percent = if (n > 0L) 100 * missing_count / n else 0,
        stringsAsFactors = FALSE
      )
    }
  )

  do.call(rbind, summary_list)
}
