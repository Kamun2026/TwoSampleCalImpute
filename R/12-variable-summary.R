#' Summarize variables in a TSCI data object
#'
#' Produces descriptive summaries for selected variables in a
#' `tsci_data` object.
#'
#' @param data A `tsci_data` object.
#' @param variables Optional character vector of variables to summarize.
#'   If `NULL`, all variables are summarized.
#'
#' @return A data frame containing variable-level descriptive summaries.
#' @export

tsci_variable_summary <- function(data, variables = NULL) {
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

  summary_list <- lapply(
    variables,
    function(variable) {
      x <- data$data[[variable]]

      data.frame(
        variable = variable,
        class = paste(class(x), collapse = ", "),
        n = length(x),
        missing = sum(is.na(x)),
        unique = length(unique(x[!is.na(x)])),
        stringsAsFactors = FALSE
      )
    }
  )

  do.call(rbind, summary_list)
}
