#' Summarise survey-design structure for plotting
#'
#' Produces a simple base-R plot describing the survey-design
#' structure of a `tsci_design` object.
#'
#' @param design A `tsci_design` object.
#'
#' @return The input `tsci_design` object invisibly.
#'
#' @export
tsci_design_plot <- function(design) {

  if (!inherits(design, "tsci_design")) {
    stop(
      "'design' must be a 'tsci_design' object.",
      call. = FALSE
    )
  }

  data_object <- design$data
  data_frame <- data_object$data

  n <- nrow(data_frame)

  components <- c(
    "Weights",
    "Strata",
    "Clusters",
    "FPC"
  )

  present <- c(
    !is.null(design$weights),
    !is.null(design$strata),
    !is.null(design$clusters),
    !is.null(design$fpc)
  )

  plot(
    x = seq_along(components),
    y = as.numeric(present),
    type = "h",
    xaxt = "n",
    yaxt = "n",
    xlab = "Survey-design component",
    ylab = "Specified",
    main = "TSCI Survey-Design Structure",
    ylim = c(0, 1.2)
  )

  graphics::axis(
    side = 1,
    at = seq_along(components),
    labels = components
  )

  graphics::axis(
    side = 2,
    at = c(0, 1),
    labels = c("No", "Yes")
  )

  invisible(design)
}
