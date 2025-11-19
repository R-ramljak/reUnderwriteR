#' Summarise portfolio loss experience
#'
#' This function computes high-level KPIs for a motor portfolio: total exposure,
#' total incurred loss and an implied loss ratio. 
#'
#' @param portfolio A data frame / tibble with at least
#'   \code{exposure} and \code{incurred_loss} columns.
#'
#' @return A tibble with one row and summary metrics.
#' @export
summarise_portfolio <- function(portfolio) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required but not installed.")
  }
  
  # In this simple version we treat exposure as a proxy for premium volume.
  dplyr::summarise(
    portfolio,
    total_exposure    = sum(.data$exposure, na.rm = TRUE),
    total_incurred    = sum(.data$incurred_loss, na.rm = TRUE),
    # Simple "loss ratio" proxy = incurred / exposure
    loss_ratio        = total_incurred / total_exposure
  )
}
