#' Quota share treaty summary
#'
#' This function applies a simple quota share (QS) treaty to the portfolio summary.
#' This is deliberately simple: ceded premium and ceded losses are
#' proportional to the quota share percentage.
#'
#' @param portfolio A data frame / tibble with exposure and incurred_loss.
#' @param share Numeric between 0 and 1. The quota share cession percentage,
#'   e.g. 0.3 for a 30% quota share.
#'
#' @return A tibble with portfolio KPIs and corresponding ceded KPIs.
#' @export
simulate_quota_share <- function(portfolio, share = 0.3) {
  if (!is.numeric(share) || length(share) != 1L || share < 0 || share > 1) {
    stop("`share` must be a single numeric value between 0 and 1.")
  }
  
  summary <- summarise_portfolio(portfolio)
  
  # Here we treat exposure as a proxy for premium. 
  summary$ceded_exposure <- summary$total_exposure * share
  summary$ceded_incurred <- summary$total_incurred * share
  summary$ceded_loss_ratio <- summary$ceded_incurred / summary$ceded_exposure
  
  summary
}
