#' Load French MTPL portfolio from CASdatasets
#'
#' This function loads the freMTPL2freq and freMTPL2sev datasets from the
#' CASdatasets package and aggregates them into a simple policy-level portfolio
#' with exposure, risk features and incurred loss.
#'
#' @return A tibble with one row per policy, including exposure, risk features
#'   and total incurred loss (sum of claim amounts per policy).
#' @export
load_cas_portfolio <- function() {
  # Ensure required packages are available
  if (!requireNamespace("CASdatasets", quietly = TRUE)) {
    stop("Package 'CASdatasets' is required but not installed.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required but not installed.")
  }
  
  # Use explicit namespace to avoid NOTE in R CMD check
  data("freMTPL2freq", envir = environment())
  data("freMTPL2sev",  envir = environment())
  
  # Aggregate claim amounts at policy level
  claims_by_policy <- freMTPL2sev |> 
    dplyr::group_by(IDpol) |>
    dplyr::summarise(
      incurred_loss = sum(ClaimAmount, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Select a simple set of risk features from the frequency dataset
  portfolio_raw <- freMTPL2freq |>
    dplyr::select(
      policy_id   = IDpol,
      exposure    = Exposure,
      area        = Area,
      veh_age     = VehAge,
      driv_age    = DrivAge,
      bonus_malus = BonusMalus,
      density     = Density
    )
  
  # Join exposure and risk features with aggregated incurred loss
  portfolio <- dplyr::left_join(
    portfolio_raw,
    claims_by_policy,
    by = c("policy_id" = "IDpol")
  ) |>
    dplyr::mutate(
      # Replace missing incurred loss with zero
      incurred_loss = ifelse(is.na(incurred_loss), 0, incurred_loss)
    )
  
  portfolio
}
