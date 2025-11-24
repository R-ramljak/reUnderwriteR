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
  
  # Create a temporary environment to load data into
  data_env <- new.env(parent = emptyenv())
  
  # Use utils::data() to load datasets from CASdatasets into this environment
  utils::data("freMTPL2freq", package = "CASdatasets", envir = data_env)
  utils::data("freMTPL2sev",  package = "CASdatasets", envir = data_env)
  
  # Extract them from the temporary environment
  freq <- data_env$freMTPL2freq
  sev  <- data_env$freMTPL2sev
  
  # Safety checks
  if (is.null(freq)) stop("Failed to load 'freMTPL2freq' from CASdatasets.")
  if (is.null(sev))  stop("Failed to load 'freMTPL2sev' from CASdatasets.")
  
  # Aggregate claim amounts at policy level
  claims_by_policy <- dplyr::group_by(sev, IDpol) |>
    dplyr::summarise(
      incurred_loss = sum(ClaimAmount, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Select a simple set of risk features from the frequency dataset
  portfolio_raw <- freq |>
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
