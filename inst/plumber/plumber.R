# plumber.R
# Minimal API for reUnderwriteR:
# - GET /health: health check
# - POST /treaty/quota_share: compute simple quota share metrics

library(plumber)
library(jsonlite)
library(tibble)
library(reUnderwriteR)

#* Health check endpoint
#* @get /health
function() {
  list(
    status = "ok",
    package = "reUnderwriteR",
    timestamp = as.character(Sys.time())
  )
}

#* Simple quota share treaty calculation
#*
#* Expects a JSON body with:
#* {
#*   "portfolio": [
#*     {"policy_id": 1, "exposure": 0.8, "incurred_loss": 1000},
#*     {"policy_id": 2, "exposure": 1.0, "incurred_loss": 0}
#*   ],
#*   "share": 0.3
#* }
#*
#* Returns portfolio KPIs and ceded KPIs.
#*
#* @post /treaty/quota_share
function(req) {
  # Parse JSON body
  body <- tryCatch(
    jsonlite::fromJSON(req$postBody),
    error = function(e) {
      stop("Invalid JSON body: ", e$message)
    }
  )
  
  # Basic validation
  if (is.null(body$portfolio)) {
    stop("Body must contain a 'portfolio' field.")
  }
  if (is.null(body$share)) {
    stop("Body must contain a 'share' field.")
  }
  
  # Convert portfolio list to tibble
  portfolio <- tibble::as_tibble(body$portfolio)
  
  # Check required columns
  required_cols <- c("exposure", "incurred_loss")
  missing_cols <- setdiff(required_cols, names(portfolio))
  if (length(missing_cols) > 0) {
    stop(
      "Portfolio is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  share <- as.numeric(body$share)
  
  # Call your package function
  result <- reUnderwriteR::simulate_quota_share(portfolio, share)
  
  # Return as plain list (plumber will convert to JSON)
  result
}
