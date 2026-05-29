# code/utils/helper_functions.R
# ------------------------------------------------------------
# Helper functions for data preparation: Countries
# ------------------------------------------------------------

#### Party-level: Function to extract effects for a given focal party family ----

extract_effects <- function(fit, pe_cols, focal_family, predictor = "intercept") {
  
  # --- Relabeling helper ---
  relabel_pe_cols <- function(cols, focal) {
    sapply(cols, function(c) {
      if (c == "residual") {
        paste0(focal, " (residual)")
      } else if (grepl("_party$", c)) {
        # inflows: X_party = inflow into focal
        orig <- sub("_party$", "", c)
        paste0(toupper(orig), " → ", focal, " (inflow)")
      } else if (grepl("^party_", c)) {
        # outflows: party_X = outflow from focal
        dest <- sub("^party_", "", c)
        paste0(focal, " → ", toupper(dest), " (outflow)")
      } else {
        c
      }
    })
  }
  
  # --- Build predictor map ---
  pred_map <- c(intercept = 1)
  if (!is.null(fit$main_predictor)) pred_map[fit$main_predictor] <- 2
  if (!is.null(fit$moderator))      pred_map[fit$moderator]      <- 3
  
  if (! predictor %in% names(pred_map)) {
    stop("Unknown predictor: ", predictor, 
         ". Available: ", paste(names(pred_map), collapse = ", "))
  }
  predictor_col <- pred_map[[predictor]]
  
  # --- Create beta map ---
  beta_map <- data.frame(
    beta_index = paste0("beta[", seq_along(pe_cols), ",", predictor_col, "]"),
    transition = pe_cols,
    label = relabel_pe_cols(pe_cols, focal_family),
    stringsAsFactors = FALSE
  )
  
  # --- Extract posterior summary ---
  beta_summary <- rstan::summary(fit$estimates, pars = "beta",
                                 probs = c(0.025, 0.975))$summary
  beta_summary_df <- as.data.frame(beta_summary)
  beta_summary_df$beta_index <- rownames(beta_summary_df)
  
  # --- Merge labels ---
  beta_labeled <- merge(beta_map, beta_summary_df, by = "beta_index")
  
  # --- Add focal family & predictor info ---
  beta_labeled$focal_family <- focal_family
  beta_labeled$predictor <- predictor
  
  # --- Add significance flag (excludes 0) ---
  beta_labeled$significant <- with(beta_labeled, (`2.5%` > 0 & `97.5%` > 0) |
                                     (`2.5%` < 0 & `97.5%` < 0))
  
  beta_labeled[, c("focal_family", "predictor", "label",
                   "mean", "sd", "2.5%", "97.5%", "n_eff", "Rhat", "significant")]
}