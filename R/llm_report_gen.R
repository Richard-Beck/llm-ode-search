generate_llm_diagnostic_report <- function(dX, dX_hat, X) {
  dX <- data.frame(dX)
  X <- data.frame(X)
  dX_hat <- data.frame(dX_hat)
  state_vars <- names(dX)
  report <- ""
  
  for (var in state_vars) {
    # --- Part 1: Predicted vs. Estimated Analysis ---
    # This part remains a good overall summary for each variable
    fit <- lm(dX_hat[, var] ~ dX[, var])
    coeffs <- summary(fit)$coefficients
    r_squared <- summary(fit)$r.squared
    
    report <- paste0(report,
                     sprintf("## Diagnostic Report for Derivative of '%s'\n\n", var),
                     "**1. Analysis of Predicted vs. Estimated Values (Overall Fit):**\n",
                     sprintf("* **Intercept:** %.3f (Systematic bias from ideal of 0)\n", coeffs[1, 1]),
                     sprintf("* **Slope:** %.3f (Scaling error from ideal of 1)\n", coeffs[2, 1]),
                     sprintf("* **R-squared:** %.3f (Strength of linear relationship)\n\n", r_squared))
    
    # --- Part 2: PAIRWISE Residuals vs. System State Analysis ---
    residuals <- dX[, var] - dX_hat[, var]
    report <- paste0(report, "**2. Pairwise Analysis of Residuals vs. Each State Variable:**\n")
    
    # This inner loop now explicitly checks the current residual 'var'
    # against the state of every variable in the system 'state_var_check'.
    for (state_var_check in names(X)) {
      report <- paste0(report, sprintf("  * **Analyzing residuals of `%s` vs. state of `%s`:**\n", var, state_var_check))
      
      # Bin the state variable and calculate residual SD in each bin
      # Added a check for constant state variables which would cause quantile() to fail
      if (length(unique(X[, state_var_check])) > 5) {
        
        breaks <- try(quantile(X[, state_var_check], probs = 0:5/5, na.rm = TRUE), silent = TRUE)
        if (inherits(breaks, "try-error") || length(unique(breaks)) < 2) {
          report <- paste0(report, "      - Could not bin state variable (likely constant or too few unique values).\n")
          next
        }
        
        bins <- cut(X[, state_var_check], breaks = breaks, include.lowest = TRUE)
        
        if (length(unique(bins)) > 1) {
          binned_sd <- tapply(residuals, bins, sd, na.rm = TRUE)
          
          report <- paste0(report, "      - Trend in residual standard deviation across bins:\n")
          for (i in seq_along(binned_sd)) {
            if(!is.na(binned_sd[i])) {
              report <- paste0(report, sprintf("          - Bin %d (%s): %.3f\n", i, names(binned_sd)[i], binned_sd[i]))
            }
          }
        }
      } else {
        report <- paste0(report, "      - Not enough unique values in state variable to bin.\n")
      }
    }
    report <- paste0(report, "\n")
  }
  
  #cat(report)
  invisible(report)
}

library(randomForest)
library(mgcv)
library(dplyr)

generate_llm_discovery_report <- function(dX, X, n_bins = 4) {
  state_vars <- names(dX)
  report <- "# Rich Model Discovery Report\n\n"
  report <- paste0(report, "This report analyzes relationships between state variables (X) and their derivatives (dX) to infer the functional forms of the underlying system.\n\n---\n\n")
  
  for (target_var in state_vars) {
    report <- paste0(report, sprintf("## Analysis for Derivative of '%s' (d.%s)\n", target_var, target_var))
    
    # --- 1. Identify Key Influencers (Unchanged) ---
    rf_model <- randomForest(x = X, y = dX[[target_var]], ntree = 100, importance = TRUE)
    importance_matrix <- importance(rf_model, type = 1)
    sorted_importance <- sort(importance_matrix[, 1], decreasing = TRUE)
    
    report <- paste0(report, "**1. Key Influencers (from Random Forest):**\n")
    for (i in seq_along(sorted_importance)) {
      report <- paste0(report, sprintf("* **%s:** Importance (%%IncMSE) = %.2f%%\n", names(sorted_importance)[i], sorted_importance[i]))
    }
    report <- paste0(report, "\n")
    
    # --- 2. Characterize Shape of Pairwise Relationships (ROBUST FIX) ---
    report <- paste0(report, "**2. Shape of Influence for Each Predictor:**\n")
    for (predictor_var in names(X)) {
      report <- paste0(report, sprintf("  * **Influence of `%s` on `d.%s`:**\n", predictor_var, target_var))
      
      # Create a temporary, clean data frame for fitting
      fit_df <- data.frame(y = dX[[target_var]], x = X[[predictor_var]])
      
      # Fit the GAM using these simple, reliable names
      gam_fit <- try(gam(y ~ s(x), data = fit_df), silent = TRUE)
      
      if (inherits(gam_fit, "try-error")) {
        report <- paste0(report, "      - Could not model relationship.\n")
        next
      }
      
      # Bin the original predictor data
      predictor_vals <- X[[predictor_var]]
      if(length(unique(predictor_vals)) < n_bins) {
        report <- paste0(report, "      - Not enough unique predictor values to bin.\n")
        next
      }
      
      breaks <- unique(quantile(predictor_vals, probs = 0:n_bins/n_bins, na.rm = TRUE))
      if(length(breaks) < 2) {
        report <- paste0(report, "      - Could not create bins for predictor.\n")
        next
      }
      bins <- cut(predictor_vals, breaks = breaks, include.lowest = TRUE)
      binned_means <- tapply(predictor_vals, bins, mean, na.rm = TRUE)
      
      # Create a clean data frame for prediction with the same simple name 'x'
      newdata_for_pred <- data.frame(x = binned_means)
      
      binned_preds <- predict(gam_fit, newdata = newdata_for_pred)
      
      # Describe the trend
      trend_desc <- "The predicted rate shows a"
      if (all(diff(binned_preds) > 0, na.rm = TRUE)) {
        trend_desc <- paste(trend_desc, "monotonic increasing (activating) trend.")
      } else if (all(diff(binned_preds) < 0, na.rm = TRUE)) {
        trend_desc <- paste(trend_desc, "monotonic decreasing (inhibiting) trend.")
      } else {
        trend_desc <- paste(trend_desc, "complex non-monotonic trend.")
      }
      
      report <- paste0(report, sprintf("      - **Trend Shape:** %s\n", trend_desc))
      report <- paste0(report, "      - **Predicted rate across bins of predictor:**\n")
      for(i in seq_along(binned_preds)) {
        report <- paste0(report, sprintf("        - Bin %d (%s): %.3f\n", i, names(binned_preds)[i], binned_preds[i]))
      }
    }
    
    # --- 3. Test for Key Interaction Effects (Unchanged) ---
    # ... (rest of the function is the same) ...
    report <- paste0(report, "\n**3. Analysis of Interaction Effects:**\n")
    top_influencers <- names(sorted_importance)[1:min(2, length(sorted_importance))]
    
    if (length(top_influencers) == 2) {
      v1 <- top_influencers[1]
      v2 <- top_influencers[2]
      
      additive_model <- lm(dX[[target_var]] ~ X[[v1]] + X[[v2]])
      interactive_model <- lm(dX[[target_var]] ~ X[[v1]] * X[[v2]])
      
      anova_test <- anova(additive_model, interactive_model)
      p_value <- anova_test$"Pr(>F)"[2]
      
      interaction_conclusion <- ifelse(p_value < 0.05, 
                                       sprintf("YES, the interaction is statistically significant (p=%.4f).", p_value),
                                       sprintf("NO, a simple additive model is likely sufficient (p=%.4f).", p_value))
      
      report <- paste0(report, sprintf("  * **Testing for interaction between top influencers (`%s` * `%s`):** %s\n", v1, v2, interaction_conclusion))
    } else {
      report <- paste0(report, "  - Not enough significant influencers to test for interactions.\n")
    }
    
    # --- 4. Baseline Rate (Unchanged) ---
    full_lm <- lm(dX[[target_var]] ~ ., data = X)
    intercept <- coef(full_lm)[1]
    report <- paste0(report, sprintf("\n**4. Estimated Baseline Rate (Intercept):** %.4f\n", intercept))
    report <- paste0(report, "This suggests a constant production (> 0) or decay (< 0) term.\n")
    
    report <- paste0(report, "\n---\n")
  }
  
  #cat(report)
  invisible(report)
}