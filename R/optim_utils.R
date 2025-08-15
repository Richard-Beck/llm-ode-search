# Ensure required packages are installed
# install.packages(c("signal", "rxode2", "dplyr"))

library(signal)
library(rxode2)
library(dplyr)


# ---------------------------------------------------------------------------
## 1. get_deriv() - Estimate Derivatives from Time Series Data
# ---------------------------------------------------------------------------
#' @description Approximates derivatives and returns a structured list of data frames.
#'
#' @param data A data.frame with columns for time, replicate, condition, and state variables.
#' @param method The differentiation method. One of "savitzky-golay" (default),
#'   "finite-difference", or "spline".
#' @param ... Additional arguments passed to the specific differentiation method.
#'
#' @return A list with three data.frames:
#'   - X: The original state variable values.
#'   - dX: The estimated derivatives, with column names matching X.
#'   - meta: The time, replicate, and condition columns.
get_deriv <- function(data, method = "savitzky-golay", ...) {
  # Identify metadata and state variable columns
  meta_cols <- c("time", "replicate", "condition")
  vars <- setdiff(names(data), meta_cols)
  
  # Define a helper function to compute derivatives for a single group
  compute_group_deriv <- function(df, .method, .vars, .args) {
    if (nrow(df) < 3) return(NULL) # Cannot compute reliable derivatives
    
    df <- df[order(df$time), ]
    dt <- mean(diff(df$time))
    
    # Create a data frame to hold the derivatives for this group
    dX_group <- as.data.frame(matrix(NA, nrow = nrow(df), ncol = length(.vars)))
    names(dX_group) <- .vars
    
    for (i in seq_along(.vars)) {
      var <- .vars[i]
      y <- df[[var]]
      
      deriv_y <- switch(
        .method,
        "savitzky-golay" = {
          sg_args <- list(p = 3, n = min(7, nrow(df) - (nrow(df) %% 2 == 0)))
          user_args <- .args
          sg_args[names(user_args)] <- user_args
          do.call(sgolayfilt, c(list(x = y, m = 1), sg_args)) / dt
        },
        "finite-difference" = {
          d <- c(NA, diff(y) / diff(df$time))
          d[1] <- d[2]
          d
        },
        "spline" = {
          fit <- do.call(smooth.spline, c(list(x = df$time, y = y), .args))
          predict(fit, x = df$time, deriv = 1)$y
        },
        stop("Invalid method specified.")
      )
      dX_group[[i]] <- deriv_y
    }
    # Return a list of the processed data frames for this group
    list(
      X = df[, .vars, drop = FALSE],
      dX = dX_group,
      meta = df[, meta_cols, drop = FALSE]
    )
  }
  
  # Group data, apply the function, and then process the results
  processed_groups <- data %>%
    group_by(replicate, condition) %>%
    do(proc = compute_group_deriv(., .method = method, .vars = vars, .args = list(...)))
  
  # Combine the results from all groups into the final list structure
  # The na.omit is crucial to handle groups that were too small to process
  valid_results <- na.omit(processed_groups$proc)
  
  list(
    X = do.call(rbind, lapply(valid_results, `[[`, "X")),
    dX = do.call(rbind, lapply(valid_results, `[[`, "dX")),
    meta = do.call(rbind, lapply(valid_results, `[[`, "meta"))
  )
}

# ---------------------------------------------------------------------------
## 2. optim_deriv() - Fit a Model to Estimated Derivatives
# ---------------------------------------------------------------------------
#' @description Finds the optimal parameters for a candidate model by minimizing
#' the sum of squared errors between its derivative output and pre-computed derivatives.
#'
#' @param p0 A named vector of initial parameter guesses.
#' @param model A list describing the ODE system, with fields:
#'   - states: A character vector of state variable names.
#'   - param_names: A character vector of parameter names.
#'   - deriv: A function(x, theta) that computes the derivatives.
#' @param X A data.frame of state variable observations.
#' @param dX A data.frame of the corresponding estimated derivatives.
#'
#' @return The output of the `optim()` function.
optim_deriv <- function(upper,lower, model, X, dX,ntest=50) {
  # Objective function: calculate sum of squared errors
  objective_fn <- function(params) {
   # params <- exp(params)
    # Compute the model's derivative predictions for all observations
    dX_hat <- t(apply(X, 1, function(row) {
      model$deriv(as.list(row), as.list(params))
    }))
    
    # Calculate Sum of Squared Errors
    sse <- unlist((dX_hat - dX)^2)
    sse[!is.finite(sse)] <- 10^9
    sum(sse)
  }
  
  pred_fn <- function(params){
#    params <- exp(params)
    # Compute the model's derivative predictions for all observations
    dX_hat <- t(apply(X, 1, function(row) {
      model$deriv(as.list(row), as.list(params))
    }))
    dX_hat
  }
  
  ## try multiple random initial params for robustness and stability.
  p0_test <- lapply(1:ntest,function(i){
    p0 <- runif(length(upper),min = lower,max=upper)
    names(p0) <- names(upper)
    p0
  })
  p0_eval <- sapply(p0_test,function(i) objective_fn(i))
  
  p0 <- p0_test[[which.min(p0_eval)]]
  
  
  # Run the optimization
  res <- optim(
    par = p0,
    fn = objective_fn,
    method = "BFGS"
  )
  dX_hat <- pred_fn(res$par)  
  #res$par <- exp(res$par)
  list(opt=res,dX_hat=dX_hat)
  
  
}


# ---------------------------------------------------------------------------
## 3. optim_integral() - Fit a Model Using Full Integration
# ---------------------------------------------------------------------------
#' @description Finds optimal parameters by fitting the integrated ODE model
#' directly to the time-series data, handling multiple replicates.
#'
#' @param p0 A named vector of initial parameter guesses.
#' @param model A character string defining the model in rxode2 syntax.
#' @param X A data.frame with columns for replicate, time, and state variables.
#'
#' @return A list containing the optim() result and a data.frame of predictions.
optim_integral <- function(p0, model, X) {
  # Compile the rxode2 model once for efficiency
  ode_model <- rxode2(model)
  state_vars <- rxode2::rxModelVars(ode_model)$state
  
  # Split data by replicate for efficient processing
  replicate_data <- split(X, X$replicate)
  
  # Objective function: calculate total SSE across all replicates
  objective_fn <- function(params, ode_model, replicate_data, state_vars) {
    total_sse <- 0
    
    for (rep_df in replicate_data) {
      sse <- tryCatch({
        # "First observation as initial condition" approach
        first_obs <- rep_df[which.min(rep_df$time), ]
        
        # Set up the event table for initial conditions
        initial_conditions <- setNames(as.numeric(first_obs[state_vars]), state_vars)
        events <- eventTable() %>%
          add.sampling(rep_df$time) 
        
        # Solve the ODE for the time points present in this replicate's data
        sim <- rxSolve(ode_model, params = params, events = events, inits=initial_conditions)
        
        sim <- sim[order(sim$time),]
        rep_df <- rep_df[order(rep_df$time),]
        
        errs <- unlist(sim[,state_vars]-rep_df[,state_vars])
        
        # Calculate SSE for this replicate and add to total
        sse <- sum(errs^2)
        sse
      },error=function(e) return(10^12))
      total_sse <- total_sse + sse
    }
    print(total_sse)
    return(total_sse)
  }
  
  # Run the optimization
  opt_result <- optim(
    par = p0,
    fn = objective_fn,
    ode_model = ode_model,
    replicate_data = replicate_data,
    state_vars = state_vars,
    method = "BFGS"
  )
  
  # Generate final predictions with optimal parameters
  best_params <- opt_result$par
  
  all_predictions <- lapply(replicate_data, function(rep_df) {
    first_obs <- rep_df[which.min(rep_df$time), ]
    initial_conditions <- setNames(as.numeric(first_obs[state_vars]), state_vars)
    events <- eventTable() %>%
      add.sampling(rep_df$time) 
    
    sim <- rxSolve(ode_model, params = best_params, events = events, inits = initial_conditions)
    as.data.frame(sim) %>% mutate(replicate = first_obs$replicate)
  })
  
  Xhat <- do.call(rbind, all_predictions)
  
  list(optimization_result = opt_result, predictions = Xhat)
}