## adapter functions to enable pipeline testing with massonis data - further integration required

summarize_for_llm_agg <- function(df, rep_col="ic_id", time_col="time",
                                  max_lag=10, eps=1e-6) {
  # ---- detect states (x1, x2, ...) ----
  states <- grep("^x[0-9]+$", names(df), value=TRUE)
  stopifnot(length(states) >= 1, rep_col %in% names(df), time_col %in% names(df))
  reps <- sort(unique(df[[rep_col]]))
  R <- length(reps)
  
  # ---- helpers ----
  slope3 <- function(t, y, head=TRUE){
    n <- length(t); k <- min(3, n); if (k < 2) return(NA_real_)
    idx <- if (head) 1:k else (n-k+1):n
    tt <- t[idx]; yy <- y[idx]
    if (length(unique(tt)) < 2) return(NA_real_)
    coef(stats::lm(yy ~ tt))[2]
  }
  n_extrema <- function(t, y){
    if (length(y) < 3) return(0L)
    dy <- diff(y)/diff(t)
    s <- sign(dy); s[abs(dy) < eps] <- 0
    s <- s[s != 0]
    if (length(s) < 2) return(0L)
    sum(diff(s) != 0L)
  }
  best_lag_corr <- function(a, b, L){
    n <- length(a); if (n < 5) return(c(corr=NA_real_, lag=NA_integer_))
    L <- min(L, n %/% 4); if (L < 1) return(c(corr=stats::cor(a,b), lag=0L))
    lags <- -L:L
    cc <- vapply(lags, function(Lg){
      if (Lg < 0)       stats::cor(a[1:(n+Lg)], b[(1-Lg):n])
      else if (Lg > 0)  stats::cor(a[(1+Lg):n], b[1:(n-Lg)])
      else              stats::cor(a, b)
    }, numeric(1))
    k <- which.max(abs(cc))
    c(corr=unname(cc[k]), lag=lags[k])
  }
  medq <- function(x){ x <- x[is.finite(x)]; if (!length(x)) return(c(NA,NA,NA))
  as.numeric(stats::quantile(x, c(.25,.5,.75), na.rm=TRUE)) }
  
  # ---- per-replicate accumulators (kept internal only) ----
  pv <- lapply(states, function(s) list(
    mono=rep(NA_real_, R), ext2=rep(NA_real_, R),
    s_init=rep(NA_real_, R), s_final=rep(NA_real_, R),
    fpos=rep(NA_real_, R),  fneg=rep(NA_real_, R),
    sink=rep(NA_real_, R)
  ))
  names(pv) <- states
  
  ppairs <- if (length(states) >= 2) combn(states, 2, simplify=FALSE) else list()
  pr <- lapply(ppairs, function(p) list(
    rho0=rep(NA_real_, R), bestc=rep(NA_real_, R), bestlag=rep(NA_real_, R)
  ))
  names(pr) <- sapply(ppairs, paste, collapse=":")
  
  # ---- iterate replicates ----
  all_dts <- c(); t0_all <- Inf; t1_all <- -Inf
  for (ri in seq_along(reps)) {
    g <- df[df[[rep_col]] == reps[ri], c(time_col, states), drop=FALSE]
    g <- g[order(g[[time_col]]), , drop=FALSE]
    t <- as.numeric(g[[time_col]])
    if (length(unique(t)) >= 2) all_dts <- c(all_dts, diff(unique(t)))
    t0_all <- min(t0_all, min(t, na.rm=TRUE))
    t1_all <- max(t1_all, max(t, na.rm=TRUE))
    S <- as.matrix(g[, states, drop=FALSE]); n <- nrow(S)
    if (n < 2) next
    dt <- diff(t); dS <- apply(S, 2, function(col) diff(col)/dt)
    C  <- 1 + rowSums(S); Cmid <- C[-n]
    
    # per-variable
    for (vj in seq_along(states)) {
      y  <- S[, vj]
      dy <- dS[, vj]
      pv[[vj]]$mono[ri]   <- as.numeric(all(dy >= -eps) | all(dy <= eps))
      pv[[vj]]$ext2[ri]   <- as.numeric(n_extrema(t, y) >= 2)
      pv[[vj]]$s_init[ri] <- slope3(t, y, TRUE)
      pv[[vj]]$s_final[ri]<- slope3(t, y, FALSE)
      pv[[vj]]$fpos[ri]   <- mean(dy >  eps, na.rm=TRUE)
      pv[[vj]]$fneg[ri]   <- mean(dy < -eps, na.rm=TRUE)
      target <- - S[-n, vj] / Cmid
      pv[[vj]]$sink[ri]   <- suppressWarnings(stats::cor(dy, target, method="spearman", use="complete.obs"))
    }
    
    # per-pair
    if (length(ppairs)) {
      for (pk in seq_along(ppairs)) {
        a <- ppairs[[pk]][1]; b <- ppairs[[pk]][2]
        ya <- S[, a]; yb <- S[, b]
        pr[[pk]]$rho0[ri]   <- suppressWarnings(stats::cor(ya, yb, method="spearman"))
        bl                  <- best_lag_corr(ya, yb, max_lag)
        pr[[pk]]$bestc[ri]  <- bl["corr"]
        pr[[pk]]$bestlag[ri]<- bl["lag"]
      }
    }
  }
  
  # ---- aggregate across replicates (medians & IQRs) ----
  per_variable <- lapply(seq_along(states), function(vj){
    x1 <- pv[[vj]]
    c25_50_75 <- medq(x1$sink)
    list(
      var = states[vj],
      n_reps = sum(is.finite(x1$mono)),
      x_frac_monotone_med   = medq(x1$mono)[2],
      x_frac_extrema_ge2_med= medq(x1$ext2)[2],
      x_slope_initial_med   = medq(x1$s_init)[2],
      x_slope_final_med     = medq(x1$s_final)[2],
      x_frac_dpos_med       = medq(x1$fpos)[2],
      x_frac_dneg_med       = medq(x1$fneg)[2],
      x_sink_corr_med       = c25_50_75[2],
      x_sink_corr_q25       = c25_50_75[1],
      x_sink_corr_q75       = c25_50_75[3]
    )
  })
  
  pairwise <- lapply(seq_along(pr), function(pk){
    p1 <- pr[[pk]]
    r0  <- medq(p1$rho0)
    bc  <- medq(p1$bestc)
    bl  <- medq(p1$bestlag)
    list(
      var_pair = names(pr)[pk],
      n_reps   = sum(is.finite(p1$rho0)),
      rho0_spearman_med = r0[2],
      rho0_q25          = r0[1],
      rho0_q75          = r0[3],
      best_corr_abs_med = stats::median(abs(p1$bestc), na.rm=TRUE),
      best_lag_med_pts  = bl[2],
      best_lag_q25      = bl[1],
      best_lag_q75      = bl[3],
      sign_consistency  = if (all(!is.finite(p1$bestc))) NA_real_
      else mean(sign(p1$bestc) == sign(bc[2]), na.rm=TRUE)
    )
  })
  
  meta <- list(
    n_reps = R,
    t0 = t0_all, t1 = t1_all,
    dt_median = if (length(all_dts)) stats::median(all_dts, na.rm=TRUE) else NA_real_
  )
  
  notes <- list(
    n_reps = "Number of trajectories (replicates) aggregated.",
    t0 = "Earliest observed time across all replicates.",
    t1 = "Latest observed time across all replicates.",
    dt_median = "Median gap between successive unique time points (across all replicates).",
    
    var = "State variable name.",
    x_frac_monotone_med = "Median across replicates of the indicator that dX/dt never changes sign (entirely nonincreasing or nondecreasing).",
    x_frac_extrema_ge2_med = "Median across replicates of an indicator for at least two slope sign changes (proxy for oscillatory behavior).",
    x_slope_initial_med = "Median initial slope estimated by a 3-point linear fit at the beginning of each trajectory.",
    x_slope_final_med = "Median final slope estimated by a 3-point linear fit at the end of each trajectory.",
    x_frac_dpos_med = "Median fraction of time intervals with positive finite-difference derivative.",
    x_frac_dneg_med = "Median fraction of time intervals with negative finite-difference derivative.",
    x_sink_corr_med = "Median Spearman correlation between dX/dt and -X/(1+sum_j X_j) (signature of a shared crowding/dilution sink).",
    x_sink_corr_q25 = "25th percentile of the per-replicate sink correlation.",
    x_sink_corr_q75 = "75th percentile of the per-replicate sink correlation.",
    
    var_pair = "Variable pair identifier A:B.",
    rho0_spearman_med = "Median zero-lag Spearman correlation between the paired variables across replicates.",
    rho0_q25 = "25th percentile of the zero-lag Spearman correlation.",
    rho0_q75 = "75th percentile of the zero-lag Spearman correlation.",
    best_corr_abs_med = "Median across replicates of the maximum absolute Pearson correlation attained over tested lags.",
    best_lag_med_pts  = "Median lag (in index steps) at which the absolute correlation is maximized; positive means the second variable leads.",
    best_lag_q25 = "25th percentile of the best-lag (index steps).",
    best_lag_q75 = "75th percentile of the best-lag (index steps).",
    sign_consistency = "Fraction of replicates where the sign of the best-lag correlation matches the sign of its median across replicates."
  )
  
  obj <- list(meta=meta, per_variable=per_variable, pairwise=pairwise, notes=notes, preview=NULL)
  jsonlite::toJSON(obj, auto_unbox=TRUE, null="null", pretty=TRUE)
}

llm_json_to_candidate <- function(spec) {
  stopifnot(is.list(spec$model),
            is.character(spec$model$ode_text),
            is.character(spec$model$states <- spec$model$states %||% spec$model$state),
            is.character(spec$model$params))
  states <- spec$model$states
  params <- spec$model$params
  ode    <- spec$model$ode_text
  
  # 1) Parse all lines into derivatives and intermediate calculations
  lines <- strsplit(ode, ";", fixed = TRUE)[[1]]
  lines <- trimws(lines[nzchar(lines)])
  rhs_map <- setNames(vector("list", length(states)), states)
  intermediates_map <- list() # Store intermediate variables here
  
  for (ln in lines) {
    # Try to match a derivative line: "d/dt(state) = rhs"
    m_deriv <- regexec("^\\s*d/dt\\(([^)]+)\\)\\s*=\\s*(.*)\\s*$", ln)
    mm_deriv <- regmatches(ln, m_deriv)[[1]]
    
    if (length(mm_deriv) >= 3) {
      rhs_map[[mm_deriv[2]]] <- mm_deriv[3]
    } else {
      # Otherwise, try to match an intermediate variable line: "var = rhs"
      m_inter <- regexec("^\\s*(\\w+)\\s*=\\s*(.*)\\s*$", ln)
      mm_inter <- regmatches(ln, m_inter)[[1]]
      if (length(mm_inter) >= 3) {
        intermediates_map[[mm_inter[2]]] <- mm_inter[3]
      }
    }
  }
  miss <- names(rhs_map)[!nzchar(unlist(rhs_map))]
  if (length(miss)) stop("No RHS found for state(s): ", paste(miss, collapse=", "))
  
  # 2) Token replace states -> x[["..."]], params -> theta[["..."]]
  replace_tokens <- function(expr) {
    for (s in states) expr <- gsub(paste0("\\b", s, "\\b"),
                                   sprintf('x[["%s"]]', s), expr, perl=TRUE)
    for (p in params) expr <- gsub(paste0("\\b", p, "\\b"),
                                   sprintf('theta[["%s"]]', p), expr, perl=TRUE)
    expr
  }
  rhs_sub <- vapply(states, function(s) replace_tokens(rhs_map[[s]]), "", USE.NAMES = TRUE)
  # Also apply token replacement to the intermediate variable expressions
  intermediates_sub <- if (length(intermediates_map) > 0) lapply(intermediates_map, replace_tokens) else list()
  
  # 3) Build the deriv() function source and eval it
  # Function body is now multi-line to accommodate intermediates
  body_lines <- c("function(x, theta) {")
  if (length(intermediates_sub) > 0) {
    # Add intermediate calculations first
    body_lines <- c(body_lines, paste(" ", names(intermediates_sub), "<-", unlist(intermediates_sub)))
  }
  # Add the final vector of derivatives
  body_lines <- c(body_lines, paste0("  c(", paste0(states, " = ", rhs_sub, collapse = ", "), ")"), "}")
  
  body_txt <- paste(body_lines, collapse = "\n")
  deriv_fn <- eval(parse(text = body_txt))
  
  list(
    states      = states,
    param_names = params,
    deriv       = deriv_fn
  )
}

# -------- 0) Minimal helpers: build (X, dX/dt) from a *_train10pct.csv ------
# Modified to use "replicate_id" instead of "ic_id"
build_X_dX <- function(train_csv, spar=NULL, min_pts=4){
  df <- read.csv(train_csv, check.names=FALSE)
  # Rename ic_id to replicate_id if it exists
  if ("ic_id" %in% names(df)) {
    names(df)[names(df) == "ic_id"] <- "replicate_id"
  }
  stopifnot("replicate_id" %in% names(df))
  
  state_cols <- grep("^x[0-9]+$", names(df), value=TRUE)
  per_replicate <- split(df, df$replicate_id)
  rows <- lapply(per_replicate, function(g){
    g <- g[order(g$time), , drop=FALSE]
    if (length(unique(g$time)) < min_pts) return(NULL)
    dmat <- sapply(state_cols, function(s){
      fit <- smooth.spline(g$time, g[[s]], spar=spar)
      as.numeric(predict(fit, g$time, deriv=1)$y)
    })
    cbind(g[, c("replicate_id","time",state_cols)],
          setNames(as.data.frame(dmat), paste0("d.", state_cols)))
  })
  D <- do.call(rbind, rows); rownames(D) <- NULL
  X  <- as.matrix(D[, state_cols, drop=FALSE])
  dX <- as.matrix(D[, paste0("d.", state_cols), drop=FALSE])
  colnames(dX) <- state_cols          # <-- key: make names match states
  list(states=state_cols, X=X, dX=dX, rows=D)
}


# -------- 1) Candidate model interface --------------------------------------
# Define a candidate as a small list with:
#   $states: character vector of state names in order
#   $param_names: character vector of parameter names
#   $deriv: function f(x_named, theta_named) -> numeric vector of derivatives (named like states)

# Example candidate (Bacterial GT-like; edit freely):
candidate_bacterial <- list(
  states=c("x1","x2"),
  param_names=c("a1","a2","a3","b1","b2"),
  deriv=function(x, theta){
    # x and theta come in as *named* lists/vectors: use x[["x1"]], theta[["a1"]], etc.
    x1 <- x[["x1"]]; x2 <- x[["x2"]]
    a1 <- theta[["a1"]]; a2 <- theta[["a2"]]; a3 <- theta[["a3"]]
    b1 <- theta[["b1"]]; b2 <- theta[["b2"]]
    C <- 1 + x1 + x2
    c(x1 = a1 + (a2*x1^2)/(a3 + x1^2) - x1/C,
      x2 = b1/(1 + b2*x1^5) - x2/C)
  }
)

# (Optional) a very simple linear 2-compartment for contrast:
candidate_linear <- list(
  states=c("x1","x2"),
  param_names=c("k1","k12","k2"),
  deriv=function(x, theta){
    x1 <- x[["x1"]]; x2 <- x[["x2"]]
    k1 <- theta[["k1"]]; k12 <- theta[["k12"]]; k2 <- theta[["k2"]]
    c(x1 = -k1*x1, x2 = k12*x1 - k2*x2)
  }
)

matrix_to_named_df <- function(mat, row_name_col = "row_name") {
  df <- as.data.frame(mat)
  df <- cbind(stats::setNames(list(rownames(mat)), row_name_col), df)
  rownames(df) <- NULL
  df
}

res_to_string <- function(obj) {
  # capture.output() returns a character vector, one element per line
  output_lines <- capture.output(print(obj))
  
  # paste() with collapse="\n" joins the lines back into a single string
  paste(output_lines, collapse = "\n")
}

# -------- 2) MODIFIED scoring function -----------------------------------------
score_candidate_fit <- function(train_csv, cand, theta0_named, lower=NULL, upper=NULL,
                                spar=NULL, scale="sd"){
  # --- 1. Data Preparation ---
  dat <- build_X_dX(train_csv, spar=spar)
  stopifnot(identical(sort(cand$states), sort(dat$states)))
  X  <- dat$X[, cand$states, drop=FALSE]
  dX <- dat$dX[, cand$states, drop=FALSE]
  
  S <- switch(scale, "sd"=apply(dX,2,sd), "iqr"=apply(dX,2,IQR), rep(1, ncol(dX)))
  S[S==0 | is.na(S)] <- 1
  p_names <- cand$param_names
  p0 <- as.numeric(theta0_named[p_names]); names(p0) <- p_names
  
  # --- 2. Objective and Prediction Functions ---
  obj <- function(pvec){
    theta <- as.list(setNames(pvec, p_names))
    pred <- t(apply(X, 1, function(row){
      out <- cand$deriv(as.list(setNames(row, cand$states)), theta)
      as.numeric(out[cand$states])
    }))
    sum(((dX - pred) / rep(S, each=nrow(dX)))^2)
  }
  
  get_pred <- function(pvec){
    theta <- as.list(setNames(pvec, p_names))
    pred <- t(apply(X, 1, function(row){
      out <- cand$deriv(as.list(setNames(row, cand$states)), theta)
      as.numeric(out[cand$states])
    }))
    colnames(pred) <- cand$states
    return(pred)
  }
  
  # --- 3. Parameter Optimization ---
  use_bounds <- !is.null(lower) && !is.null(upper)
  fit <- if (use_bounds) {
    optim(p0, obj, method="L-BFGS-B", lower=lower, upper=upper, hessian=TRUE)
  } else {
    optim(p0, obj, method="BFGS", hessian=TRUE)
  }
  
  # --- 4. Detailed Summary Statistics Calculation ---
  dX_pred <- get_pred(fit$par)
  residuals <- dX - dX_pred
  
  summary_df <- cbind(dat$rows[, c("replicate_id", "time")], 
                      as.data.frame(dX), 
                      setNames(as.data.frame(dX_pred), paste0("pred_", cand$states)),
                      setNames(as.data.frame(residuals), paste0("resid_", cand$states)))
  
  per_replicate_list <- split(summary_df, summary_df$replicate_id)
  q_probs <- c(min=0, q25=0.25, median=0.5, q75=0.75, max=1.0)
  
  per_rep_stats <- lapply(per_replicate_list, function(df) {
    if(nrow(df) < 2) return(NULL)
    sapply(cand$states, function(s) {
      obs <- df[[s]]; pred <- df[[paste0("pred_", s)]]; resid <- df[[paste0("resid_", s)]]
      tss <- sum((obs - mean(obs, na.rm=TRUE))^2, na.rm=TRUE)
      rss <- sum(resid^2, na.rm=TRUE)
      r_sq <- if (tss > 0) 1 - (rss / tss) else NA
      c(r_sq = r_sq, pred_data_cor = cor(obs, pred), resid_autocor = cor(resid[-1], resid[-nrow(df)]), resid_time_cor = cor(resid, df$time))
    })
  })
  
  stat_names <- c("r_sq", "pred_data_cor", "resid_autocor", "resid_time_cor")
  quantile_matrices <- lapply(stat_names, function(stat) {
    stat_matrix <- do.call(rbind, lapply(per_rep_stats, `[`, stat,))
    apply(stat_matrix, 2, quantile, probs=q_probs, na.rm=TRUE)
  })
  
  # <<<<<<< MODIFICATION HERE >>>>>>>>>
  # Convert quantile matrices to named data frames
  quantile_summaries <- list(
    quantiles_r_squared = matrix_to_named_df(quantile_matrices[[1]], "quantile"),
    quantiles_pred_vs_data_cor = matrix_to_named_df(quantile_matrices[[2]], "quantile"),
    quantiles_residual_autocor = matrix_to_named_df(quantile_matrices[[3]], "quantile"),
    quantiles_residual_time_cor = matrix_to_named_df(quantile_matrices[[4]], "quantile")
  )
  
  pred_vs_obs_matrix <- sapply(cand$states, function(s) {
    fit <- lm(summary_df[[s]] ~ summary_df[[paste0("pred_", s)]])
    c(intercept = coef(fit)[1], slope = coef(fit)[2], r.squared = summary(fit)$r.squared)
  })
  
  # <<<<<<< MODIFICATION HERE >>>>>>>>>
  # Convert diagnostic matrix to a named data frame
  pred_vs_obs_fit_df <- matrix_to_named_df(pred_vs_obs_matrix, "metric")
  
  # (Model selection and parameter analysis are unchanged)
  n_obs <- length(residuals); n_params <- length(fit$par); unscaled_rss <- sum(residuals^2, na.rm = TRUE)
  aic <- n_obs * log(unscaled_rss / n_obs) + 2 * n_params
  bic <- n_obs * log(unscaled_rss / n_obs) + n_params * log(n_obs)
  model_selection <- c(AIC = aic, BIC = bic, RSS = unscaled_rss)
  param_stats <- list(param_std_errors = NULL, param_correlation = NULL)
  cov_mat_approx <- try(solve(0.5 * fit$hessian), silent = TRUE)
  if (!inherits(cov_mat_approx, "try-error")) {
    param_stats$param_std_errors <- setNames(sqrt(diag(cov_mat_approx)), p_names)
    param_stats$param_correlation <- cov2cor(cov_mat_approx)
  }
  
  # --- 5. Consolidate and Return Results ---
  results <- list(
    theta_initial = theta0_named,
    optim_results = fit,
    model_selection_criteria = model_selection,
    summary_stats = quantile_summaries, # Now contains data frames
    diagnostic_summary = list(
      pred_vs_obs_fit = pred_vs_obs_fit_df, # Now a data frame
      predicted_derivative_correlations = cor(dX_pred)
    ),
    parameter_analysis = param_stats,
    statistic_definitions = list( # (definitions are unchanged)
      AIC_BIC = "Model selection criteria (Akaike/Bayesian Information Criterion) that balance fit (RSS) and complexity (number of parameters). Lower values are better when comparing models.",
      quantiles_r_squared = "Distribution of R-squared across replicates. Measures how much variance in observed derivatives is explained by the model. Ideal median is close to 1.",
      quantiles_pred_vs_data_cor = "Distribution of Pearson correlation between predictions and observations across replicates. Ideal median is close to 1.",
      quantiles_residual_autocor = "Distribution of lag-1 autocorrelation of residuals. Checks for time-dependent errors. Ideal median is close to 0.",
      quantiles_residual_time_cor = "Distribution of correlation between residuals and time. Checks if model error systematically changes over trajectories. Ideal median is close to 0.",
      pred_vs_obs_fit = "Summary of lm(observed ~ predicted). `intercept` shows additive bias (ideal:0), `slope` shows multiplicative bias (ideal:1), `r.squared` shows linearity (ideal:1).",
      predicted_derivative_correlations = "Correlation matrix of the model's own predicted derivatives, showing how state variables are coupled in the model."
    )
  )
  
  return(results)
}


# -------- 3) Example usage ---------------------------------------------------
# Make sure to set your working directory and that the CSV has a 'replicate_id' column
# setwd("~/projects/llm-ode-search/") 
#csv <- "data/ground_truth_raw/GT_Bacterial_train10pct.csv"
# Note: For the code to run, the source CSV needs the "ic_id" column renamed to "replicate_id"
# If not, the build_X_dX function will handle it.

# (A) Fit + score the GT-like candidate:
#gt_guess <- c(a1=0.01, a2=0.05, a3=0.05, b1=0.8, b2=1000)
#system.time(res <- score_candidate_fit(csv, candidate_bacterial, gt_guess,
  #                                     lower=rep(1e-8, 5), upper=c(1,1,1,5,1e6), spar=NULL))

# Print the new, cleaner, and more descriptive output
#print(res)
# (B) Compare a linear 2-compartment (should score worse):
#lin_guess <- c(k1=0.6, k12=1.2, k2=0.3)
#system.time(res_lin <- score_candidate_fit(csv, candidate_linear, lin_guess,
 #                                          lower=rep(1e-8, 3), upper=rep(10, 3), spar=NULL))
#res_lin
