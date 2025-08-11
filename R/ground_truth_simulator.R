# deps: rxode2, jsonlite, dplyr, tidyr, readr
suppressPackageStartupMessages({
  library(rxode2); library(jsonlite); library(dplyr); library(tidyr); library(readr)
})

`%||%` <- function(a,b) if (is.null(a)) b else a

# Build an event *data.frame* with time + covariate columns
inputs_to_event_df <- function(inputs, sampling_times){
  brk <- sort(unique(c(sampling_times, input_change_times(inputs))))
  if (length(inputs)==0) return(data.frame(time=brk))
  covdf <- data.frame(time=brk)
  for (inp in inputs){
    v <- numeric(length(brk))
    if (inp$type=="piecewise"){
      for (seg in inp$schedule){ # [t0,t1,val], left-closed/right-open
        t0 <- seg[[1]]; t1 <- seg[[2]]; val <- seg[[3]]
        v[brk>=t0 & brk<t1] <- val
        if (t1==max(brk)) v[brk==t1] <- val
      }
    } else if (inp$type=="constant"){
      v[] <- inp$value
    } else stop("Unknown input type: ", inp$type)
    covdf[[inp$name]] <- v
  }
  covdf
}



# ---- noise ----
apply_noise <- function(df, noise_spec){
  if (is.null(noise_spec) || length(noise_spec)==0) return(df)
  add_noise <- function(x, ns){
    if (ns$type=="gaussian")  return(x + rnorm(length(x), 0, ns$sigma))
    if (ns$type=="lognormal") return(x * exp(rnorm(length(x), 0, ns$sigma)))
    if (ns$type=="negbin")    return(rnbinom(length(x), size=ns$k, mu=pmax(x,1e-8)))
    stop("Unknown noise type: ", ns$type)
  }
  for (ns in noise_spec){
    tgt <- ns$target
    if (tgt %in% names(df)) df[[tgt]] <- add_noise(df[[tgt]], ns)
  }
  df
}

# ---- validator (fast fails, friendly msgs) ----
validate_spec <- function(s){
  stopifnot(!is.null(s$model$states), !is.null(s$model$params))
  if (is.null(s$model$ode_text) || !nzchar(s$model$ode_text))
    stop("Spec must include model$ode_text (rxode2 syntax).")
  if (is.null(s$observe) || length(s$observe)==0)
    s$observe <- setNames(as.list(s$model$states), s$model$states)
  s$conditions <- s$conditions %||% list(list(name="baseline", init=list(), params=list()))
  s$inputs <- s$inputs %||% list()
  s$noise <- s$noise %||% list()
  s$replicates <- s$replicates %||% 1
  s
}

input_change_times <- function(inputs){
  if (length(inputs)==0) return(numeric())
  unlist(lapply(inputs, function(inp){
    if (inp$type=="piecewise") unique(unlist(lapply(inp$schedule, function(seg) c(seg[[1]], seg[[2]]))))
    else numeric()
  }))
}


# ---- main ----
simulate_from_spec <- function(spec, seed=1){
  spec <- validate_spec(spec)
  t0 <- spec$sampling$t0; t1 <- spec$sampling$t1; dt <- spec$sampling$dt
  times <- seq(t0, t1, by=dt)
  
  mdl <- rxode2::rxode(spec$model$ode_text)
  
  out <- list()
  for (ci in seq_along(spec$conditions)){
    cond <- spec$conditions[[ci]]
    init <- modifyList(as.list(spec$init), cond$init %||% list())
    parms <- modifyList(as.list(spec$param_values), cond$params %||% list())
    
    # ensure correct types
    y0 <- unlist(init)[unlist(spec$model$states)]
    if (any(is.na(y0))) stop("Missing init for: ", paste(names(y0)[is.na(y0)], collapse=", "))
    y0 <- setNames(as.numeric(y0), names(y0))
    
    parms <- unlist(parms)                 # <-- key fix: list -> named numeric vector
    parms <- setNames(as.numeric(parms), names(parms))
    
    # ✔ build event *data.frame* with covariates
    ev <- inputs_to_event_df(spec$inputs, times)
    
    for (ri in seq_len(spec$replicates)){
      set.seed(seed + 1000*ci + ri)
      # ✔ pass covariates via `events=ev` (no `covs=` arg)
      sol <- rxode2::rxSolve(mdl, params=parms, inits=y0, events=ev, atol=1e-8, rtol=1e-8) |>
        as.data.frame()
      sol <- sol[sol$time %in% times, , drop=FALSE]
      obs_map <- spec$observe
      obs_df <- sol[, c("time", unname(unlist(obs_map))), drop=FALSE]
      names(obs_df) <- c("time", names(obs_map))
      
      noisy <- apply_noise(obs_df, spec$noise)
      
      long <- noisy |>
        tidyr::pivot_longer(-time, names_to="variable", values_to="value") |>
        mutate(condition = cond$name %||% paste0("cond",ci),
               replicate = ri,
               model = spec$model$name)
      out[[length(out)+1]] <- long
    }
  }
  dplyr::bind_rows(out)
}


simulate_and_write <- function(json_path, seed=1, out_csv=NULL){
  spec <- jsonlite::fromJSON(json_path, simplifyVector=FALSE)
  df <- simulate_from_spec(spec, seed=seed)
  if (is.null(out_csv)) out_csv <- sub("\\.json$", "_gt.csv", basename(json_path))
  readr::write_csv(df, out_csv)
  message("Wrote: ", out_csv, "  (n=", nrow(df), ")")
  invisible(df)
}

summarize_for_llm <- function(df, spec_inputs=NULL, max_points=64){
  library(dplyr); library(tidyr); library(jsonlite)
  
  # grid meta
  grid_meta <- df %>% group_by(condition) %>%
    summarize(t0=min(time), t1=max(time),
              n_time=n_distinct(time),
              dt_median=median(diff(sort(unique(time)))), .groups="drop")
  
  # replicate-mean per time
  mean_traj <- df %>%
    group_by(condition, variable, time) %>%
    summarize(mean=mean(value), sd=sd(value), .groups="drop_last") %>%
    arrange(time)
  
  # downsample preview
  ds <- mean_traj %>%
    group_by(condition, variable) %>%
    mutate(rn=row_number()) %>%
    mutate(sel = rn %in% unique(floor(quantile(rn, probs = seq(0,1,length.out=min(max_points,n())))))) %>%
    filter(sel) %>% select(-rn, -sel)
  
  # per-variable shape features
  feats_var <- function(tt, yy){
    dy <- diff(yy)/diff(tt)
    auc <- sum(diff(tt) * (head(yy,-1)+tail(yy,-1))/2)
    i_pk <- which.max(yy); t_pk <- tt[i_pk]; y_pk <- yy[i_pk]
    slope_init <- if(length(tt)>=3) coef(lm(yy[1:3] ~ tt[1:3]))[2] else NA_real_
    slope_final<- if(length(tt)>=3) coef(lm(tail(yy,3) ~ tail(tt,3)))[2] else NA_real_
    mono <- as.integer(all(dy>=0) | all(dy<=0))
    sign_switches <- sum(diff(sign(dy))!=0, na.rm=TRUE)
    list(n=length(tt), mean=mean(yy), sd=sd(yy), min=min(yy), max=max(yy),
         auc=auc, peak_time=t_pk, peak_value=y_pk,
         slope_initial=slope_init, slope_final=slope_final,
         monotone=mono, derivative_sign_changes=sign_switches)
  }
  
  var_summ <- mean_traj %>%
    group_by(condition, variable) %>%
    summarize(stats=list(feats_var(time, mean)), .groups="drop")
  
  pairwise <- function(df1){
    vars <- unique(df1$variable); if(length(vars)<2) return(list())
    out <- list()
    for(i in 1:(length(vars)-1)) for(j in (i+1):length(vars)){
      a <- df1 %>% filter(variable==vars[i]) %>% arrange(time)
      b <- df1 %>% filter(variable==vars[j]) %>% arrange(time)
      tt <- intersect(a$time,b$time); if(length(tt)<5) next
      ya <- a$mean[a$time %in% tt]; yb <- b$mean[b$time %in% tt]
      maxLag <- min(20, length(tt)%/%4)
      cc <- sapply(-maxLag:maxLag, function(L){
        if(L<0){cor(ya[1:(length(ya)+L)], yb[(1-L):length(yb)])}
        else if(L>0){cor(ya[(1+L):length(ya)], yb[1:(length(yb)-L)])}
        else cor(ya, yb)
      })
      k <- which.max(abs(cc)); lag_idx <- (-maxLag:maxLag)[k]
      out[[length(out)+1]] <- list(var_a=vars[i], var_b=vars[j],
                                   max_abs_corr=unname(cc[k]),
                                   lag_points=lag_idx)
    }
    out
  }
  
  objs <- lapply(split(mean_traj, mean_traj$condition), function(sub){
    cond <- unique(sub$condition)
    meta <- grid_meta %>% filter(condition==cond)
    
    # variable_summaries: named list {var -> stats}
    sub_vs <- var_summ %>% filter(condition==cond) %>% select(variable, stats)
    vstats <- setNames(lapply(sub_vs$stats, `[[`, 1), sub_vs$variable)
    
    # series_preview: named list {var -> list(time, mean, sd)}
    sub_ds <- ds %>% filter(condition==cond) %>% arrange(variable, time)
    series_tbl <- sub_ds %>%
      group_by(variable) %>%
      summarize(time=list(time), mean=list(mean), sd=list(sd), .groups="drop")
    series <- setNames(lapply(seq_len(nrow(series_tbl)), function(i)
      list(time=series_tbl$time[[i]], mean=series_tbl$mean[[i]], sd=series_tbl$sd[[i]])
    ), series_tbl$variable)
    
    list(
      condition = cond,
      time = list(t0=meta$t0, t1=meta$t1, n_points=meta$n_time, dt_median=meta$dt_median),
      variables = sort(unique(sub$variable)),
      variable_summaries = vstats,
      pairwise = pairwise(sub),
      series_preview = series,
      inputs = if (!is.null(spec_inputs)) spec_inputs else NULL
    )
  })
  
  jsonlite::toJSON(objs, auto_unbox=TRUE, null="null", pretty=TRUE)
}



Rseed <- 41
setwd("~/projects/llm-ode-search/")
spec <- jsonlite::fromJSON("ground_truth_specs/droop.json", simplifyVector=FALSE)
df <- simulate_from_spec(spec, seed=Rseed)
spec$sampling$dt <- 8
spec$observe<-spec$observe[c("X","S")]
df_sam <- simulate_from_spec(spec, seed=Rseed)
packet_json <- summarize_for_llm(df_sam, spec_inputs = spec$inputs)
writeLines(packet_json, "LLM_data_packets/droop.json")
p <- ggplot(df,aes(x=time,y=value))+
  facet_grid(cols=vars(condition),rows = vars(variable),scales="free")+
  geom_line()+
  geom_point(data=df_sam,color="red")
p

spec <- jsonlite::fromJSON("ground_truth_specs/edelstein.json", simplifyVector=FALSE)
df <- simulate_from_spec(spec, seed=Rseed)
spec$sampling$dt <- 4
spec$observe<-spec$observe[c("Rstar")]
df_sam <- simulate_from_spec(spec, seed=Rseed)
packet_json <- summarize_for_llm(df_sam, spec_inputs = spec$inputs)
writeLines(packet_json, "LLM_data_packets/edelstein.json")
p <- ggplot(df,aes(x=time,y=value))+
  facet_grid(cols=vars(condition),rows = vars(variable),scales="free")+
  geom_line()+
  geom_point(data=df_sam,color="red")
p

spec <- jsonlite::fromJSON("ground_truth_specs/beddington.json", simplifyVector=FALSE)
df <- simulate_from_spec(spec, seed=Rseed)
spec$sampling$dt <- 8
df_sam <- simulate_from_spec(spec, seed=Rseed)
packet_json <- summarize_for_llm(df_sam, spec_inputs = spec$inputs)
writeLines(packet_json, "LLM_data_packets/beddington.json")
p <- ggplot(df,aes(x=time,y=value))+
  facet_grid(cols=vars(condition),rows = vars(variable),scales="free")+
  geom_line()+
  geom_point(data=df_sam,color="red")
p

