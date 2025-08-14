# deps: deSolve
library(deSolve)

simulate_case <- function(name, derivs, pars, nstate, NIC, lb, ub, t0, tf, dt, ICSpp, seed=7713047) {
  set.seed(seed)
  times <- seq(t0, tf, by=dt)
  X0 <- matrix(runif(NIC * nstate, min=lb, max=ub), ncol=nstate)
  colnames(X0) <- paste0("x", 1:nstate)
  
  sim_one <- function(x0, id) {
    out <- ode(y=x0, times=times, func=derivs, parms=pars, method="lsoda")
    cbind(model=name, ic_id=id, as.data.frame(out))
  }
  all <- do.call(rbind, lapply(seq_len(NIC), function(i) sim_one(X0[i,], i)))
  
  set.seed(seed)                       # for reproducible 10% draw
  idx <- sample.int(nrow(all), size=ceiling(0.10*nrow(all)))
  train <- all[idx, , drop=FALSE][order(all$time[idx]), ]
  
  pred <- cbind(model=name, ic_id="ICSpp",
                as.data.frame(ode(y=setNames(ICSpp, paste0("x",1:nstate)),
                                  times=times, func=derivs, parms=pars, method="lsoda")))
  list(all=all, train10=train, pred=pred)
}

# --------------- ODE right-hand-sides ----------------
lorenz_derivs <- function(t, x, p) with(as.list(c(x,p)), {
  list(c(a*(x2-x1), x1*(b-x3)-x2, x1*x2 - c*x3))
})

immunity_derivs <- function(t, x, p) with(as.list(c(x,p)), {
  dx1 <- a*(1 - x1/k)*x1 - e*x1*x2 - (beta*gamma*x1*x2^2)/(gamma*x2 + alpha*x1)
  dx2 <- S + d*x1 - delta*x2
  list(c(dx1, dx2))
})

bacterial_derivs <- function(t, x, p) with(as.list(c(x,p)), {
  dx1 <- a1 + (a2*x1^2)/(a3 + x1^2) - x1/(1 + x1 + x2)
  dx2 <- b1/(1 + b2*x1^5) - x2/(1 + x1 + x2)
  list(c(dx1, dx2))
})

microbial_derivs <- function(t, x, p) with(as.list(c(x,p)), {
  dx1 <- (mu*x2*x1)/(Ks + x2) - Kd*x1
  dx2 <- -(mu*x2*x1)/(gamma*(Ks + x2))
  list(c(dx1, dx2))
})

crypt_derivs <- function(t, x, p) with(as.list(c(x,p)), {
  dx1 <- (a3 - a1 - a2)*x1 - (k0*x1^2)/(1 + m0*x1)
  dx2 <- (b3 - b1 - b2)*x2 + a2*x1 - (k1*x2^2)/(1 + m1*x2) + (k0*x1^2)/(1 + m0*x1)
  dx3 <- -g*x3 + b2*x2 + (k1*x2^2)/(1 + m1*x2)
  list(c(dx1, dx2, dx3))
})

glycolysis_derivs <- function(t, x, p) with(as.list(c(x,p)), {
  dx1 <- c1 + (c2*x1*x6)/(1 + c3*x6^4)
  dx2 <- (d1*x1*x6)/(1 + d2*x6^4) + d3*x2 - d4*x2*x7
  dx3 <- e1*x2 + e2*x3 + e3*x2*x7 + e4*x3*x6
  dx4 <- f1*x3 + e2*x4 + f3*x5 + f4*x3*x6 + f5*x4*x7
  dx5 <- g1*x4 + g2*x5
  dx6 <- h3*x3 + h5*x6 + h4*x3*x6 + (h1*x1*x6)/(1 + h2*x6^4)
  dx7 <- j1*x2 + j2*x2*x7 + j3*x4*x7
  list(c(dx1, dx2, dx3, dx4, dx5, dx6, dx7))
})

# --------------- Case definitions ----------------
cases <- list(
  list(name="Lorenz", derivs=lorenz_derivs,
       pars=c(a=10, b=28, c=8/3), nstate=3, NIC=80,
       lb=1e-4, ub=0.4706, t0=0, tf=15, dt=0.1, ICSpp=c(0.35,0.2,0.4)),
  list(name="Immunity", derivs=immunity_derivs,
       pars=c(a=0.18, k=60, e=0.005, beta=0.013, gamma=0.02, alpha=0.01, S=1.1, d=0.001, delta=0.105),
       nstate=2, NIC=80, lb=0.05, ub=75.9614, t0=0, tf=15, dt=0.1, ICSpp=c(0.2635,0.8278)),
  list(name="Bacterial", derivs=bacterial_derivs,
       pars=c(a1=0.004, a2=0.07, a3=0.04, b1=0.82, b2=1854.5),
       nstate=2, NIC=80, lb=1e-4, ub=4.9, t0=0, tf=15, dt=0.1, ICSpp=c(0.8,0.9)),
  list(name="Microbial", derivs=microbial_derivs,
       pars=c(Kd=0.05, Ks=1, mu=0.4, gamma=0.5),
       nstate=2, NIC=80, lb=1e-4, ub=0.4706, t0=0, tf=15, dt=0.1, ICSpp=c(0.8,0.9)),
  list(name="Crypt", derivs=crypt_derivs,
       pars=c(a1=0.1, a2=0.3, a3=0.69, b1=0.1, b2=0.3, b3=0.397, g=0.139, k0=0.1, k1=3e-4, m0=0.1, m1=4e-4),
       nstate=3, NIC=80, lb=1e-4, ub=79.9582, t0=0, tf=15, dt=0.1, ICSpp=c(0.35,0.2,0.1)),
  list(name="Glycolysis", derivs=glycolysis_derivs,
       pars=c(c1=2.5, c2=-100, c3=13.6769, d1=200, d2=13.6769, d3=-6, d4=-6,
              e1=6, e2=-64, e3=6, e4=16, f1=64, f2=-13, f3=13, f4=-16, f5=-100,
              g1=1.3, g2=3.1, h1=-200, h2=13.6769, h3=128, h4=-32, h5=-32/25,
              j1=6, j2=-18, j3=-100),
       nstate=7, NIC=450, lb=1e-7, ub=0.25, t0=0, tf=15, dt=0.1, ICSpp=c(0.5,0.2,1.1,0.8,0.5,0.4,0.1))
)

generate_all_groundtruth <- function(seed=7713047, write_csv=FALSE, out_dir=".") {
  out <- lapply(cases, function(cs) {
    r <- simulate_case(cs$name, cs$derivs, cs$pars, cs$nstate, cs$NIC, cs$lb, cs$ub,
                       cs$t0, cs$tf, cs$dt, cs$ICSpp, seed=seed)
    if (write_csv) {
      write.csv(r$all,     file=file.path(out_dir, paste0("GT_", cs$name, "_all.csv")), row.names=FALSE)
      write.csv(r$train10, file=file.path(out_dir, paste0("GT_", cs$name, "_train10pct.csv")), row.names=FALSE)
      write.csv(r$pred,    file=file.path(out_dir, paste0("GT_", cs$name, "_ICSpp.csv")), row.names=FALSE)
    }
    r
  })
  names(out) <- vapply(cases, `[[`, "", "name")
  out
}

# Example:
 gt <- generate_all_groundtruth(seed=20230913, write_csv=TRUE, out_dir=".")
