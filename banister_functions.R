# Exponential decay and fitness-fatigue profiles
exp_decay <- function(t, tau) {
  exp(-t / tau)
}

convolve_training <- function(training, n, tau) {
  sum(training[1:(n - 1)] * exp_decay((n - 1):1, tau))
}

k_2 <- function(training, n, k_3, tau) {
  k_3*sum(training[1:(n)] * exp_decay(n:1, tau))
}

convolve_fatigue <- function(training, n, tau, k_2) {
  sum(k_2[1:(n - 1)] * training[1:(n - 1)] * exp_decay((n - 1):1, tau))
}


banister_function_mae <- function(theta, tl, performance) {
  int  <- theta[1] # performance baseline
  k1   <- theta[2] # fitness weight
  k2   <- theta[3] # fatigue weight
  tau1 <- theta[4] # fitness decay
  tau2 <- theta[5] # fatigue decay
  
  
  fitness <- sapply(1:length(tl),
                    function(n) convolve_training(tl, n, tau1))
  
  fatigue <- sapply(1:length(tl),
                    function(n) convolve_training(tl, n, tau2))
  
  perf_hat <- int + k1 * fitness - k2 * fatigue
  
  abs_errors <- abs(performance[!is.na(performance)] - perf_hat[which(!is.na(performance))]) 
  
  MAE <- mean(abs_errors)
  
  return(MAE)
}

# Busso function
busso_function_mae <- function(theta, tl, performance) {
  int  <- theta[1] # performance baseline
  k1   <- theta[2] # multiplying factor for the positive component of training
  k3   <- theta[3] # multiplying factor for the fatigue factor
  tau1 <- theta[4] # fitness decay
  tau2 <- theta[5] # fatigue decay
  tau3 <- theta[6]
  
  fitness <- sapply(1:length(tl),
                    function(n) convolve_training(tl, n, tau1))
  
  k2 <-  sapply(1:length(tl),
                function(n) k_2(tl, n, k3, tau3))
  
  fatigue <- sapply(1:length(tl),
                    function(n) convolve_fatigue(tl, n, tau2, k2))
  
  perf_hat <- int + k1 * fitness - fatigue
  
  abs_errors <- abs(performance[!is.na(performance)] - perf_hat[which(!is.na(performance))]) 
  
  MAE <- mean(abs_errors)
  
  return(MAE)
}

