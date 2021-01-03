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


banister_function_mae <- function(theta, tl, performance, fit = FALSE) {
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
  
  if (fit == TRUE) {
    return(MAE)
  }
  else {
    return(perf_hat)
  }
}




busso_function_mae <- function(theta, tl, performance, fit = FALSE) {
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
  
  if (fit == TRUE) {
    return(MAE)
  }
  else {
    return(perf_hat)
  }
}



matabuena_function_mae <- function(theta, tl, performance, delay, fit = FALSE) {
  
  if (delay == 1) {
    int  <- theta[1] # performance baseline
    k1   <- theta[2] # fitness weight
    k2   <- theta[3] # fatigue weight
    tau1 <- theta[4] # fitness decay
    tau2 <- theta[5] # fatigue decay
    tau3 <- theta[6] # fitness decay
    tau4 <- theta[7] # fatigue decay
    
    fitness <- 0
    fatigue <- 0
    
    for (i in 2:length(tl)) {
      
      if (i == 2) {
        fitness[i] <- exp(-1/tau1)*(tl[i-1] + fitness[i-1]) 
        fatigue[i] <- exp(-1/tau2)*(tl[i-1] + fatigue[i-1])
        #perf_hat <- int + k1 * fitness - k2 * fatigue
      }
      
      else { 
        fitness[i] <- (tl[i-1] + fitness[i-1] - (1/tau3)*fitness[i-2])*exp(-1/tau1)
        fatigue[i] <- (tl[i-1] + fatigue[i-1] - (1/tau4)*fatigue[i-2])*exp(-1/tau2)
        #perf_hat <- int + k1 * fitness - k2 * fatigue
      }
      
      perf_hat <- int + k1 * fitness - k2 * fatigue
      
    }
    
    abs_errors <- abs(performance[!is.na(performance)] - perf_hat[which(!is.na(performance))]) 
    
    MAE <- mean(abs_errors)
    
    if (fit == TRUE) {
      return(MAE)
    }
    else {
      return(perf_hat)
    }
    
  }
  
  if (delay == 2) {
    int  <- theta[1] # performance baseline
    k1   <- theta[2] # fitness weight
    k2   <- theta[3] # fatigue weight
    tau1 <- theta[4] # fitness decay
    tau2 <- theta[5] # fatigue decay
    tau3 <- theta[6] # fitness decay
    tau4 <- theta[7] # fatigue decay
    tau5 <- theta[8] # fitness decay
    tau6 <- theta[9] # fatigue decay
    
    fitness <- 0
    fatigue <- 0
    
    for (i in 2:length(tl)) {
      
      if (i == 2) {
        fitness[i] <- exp(-1/tau1)*(tl[i-1] + fitness[i-1]) 
        fatigue[i] <- exp(-1/tau2)*(tl[i-1] + fatigue[i-1])
      }
      
      else if (i == 3) {
        fitness[i] <- (tl[i-1] + fitness[i-1] - (1/tau3)*fitness[i-2])*exp(-1/tau1)
        fatigue[i] <- (tl[i-1] + fatigue[i-1] - (1/tau4)*fatigue[i-2])*exp(-1/tau2)
      }
      
      else { 
        fitness[i] <- (tl[i-1] + fitness[i-1] - (1/tau3)*fitness[i-2] - (1/tau5)*fitness[i-3])*exp(-1/tau1)
        fatigue[i] <- (tl[i-1] + fatigue[i-1] - (1/tau4)*fatigue[i-2] - (1/tau6)*fatigue[i-3])*exp(-1/tau2)
      }
      
      perf_hat <- int + k1 * fitness - k2 * fatigue
      
    }
    
    abs_errors <- abs(performance[!is.na(performance)] - perf_hat[which(!is.na(performance))]) 
    
    MAE <- mean(abs_errors)
    
    if (fit == TRUE) {
      return(MAE)
    }
    else {
      return(perf_hat)
    }
    
  }
  
  if (delay == 3) {
    int  <- theta[1] # performance baseline
    k1   <- theta[2] # fitness weight
    k2   <- theta[3] # fatigue weight
    tau1 <- theta[4] # fitness decay
    tau2 <- theta[5] # fatigue decay
    tau3 <- theta[6] # fitness decay
    tau4 <- theta[7] # fatigue decay
    tau5 <- theta[8] # fitness decay
    tau6 <- theta[9] # fatigue decay
    tau7 <- theta[10] # fitness decay
    tau8 <- theta[11] # fatigue decay
    
    fitness <- 0
    fatigue <- 0
    
    for (i in 2:length(tl)) {
      
      if (i == 2) {
        fitness[i] <- exp(-1/tau1)*(tl[i-1] + fitness[i-1]) 
        fatigue[i] <- exp(-1/tau2)*(tl[i-1] + fatigue[i-1])
      }
      
      else if (i == 3) {
        fitness[i] <- (tl[i-1] + fitness[i-1] - (1/tau3)*fitness[i-2])*exp(-1/tau1)
        fatigue[i] <- (tl[i-1] + fatigue[i-1] - (1/tau4)*fatigue[i-2])*exp(-1/tau2)
      }
      
      else if (i == 4) { 
        fitness[i] <- (tl[i-1] + fitness[i-1] - (1/tau3)*fitness[i-2] - (1/tau5)*fitness[i-3])*exp(-1/tau1)
        fatigue[i] <- (tl[i-1] + fatigue[i-1] - (1/tau4)*fatigue[i-2] - (1/tau6)*fatigue[i-3])*exp(-1/tau2)
      }
      
      else {
        fitness[i] <- (tl[i-1] + fitness[i-1] - (1/tau3)*fitness[i-2] - (1/tau5)*fitness[i-3] - (1/tau7)*fitness[i-4])*exp(-1/tau1)
        fatigue[i] <- (tl[i-1] + fatigue[i-1] - (1/tau4)*fatigue[i-2] - (1/tau6)*fatigue[i-3] - (1/tau8)*fatigue[i-4])*exp(-1/tau2)
      }
      
      perf_hat <- int + k1 * fitness - k2 * fatigue
      
    }
    
    abs_errors <- abs(performance[!is.na(performance)] - perf_hat[which(!is.na(performance))]) 
    
    MAE <- mean(abs_errors)
    
    if (fit == TRUE) {
      return(MAE)
    }
    else {
      return(perf_hat)
    }
    
  }
  
}


modelT_mae <- function(theta, tl, performance, fit = FALSE) {
  k_off <- theta[1]
  k_on <- theta[2]
  k_out <- theta[3]
  
  int <- performance[!is.na(performance)][1]
  signal <- 0
  prod_perf <-int * (1 - exp(-k_off))
  prod_removal <- int * (1 - exp(-k_off))
  perf_hat <- int
  
  for (i in 2:length(tl)) {
   
    signal[i] <- tl[i] + signal[i-1]*exp(-k_on - k_out) 
    prod_perf[i] <- prod_perf[1] + signal[i] * k_on
    perf_hat[i] <- prod_perf[i-1] + perf_hat[i-1] * exp(-k_off)
    
  }
  
  abs_errors <- abs(performance[!is.na(performance)] - perf_hat[which(!is.na(performance))]) 
  
  MAE <- mean(abs_errors)
  
  if (fit == TRUE) {
    return(MAE)
  }
  else {
    return(perf_hat)
  }
  
}



peis <- c(0.0845, 0.0030, 0.0815)

get_performance_modelT(peis, s1$tl, s1$performance)
