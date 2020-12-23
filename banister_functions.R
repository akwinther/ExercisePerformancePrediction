banister_function_sse <- function (v, Training.Load, Performance) { #The function takes three inputs...
  p0 <- v[1]; k1 <- v[2]; k2 <- v[3]; tau1 <- v[4]; tau2 <- v[5]
  Fitness <- 0
  Fatigue <- 0
  for (i in 1:length(Training.Load)) {
    Fitness[i+1] <- Fitness[i] * exp(-1/tau1) + Training.Load[i+1] #Recursion equations
    Fatigue[i+1] <- Fatigue[i] * exp(-1/tau2) + Training.Load[i+1] 
    Predicted.Performance <- p0 + k1*Fitness - k2*Fatigue
  }
  errors <- Performance[!is.na(Performance)] - Predicted.Performance[which(!is.na(Performance))]  
  SSE <- sum(errors^2)
  return(SSE) #...and returns the sum of squared errors
}

banister_function_mae <- function (v, Training.Load, Performance) { 
  p0 <- v[1]; k1 <- v[2]; k2 <- v[3]; tau1 <- v[4]; tau2 <- v[5]
  Fitness <- 0
  Fatigue <- 0
  for (i in 1:length(Training.Load)) {
    Fitness[i+1] <- Fitness[i] * exp(-1/tau1) + Training.Load[i+1] 
    Fatigue[i+1] <- Fatigue[i] * exp(-1/tau2) + Training.Load[i+1] 
    Predicted.Performance <- p0 + k1*Fitness - k2*Fatigue
  }
  abs_errors <- abs(Performance[!is.na(Performance)] - Predicted.Performance[which(!is.na(Performance))]) 
  MAE <- mean(abs_errors)
  
  return(MAE) #...and returns the mean absolute error
}

