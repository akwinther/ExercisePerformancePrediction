# Run *optim* over each athlete in a list
banister_optimised <- vector("list", 6)
for (i in 1:6) {
  banister_optimised[[i]] <- optim(par = c(256, 0.10, 0.10, 15, 11), fn = banister_function_sse, Training.Load = subjects_list[[i]][,2], Performance = subjects_list[[i]][,3])
}  
names(banister_optimised) <- paste("Subject", 1:6, sep = "")
str(banister_optimised[1])

# Put banister parameters and SSE in a dataframe for all athletes 
banister_parameters <- vector("list", 6)
for (i in 1:6) {
  banister_parameters[[i]] <- list("Day" = subjects_list[[i]][,1], "Training.Load" = subjects_list[[i]][,2], "Performance" = subjects_list[[i]][,3], "Parameters" = banister_optimised[[i]]$par, "SSE" = banister_optimised[[i]]$value)
}
names(banister_parameters) <- paste("Subject", 1:6, sep = "")
str(banister_parameters[1])


banister_models <- lapply(banister_parameters, function(x) {
  p0 <- x$Parameters[1]; k1 <- x$Parameters[2]; k2 <- x$Parameters[3]; tau1 <- x$Parameters[4]; tau2 <- x$Parameters[5]
  Fitness <- 0
  Fatigue <- 0
  for (i in 1:length(x$Training.Load)) {
    Fitness[i+1] <- Fitness[i] * exp(-1/tau1) + x$Training.Load[i+1]
    Fatigue[i+1] <- Fatigue[i] * exp(-1/tau2) + x$Training.Load[i+1]
    Predicted_Performance <- p0 + k1*Fitness - k2*Fatigue
  }
  Errors <- x$Performance[!is.na(x$Performance)] - Predicted_Performance[which(!is.na(x$Performance))]
  SSE <- sum(Errors^2)
  R2 <- (cor(x$Performance[!is.na(x$Performance)], Predicted_Performance[which(!is.na(x$Performance))]))^2
  return(list("Day" = x$Day, "Training.Load" = x$Training.Load, "Performance" = x$Performance, "Predicted_Performance" = Predicted_Performance[!is.na(Predicted_Performance)], "SSE" = SSE ,"R2" = R2))
})

lapply(banister_models, function(x) {
  return(x$R2)
})

# Visualize results

banister_models <- lapply(banister_models, function(x) {
  x$Week <- x$Day/7
  return(x) #Creates a variable called 'Week'
})

banister_df <- plyr::ldply(banister_models, data.frame)
colnames(banister_df)[colnames(banister_df)==".id"] <- "Subject"
banister_plots <- ggplot(banister_df, aes(x = Week, y = Predicted_Performance)) +
  geom_line(aes(y = Predicted_Performance, colour = "black"), size = 1) +
  geom_point(aes(y = Performance, colour = "red"), shape = 1) +
  scale_color_manual("", values = c("black", "red"), labels = c("Predicted Performance", "Actual Performance")) +
  guides(colour = guide_legend(override.aes = list(linetype = c("solid", "blank"), shape = c(NA, 1)))) +
  ylab("Performance") +
  xlab("Week") +
  scale_x_continuous(breaks = seq(0,16,2)) +
  geom_label(data = banister_df, aes(x = 2, y = 400, label = paste("italic(R) ^ 2 == ", round(R2, 2))), parse = TRUE, size = 3) +
  theme_minimal() +
  facet_wrap(~ Subject, ncol = 2) 

banister_plots
