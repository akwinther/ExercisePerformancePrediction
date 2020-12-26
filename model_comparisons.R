# Run *optim* over each athlete in a list
optim_results_banister <- vector("list", 6)
for (i in 1:6) {
  optim_results_banister[[i]] <- optim(par = c(255, 0.10, 0.10, 15, 11), fn = banister_function_mae, tl = subjects_list[[i]][,2], performance = subjects_list[[i]][,3])
}  
names(optim_results_banister) <- paste("Subject", 1:6, sep = "")

get_performance <- function(theta, tl, performance) {
  int  <- theta[1] # performance baseline
  k1   <- theta[2] # fitness weight
  k2   <- theta[3] # fatigue weight
  tau1 <- theta[4] # fitness decay
  tau2 <- theta[5] # fatigue decay
  
  fitness <- sapply(1:length(tl),
                    function(n) convolve_training(tl, n, tau1))
  
  fatigue <- sapply(1:length(tl),
                    function(n) convolve_training(tl, n, tau2))
  
  int + k1 * fitness - k2 * fatigue
}


for (i in 1:6) {
  subjects_list[[i]]$perf_hat <- get_performance(optim_results[[i]]$par, banister_parameters[[i]]$tl, banister_parameters[[i]]$performance)
  
}

r2 <- sapply(subjects_list, function(x) cor(x$performance, x$perf_hat, use = "complete.obs")^2)  

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


# Try busso mod
# Run *optim* over each athlete in a list
optim_results_busso <- vector("list", 6)
for (i in 1:6) {
  optim_results_busso[[i]] <- optim(par = c(255, 0.01, 0.00001, 30, 15, 1), fn = busso_function_mae, tl = subjects_list[[i]][,2], performance = subjects_list[[i]][,3])
                              
}  

names(optim_results_busso) <- paste("Subject", 1:6, sep = "")

lapply(optim_results_busso, function(x) x$par)
lapply(optim_results_banister, function(x) x$par)

lapply(optim_results_busso, function(x) x$value)
lapply(optim_results_banister, function(x) x$value)

get_performance_busso <- function(theta, tl, performance) {
  int  <- theta[1] # performance baseline
  k1   <- theta[2] # fitness weight
  k2   <- theta[3] # fatigue weight
  tau1 <- theta[4] # fitness decay
  tau2 <- theta[5] # fatigue decay
  
  fitness <- sapply(1:length(tl),
                    function(n) convolve_training(tl, n, tau1))
  
  fatigue <- sapply(1:length(tl),
                    function(n) convolve_training(tl, n, tau2))
  
  int + k1 * fitness - k2 * fatigue
}
