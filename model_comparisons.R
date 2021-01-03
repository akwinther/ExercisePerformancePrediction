# Run *optim* over each athlete in a list
optim_results_banister <- lapply(subjects_list, function(x) optim(par = c(255, 0.10, 0.10, 15, 11), fn = banister_function_mae, tl = x[,2], performance = x[,3]))
optim_results_busso <- lapply(subjects_list, function(x) optim(par = c(255, 0.01, 0.00001, 30, 15, 1), fn = busso_function_mae, tl = x[,2], performance = x[,3]))
optim_results_matabuena_delay1 <- lapply(subjects_list, function(x) optim(par = c(255, 0.10, 0.10, 10, 10, 5, 5), fn = matabuena_function_mae, tl = x[,2], performance = x[,3], delay = 1, fit = TRUE))
optim_results_matabuena_delay2 <- lapply(subjects_list, function(x) optim(par = c(255, 0.10, 0.10, 10, 10, 5, 5, 5, 5), fn = matabuena_function_mae, tl = x[,2], performance = x[,3], delay = 2, fit = TRUE))
optim_results_matabuena_delay3 <- lapply(subjects_list, function(x) optim(par = c(255, 0.10, 0.10, 10, 10, 5, 5, 5, 5, 5, 5), fn = matabuena_function_mae, tl = x[,2], performance = x[,3], delay = 3, fit = TRUE))
optim_results_modelT <- lapply(subjects_list, function(x) optim(par = c(0.0845, 0.0030, 0.0815), fn = modelT_mae, tl = x[,2], performance = x[,3], fit = TRUE))



for (i in 1:6) {
  subjects_list[[i]]$perf_hat_banister <- get_performance_banister(optim_results_banister[[i]]$par, subjects_list[[i]][,2], performance = subjects_list[[i]][,3])
  subjects_list[[i]]$perf_hat_busso <- get_performance_busso(optim_results_busso[[i]]$par, subjects_list[[i]][,2], performance = subjects_list[[i]][,3])
  subjects_list[[i]]$perf_hat_matabuena_delay1 <- get_performance_matabuena(optim_results_matabuena_delay1[[i]]$par, subjects_list[[i]][,2], performance = subjects_list[[i]][,3], delay = 1)
  subjects_list[[i]]$perf_hat_modelT <- modelT_mae(optim_results_modelT[[i]]$par, subjects_list[[i]][,2], performance = subjects_list[[i]][,3])
  }


r2_banister <- sapply(subjects_list, function(x) cor(x$performance, x$perf_hat_banister, use = "complete.obs")^2)  
r2_busso <- sapply(subjects_list, function(x) cor(x$performance, x$perf_hat_busso, use = "complete.obs")^2)  
r2_matabuena <- sapply(subjects_list, function(x) cor(x$performance, x$perf_hat_matabuena_delay1, use = "complete.obs")^2)  

r2_banister
r2_busso
r2_matabuena

# Visualize results

subjects_list <- lapply(subjects_list, function(x) {
  x$week <- x$day/7
  return(x) #Creates a variable called 'Week'
})

subject_df <- plyr::ldply(subjects_list, data.frame)



colnames(subject_df)[colnames(subject_df)==".id"] <- "Subject"
subject_plots <- ggplot(subject_df, aes(x = week, y = performance)) +
  geom_line(aes(y = perf_hat_banister, color = "Predicted performance (Banister)"), size = 1) +
  geom_line(aes(y = perf_hat_busso, color = "Predicted performance (Busso)"), size = 1) +
  geom_line(aes(y = perf_hat_matabuena_delay1, color = "Predicted performance (Matabuena w/1 delay)"), size = 1) +
  geom_point(aes(y = performance, color = "Actual Performance")) +
  #scale_color_manual("", values = c("red", "blue" ,"green", "black"), labels = c("Predicted performance (Banister)", "Predicted performance (Busso)", "Predicted performance (Matabuena)" ,"Actual performance")) +
  guides(color = guide_legend(override.aes = list(linetype = c("blank", "solid", "solid", "solid"), shape = c(16, NA, NA, NA)), title = NULL)) +
  ylab("Performance") +
  xlab("Week") +
  scale_x_continuous(breaks = seq(0,16,2)) +
  #geom_label(data = subject_df, aes(x = 2, y = 400, label = paste("italic(R) ^ 2 == ", round(R2, 2))), parse = TRUE, size = 3) +
  #theme_minimal() +
  facet_wrap(~ Subject, ncol = 2) 

subject_plots








