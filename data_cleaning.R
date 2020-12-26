# import packages
library(readxl)
library(ggplot2)
library(dplyr)

# import dataset
subjects_data <- read_excel("Busso2017su1.xlsx")

# make dataset into a list
training_cols <- subjects_data[seq(2, 13, by = 2)] #Subsets all the 'training dose'  columns
performance_cols <- subjects_data[seq(3, 13, by = 2)] #Subsets all the 'performance' columns
subjects_list <- vector("list", 6) #Creates an empty list vector

for (i in 1:6) {
  subjects_list[i] <- list(cbind(subjects_data[1], training_cols[i], performance_cols[i])) 
}

names(subjects_list) <- paste("Subject", 1:6, sep ="") #Sets the name of each data frame in the list to 'Athlete1', 2, etc

subjects_list <- lapply(subjects_list, function(x) { #Sets the name of each column within the data frames
  colnames(x) <- c("day", "tl", "performance")
  return(x)
})

str(subjects_list)

