library(dplyr)
data<-read_tsv("Số liệu Hường - Trang tính2.tsv")
data<-data%>%
  pivot_longer(-method,values_to="values",names_to="sample")
perform_f_test <- function(df, sample_name) {
  # Subset data for the sample
  sample_data <- df %>% filter(sample == sample_name)
  
  # Check if there are enough groups for the F-test
  if (length(unique(sample_data$method)) < 2) {
    return(NA)  # Not enough groups for F-test
  }
  
  # Perform F-test
  f_test <- var.test(values ~ method, data = sample_data)
  
  # Return F statistic and p-value
  return(c(F_statistic = f_test$statistic, p_value = f_test$p.value))
}
samples <- unique(data$sample)
f_test_results <- lapply(samples, function(s) perform_f_test(data, s))
results_df <- do.call(rbind, f_test_results)
colnames(results_df) <- c("F_statistic", "p_value")
results_df <- data.frame(sample = samples, results_df)
write.table(results_df, "f_test_results.tsv", sep = "\t", row.names = FALSE, quote = FALSE)

perform_t_test <- function(df, sample_name) {
  # Subset data for the sample
  sample_data <- df %>% filter(sample == sample_name)
  
  # Check if there are exactly two groups for the t-test
  if (length(unique(sample_data$method)) != 2) {
    return(c(NA, NA))  # Not enough or too many groups for t-test
  }
  
  # Split the data into two groups
  group1 <- sample_data$values[sample_data$method == unique(sample_data$method)[1]]
  group2 <- sample_data$values[sample_data$method == unique(sample_data$method)[2]]
  
  # Perform t-test
  t_test <- t.test(group1, group2)
  
  # Return t statistic and p-value
  return(c(T_statistic = t_test$statistic, p_value = t_test$p.value))
}
samples <- unique(data$sample)

# Perform t-test for each sample and store the results
t_test_results <- lapply(samples, function(s) perform_t_test(data, s))

# Combine results into a data frame
results_df <- do.call(rbind, t_test_results)
colnames(results_df) <- c("T_statistic", "p_value")
results_df <- data.frame(sample = samples, results_df)

# Save results to a TSV file
write.table(results_df, "t_test_results.tsv", sep = "\t", row.names = FALSE, quote = FALSE)
