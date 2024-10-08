library(ggplot2)

# Define the parameters
sample_sd <- 20         # Replace with your sample standard deviation
mean_diff <- 1        # Difference between sample mean and hypothesized mean
alpha <- 0.05

# Function to compute p-value for a given sample size
compute_p_value <- function(sample_size) {
  t_stat <- mean_diff / (sample_sd / sqrt(sample_size))
  p_value <- 2 * pt(-abs(t_stat), df = sample_size - 1)
  return(p_value)
}

# Generate sample sizes and compute p-values until p-value is <= alpha
sample_sizes <- c()
p_values <- c()
for (n in 2:20000) {
  p_value <- compute_p_value(n)
  sample_sizes <- c(sample_sizes, n)
  p_values <- c(p_values, p_value)
  if (p_value <= alpha) {
    break
  }
}

# Determine the step size to have no more than 25 points
total_points <- length(sample_sizes)
step_size <- max(1, ceiling(total_points / 25))

# Filter the sample sizes and p-values
filtered_sample_sizes <- sample_sizes[seq(1, total_points, by = step_size)]
filtered_p_values <- p_values[seq(1, total_points, by = step_size)]

# Create a data frame for plotting
p_value_data <- data.frame(sample_size = filtered_sample_sizes, 
                           p_value = filtered_p_values)

# Determine pretty breaks for the x-axis
x_breaks <- pretty(filtered_sample_sizes, n = 10)

library(ggplot2)

# Plotting the data with improved theme and title
ggplot(p_value_data, aes(x = sample_size, y = p_value)) +
  geom_vline(xintercept = 0, linetype = "solid", color = "gray70") +  # Light grey x-axis line
  geom_hline(yintercept = 0, linetype = "solid", color = "gray70") +  # Light grey y-axis line
  geom_hline(yintercept = alpha, linetype = "dashed", color = "#DC143C", size = .8) +  # Red dashed line for alpha
  geom_segment(aes(x = sample_size, xend = sample_size, y = p_value, yend = 0), 
               linetype = "dashed", color = "#1f77b4") +  # Blue dashed lines
  geom_point(size = 3, color = "#ab8150") +  # Open circles with a larger size
  labs(
    title = "P-values of a Two-Tailed t-Test",
    subtitle = paste("Statistical significance reached at sample size =", total_points),
    x = "Sample Size",
    y = "P-value"
  ) +
  theme_classic(base_size = 15) +  # Clean, simple theme
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),  # Centered and bold title
    plot.subtitle = element_text(hjust = 0.5, size = 12, face = "italic"),  # Centered italic subtitle
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(color = "gray90"),  # Light grey grid lines
    panel.grid.minor = element_blank(),  # No minor grid lines
    panel.border = element_blank(),  # No border
    plot.margin = margin(10, 10, 10, 10)  # Added margin for better spacing
  ) +
  scale_x_continuous(breaks = x_breaks)
