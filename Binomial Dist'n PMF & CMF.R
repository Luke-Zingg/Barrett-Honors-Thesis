library(ggplot2)

# Set the parameters for the binomial distribution
n <- 15
p <- 0.5

# Create a data frame with x values and corresponding probabilities
x <- 0:n
probabilities <- dbinom(x, size = n, prob = p)
cumulative_probabilities <- cumsum(probabilities)

data <- data.frame(x = x, probabilities = probabilities, cumulative_prob = cumulative_probabilities)

# Function to create consistent styling for both plots
create_plot <- function(data, y_col, y_label, plot_title) {
  ggplot(data, aes(x, !!sym(y_col))) +
    geom_segment(aes(x = x, xend = x, y = 0, yend = !!sym(y_col)), 
                 linetype = "dotted", color = "gray50", linewidth = 0.8) +  # Light gray dotted lines
    geom_point(shape = 21, color = "#6D5A50", fill = "#D3B8AE", size = 5) +  # Warm tones for points
    labs(x = "X", 
         y = y_label, 
         title = plot_title, 
         subtitle = paste("(n =", n, ", p =", p, ")")) +
    theme_minimal(base_size = 15) +  # Clean minimal theme with larger text
    theme(
      plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "#6D5A50"),  # Centered bold title
      plot.subtitle = element_text(hjust = 0.5, size = 14, face = "italic", color = "#6D5A50"),  # Centered italic subtitle
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      panel.grid.major = element_line(color = "gray90"),  # Soft gray gridlines
      panel.grid.minor = element_blank(),  # No minor grid lines for a cleaner look
      panel.border = element_blank(),  # No border
      plot.margin = margin(10, 10, 10, 10)  # Add spacing around the plot
    )
}

# Plot for the PDF (Probability Density Function)
plot_pdf <- create_plot(data, "probabilities", "Probability", "Binomial Distribution PDF")

# Plot for the CMF (Cumulative Mass Function)
plot_cmf <- create_plot(data, "cumulative_prob", "Cumulative Probability", "Binomial Distribution CMF")

# Print the plots
print(plot_pdf)
print(plot_cmf)