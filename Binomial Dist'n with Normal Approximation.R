library(ggplot2)

#Example of Normal Approximation used correctly i.e np, nq > 5

#Start with our binomial setup. Here, n = 25 and p = .5. Thus, np and nq > 5.

# Create a data frame for the binomial distribution from 5 to 20
df_binomial1 <- data.frame(x = seq(5, 20))
df_binomial1$y <- dbinom(df_binomial1$x, size = 25, prob = 0.5)

# Define the parameters for the normal distribution
mean_value1 <- 12.5 #This is np
sd_value1 <- 2.5  # Updated standard deviation, sqrt(npq)

# Create a data frame for the normal distribution
df_normal1 <- data.frame(x = seq(5, 20, by = 0.1))
df_normal1$y <- dnorm(df_normal1$x, mean = mean_value1, sd = sd_value1)


# Plot 1: Normal Approximation (np > 5)
plot1 <- ggplot() +
  geom_line(data = df_normal1, aes(x, y), color = "#1f78b4", linewidth = 1.2) +  # Blue line for normal distribution
  geom_point(data = df_binomial1, aes(x, y), color = "#e31a1c", size = 3) +  # Red points for binomial distribution
  geom_segment(data = df_binomial1, aes(x, y, xend = x, yend = 0), 
               color = "#e31a1c", linetype = 'dashed') +  # Dashed red lines for binomial segments
  labs(
    title = "Normal Distribution with Binomial Distribution Points",
    subtitle = "Accurate Normal Approximation (np & nq > 5)",
    x = "X",
    y = "Probability"
  ) +
  theme_minimal(base_size = 15) +  # Use a clean minimal theme
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),  # Centered bold title
    plot.subtitle = element_text(hjust = 0.5, size = 14, face = "italic"),  # Centered italic subtitle
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(color = "gray90")  # Lighter grid lines for a clean look
  )

print(plot1)


#Example of Normal Approximation used wrong, np < 5

#Start with our binomial setup. Here, n = 25 and p = .5. Thus, np and nq > 5.

# Create a data frame for the binomial distribution from 5 to 20
df_binomial2 <- data.frame(x = seq(0, 8))
df_binomial2$y <- dbinom(df_binomial2$x, size = 25, prob = 0.1)

# Define the parameters for the normal distribution
mean_value2 <- 25*.1 #This is np
sd_value2 <- sqrt(25*.1*.9)  # Updated standard deviation, sqrt(npq)

# Create a data frame for the normal distribution
df_normal2 <- data.frame(x = seq(0, 8, by = 0.1))
df_normal2$y <- dnorm(df_normal2$x, mean = mean_value2, sd = sd_value2)


# Plot 2: Normal Approximation (np < 5)
plot2 <- ggplot() +
  geom_line(data = df_normal2, aes(x, y), color = "#1f78b4", size = 1.2) +  # Blue line for normal distribution
  geom_point(data = df_binomial2, aes(x, y), color = "#e31a1c", size = 3) +  # Red points for binomial distribution
  geom_segment(data = df_binomial2, aes(x, y, xend = x, yend = 0), 
               color = "#e31a1c", linetype = 'dashed') +  # Dashed red lines for binomial segments
  labs(
    title = "Normal Distribution with Binomial Distribution Points",
    subtitle = "Inaccurate Normal Approximation (np < 5)",
    x = "X",
    y = "Probability"
  ) +
  theme_minimal(base_size = 15) +  # Use a clean minimal theme
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),  # Centered bold title
    plot.subtitle = element_text(hjust = 0.5, size = 14, face = "italic"),  # Centered italic subtitle
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(color = "gray90")  # Lighter grid lines for a clean look
  )

print(plot2)