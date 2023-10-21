# Read the CSV file
data <- read.csv("birds_collision.csv")
str(data)

# Start PDF file
pdf("Normal_plots.pdf")

# Define a function to create normal distribution plots
create_normal_distribution_plot <- function(variable_data, variable_name) {
  hist(variable_data, probability = TRUE, col = "lightblue", 
       main = paste("Normal Distribution of", variable_name), 
       xlab = variable_name, ylab = "Density")

  # Add a blue curve representing the theoretical normal distribution
  curve(dnorm(x, mean = mean(variable_data), sd = sd(variable_data)), 
        col = "blue", lwd = 2, add = TRUE)

  # Add a red density plot
  dens <- density(variable_data)
  lines(dens, col = "red", lwd = 2)
}

# Specify variables for analysis
variables <- c("speed", "height", "num_engs", "ac_mass")

# Generate normal distribution plots for each variable
for (variable in variables) {
  create_normal_distribution_plot(data[[variable]], variable)
}

# End PDF file
dev.off()
