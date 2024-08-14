# Load necessary libraries
library(tidyverse)
library(ggplot2)

# Load the data
data <- read.csv("cattail_data.csv")

# Convert relevant columns to factors
data$survival <- as.factor(data$survival)
data$weeks_submerged <- as.factor(data$weeks_submerged)

# Summary of data
summary(data)

# Logistic regression models
# Model 1: Weeks submerged effect on survival
model_weeks <- glm(survival ~ weeks_submerged, data = data, family = binomial)

# Model 2: Effect of height, flowers, and shoots on survival
model_effects <- glm(survival ~ height + flowers + shoots, data = data, family = binomial)

# Summary of models
summary(model_weeks)
summary(model_effects)

# Predictions for graphical representation
data$predicted_weeks <- predict(model_weeks, type = "response")
data$predicted_effects <- predict(model_effects, type = "response")

# Plot: Survival by weeks submerged
ggplot(data, aes(x = weeks_submerged, fill = survival)) +
  geom_bar(position = "fill") +
  labs(title = "Survival by Weeks Submerged", x = "Weeks Submerged", y = "Proportion", fill = "Survival") +
  theme_minimal()

# Plot: Effect of height on survival
ggplot(data, aes(x = height, fill = survival)) +
  geom_histogram(binwidth = 10, position = "fill") +
  labs(title = "Survival by Height", x = "Height", y = "Proportion", fill = "Survival") +
  theme_minimal()

# Plot: Effect of flowers on survival
ggplot(data, aes(x = flowers, fill = survival)) +
  geom_histogram(binwidth = 1, position = "fill") +
  labs(title = "Survival by Number of Flowers", x = "Number of Flowers", y = "Proportion", fill = "Survival") +
  theme_minimal()

# Plot: Effect of shoots on survival
ggplot(data, aes(x = shoots, fill = survival)) +
  geom_histogram(binwidth = 1, position = "fill") +
  labs(title = "Survival by Number of Shoots", x = "Number of Shoots", y = "Proportion", fill = "Survival") +
  theme_minimal()
