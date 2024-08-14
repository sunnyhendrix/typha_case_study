# Load necessary libraries
library(tidyverse)
library(ggplot2)

# Load the data
data <- read.csv("cattail_data.csv")

# Convert 'survival' to a factor
data$survival <- as.factor(data$survival)

# Check the structure of the data
str(data)

# 1. Test if weeks_submerged affects survival
# Create a logistic regression model
model_weeks_submerged <- glm(survival ~ weeks_submerged, data = data, family = binomial)

# Summary of the model
summary(model_weeks_submerged)

# 2. Test if height, flowers, or shoots affect survival
# Create a logistic regression model with multiple predictors
model_multiple <- glm(survival ~ height + flowers + shoots, data = data, family = binomial)

# Summary of the model
summary(model_multiple)

# 3. Graphical representation of the number of survival by weeks_submerged
# Create a bar plot of survival by weeks_submerged
ggplot(data, aes(x = as.factor(weeks_submerged), fill = survival)) +
  geom_bar(position = "fill") +
  labs(x = "Weeks Submerged", y = "Proportion", fill = "Survival") +
  ggtitle("Proportion of Survival by Weeks Submerged") +
  theme_minimal()

# Graph the effect of height, flowers, and shoots on survival
# Plot survival vs. height
ggplot(data, aes(x = height, fill = survival)) +
  geom_histogram(binwidth = 10, position = "fill") +
  labs(x = "Height", y = "Proportion", fill = "Survival") +
  ggtitle("Survival Proportion by Height") +
  theme_minimal()

# Plot survival vs. flowers
ggplot(data, aes(x = flowers, fill = survival)) +
  geom_histogram(binwidth = 1, position = "fill") +
  labs(x = "Flowers", y = "Proportion", fill = "Survival") +
  ggtitle("Survival Proportion by Flowers") +
  theme_minimal()

# Plot survival vs. shoots
ggplot(data, aes(x = shoots, fill = survival)) +
  geom_histogram(binwidth = 1, position = "fill") +
  labs(x = "Shoots", y = "Proportion", fill = "Survival") +
  ggtitle("Survival Proportion by Shoots") +
  theme_minimal()
