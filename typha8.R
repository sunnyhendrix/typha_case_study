# Load required libraries
library(tidyverse)  # For data manipulation and visualization
library(broom)      # For tidying model outputs
library(ggplot2)    # For creating plots

# Load the data
data <- read.csv("cattail_data.csv")

# Convert 'survival' to a factor for binary outcome
data$survival <- as.factor(data$survival)

# Check the data structure
str(data)

# 1. Statistical test for weeks_submerged
# Logistic regression to check if weeks_submerged affects survival
model_weeks_submerged <- glm(survival ~ weeks_submerged, data = data, family = binomial)

# Summarize the model
summary(model_weeks_submerged)
tidy(model_weeks_submerged)

# 2. Graphical representation of survival by weeks_submerged
ggplot(data, aes(x = factor(weeks_submerged), fill = survival)) +
  geom_bar(position = "fill") +
  labs(x = "Weeks Submerged", y = "Proportion", fill = "Survival") +
  ggtitle("Proportion of Survival by Weeks Submerged") +
  theme_minimal()

# 3. Statistical tests for height, flowers, and shoots
# Logistic regression to check if height, flowers, or shoots affect survival
model_height <- glm(survival ~ height, data = data, family = binomial)
model_flowers <- glm(survival ~ flowers, data = data, family = binomial)
model_shoots <- glm(survival ~ shoots, data = data, family = binomial)

# Summarize the models
summary(model_height)
summary(model_flowers)
summary(model_shoots)

# Tidy the models
tidy(model_height)
tidy(model_flowers)
tidy(model_shoots)

# 4. Graphical representations for height, flowers, and shoots

# Height vs Survival
ggplot(data, aes(x = height, fill = survival)) +
  geom_histogram(binwidth = 10, position = "fill") +
  labs(x = "Height", y = "Proportion", fill = "Survival") +
  ggtitle("Proportion of Survival by Height") +
  theme_minimal()

# Flowers vs Survival
ggplot(data, aes(x = flowers, fill = survival)) +
  geom_bar(position = "fill") +
  labs(x = "Number of Flowers", y = "Proportion", fill = "Survival") +
  ggtitle("Proportion of Survival by Number of Flowers") +
  theme_minimal()

# Shoots vs Survival
ggplot(data, aes(x = shoots, fill = survival)) +
  geom_bar(position = "fill") +
  labs(x = "Number of Shoots", y = "Proportion", fill = "Survival") +
  ggtitle("Proportion of Survival by Number of Shoots") +
  theme_minimal()
