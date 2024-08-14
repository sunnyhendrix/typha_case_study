# Load necessary libraries
library(ggplot2)
library(dplyr)
library(ggpubr)

# Read the data
cattail_data <- read.csv("cattail_data.csv")

# Convert columns to appropriate data types
cattail_data$survival <- as.factor(cattail_data$survival)
cattail_data$weeks_submerged <- as.factor(cattail_data$weeks_submerged)

# Summary of the data
summary(cattail_data)

# Test if weeks_submerged affects survival using Chi-square test
weeks_survival_table <- table(cattail_data$weeks_submerged, cattail_data$survival)
chi_square_test <- chisq.test(weeks_survival_table)
print(chi_square_test)

# Plot survival by weeks_submerged
ggplot(cattail_data, aes(x = weeks_submerged, fill = survival)) +
  geom_bar(position = "dodge") +
  labs(title = "Survival by Weeks Submerged", x = "Weeks Submerged", y = "Count of Plants") +
  theme_minimal()

# Test if height affects survival using logistic regression
height_model <- glm(survival ~ height, data = cattail_data, family = "binomial")
summary(height_model)

# Plot survival by height
ggplot(cattail_data, aes(x = height, fill = survival)) +
  geom_histogram(bins = 30, position = "dodge") +
  labs(title = "Survival by Height", x = "Height", y = "Count of Plants") +
  theme_minimal()

# Test if flowers affect survival using logistic regression
flowers_model <- glm(survival ~ flowers, data = cattail_data, family = "binomial")
summary(flowers_model)

# Plot survival by number of flowers
ggplot(cattail_data, aes(x = flowers, fill = survival)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  labs(title = "Survival by Number of Flowers", x = "Number of Flowers", y = "Count of Plants") +
  theme_minimal()

# Test if shoots affect survival using logistic regression
shoots_model <- glm(survival ~ shoots, data = cattail_data, family = "binomial")
summary(shoots_model)

# Plot survival by number of shoots
ggplot(cattail_data, aes(x = shoots, fill = survival)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  labs(title = "Survival by Number of Shoots", x = "Number of Shoots", y = "Count of Plants") +
  theme_minimal()
