# Load necessary libraries
library(tidyverse)
library(ggplot2)

# Load the data
data <- read.csv("cattail_data.csv")

# Convert categorical variables to factors
data$survival <- factor(data$survival, levels = c("no", "yes"))
data$weeks_submerged <- as.factor(data$weeks_submerged)

# Check the data structure
str(data)

# Statistical tests

# Test if weeks_submerged affects survival using Chi-Square Test
chisq_test_weeks <- chisq.test(table(data$weeks_submerged, data$survival))
print(chisq_test_weeks)

# Test if height, flowers, or shoots affect survival using logistic regression
# Convert 'survival' to binary for logistic regression
data$survival_binary <- ifelse(data$survival == "yes", 1, 0)

# Logistic regression model
logistic_model <- glm(survival_binary ~ height + flowers + shoots, data = data, family = binomial)
summary(logistic_model)

# Graphical representation

# Plot number of survival by weeks_submerged
ggplot(data, aes(x = weeks_submerged, fill = survival)) +
  geom_bar(position = "dodge") +
  labs(title = "Number of Survival by Weeks Submerged",
       x = "Weeks Submerged",
       y = "Count",
       fill = "Survival") +
  theme_minimal()

# Plot survival by height
ggplot(data, aes(x = height, fill = survival)) +
  geom_histogram(position = "dodge", binwidth = 10) +
  labs(title = "Survival by Height",
       x = "Height",
       y = "Count",
       fill = "Survival") +
  theme_minimal()

# Plot survival by flowers
ggplot(data, aes(x = flowers, fill = survival)) +
  geom_histogram(position = "dodge", binwidth = 1) +
  labs(title = "Survival by Number of Flowers",
       x = "Number of Flowers",
       y = "Count",
       fill = "Survival") +
  theme_minimal()

# Plot survival by shoots
ggplot(data, aes(x = shoots, fill = survival)) +
  geom_histogram(position = "dodge", binwidth = 1) +
  labs(title = "Survival by Number of Shoots",
       x = "Number of Shoots",
       y = "Count",
       fill = "Survival") +
  theme_minimal()
