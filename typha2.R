# Load necessary libraries
library(ggplot2)
library(dplyr)

# Load the data
cattail_data <- read.csv("cattail_data.csv")

# Convert relevant columns to appropriate data types
cattail_data$survival <- as.factor(cattail_data$survival)
cattail_data$weeks_submerged <- as.factor(cattail_data$weeks_submerged)

# Statistical test to determine if weeks_submerged affects survival
weeks_survival_table <- table(cattail_data$weeks_submerged, cattail_data$survival)
chi2_test_weeks <- chisq.test(weeks_survival_table)

# Print the result of the chi-square test
print(chi2_test_weeks)

# Plot survival by weeks_submerged
ggplot(cattail_data, aes(x = weeks_submerged, fill = survival)) +
  geom_bar(position = "dodge") +
  labs(title = "Survival by Weeks Submerged", x = "Weeks Submerged", y = "Count") +
  scale_fill_manual(values = c("no" = "red", "yes" = "green"))

# Logistic regression to test whether height, flowers, or shoots affect survival
logistic_model <- glm(survival ~ height + flowers + shoots, data = cattail_data, family = binomial)

# Print the summary of the logistic regression model
summary(logistic_model)

# Plot the relationship between height, flowers, shoots, and survival

# Plot height vs survival
ggplot(cattail_data, aes(x = height, fill = survival)) +
  geom_histogram(binwidth = 10, position = "dodge") +
  labs(title = "Survival by Height", x = "Height (cm)", y = "Count") +
  scale_fill_manual(values = c("no" = "red", "yes" = "green"))

# Plot flowers vs survival
ggplot(cattail_data, aes(x = flowers, fill = survival)) +
  geom_bar(position = "dodge") +
  labs(title = "Survival by Number of Flowers", x = "Number of Flowers", y = "Count") +
  scale_fill_manual(values = c("no" = "red", "yes" = "green"))

# Plot shoots vs survival
ggplot(cattail_data, aes(x = shoots, fill = survival)) +
  geom_bar(position = "dodge") +
  labs(title = "Survival by Number of Shoots", x = "Number of Shoots", y = "Count") +
  scale_fill_manual(values = c("no" = "red", "yes" = "green"))
