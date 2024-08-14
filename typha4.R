# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)
library(broom)

# Load the data
data <- read_csv("cattail_data.csv")

# Convert relevant columns to appropriate data types
data <- data %>%
  mutate(
    survival = as.factor(survival),
    weeks_submerged = as.factor(weeks_submerged)
  )

# 1. Statistical Test: Does weeks_submerged affect survival?
weeks_submerged_test <- chisq.test(table(data$weeks_submerged, data$survival))

# Print the result of the test
print(weeks_submerged_test)

# 2. Plot: Number of survivals by weeks submerged
ggplot(data, aes(x = weeks_submerged, fill = survival)) +
  geom_bar(position = "dodge") +
  labs(title = "Survival by Weeks Submerged", x = "Weeks Submerged", y = "Count", fill = "Survival") +
  theme_minimal()

# 3. Statistical Tests: Do height, flowers, or shoots affect survival?
# Height
height_test <- glm(survival ~ height, data = data, family = binomial)
summary(height_test)

# Flowers
flowers_test <- glm(survival ~ flowers, data = data, family = binomial)
summary(flowers_test)

# Shoots
shoots_test <- glm(survival ~ shoots, data = data, family = binomial)
summary(shoots_test)

# 4. Plot: Survival by height
ggplot(data, aes(x = height, fill = survival)) +
  geom_histogram(binwidth = 10, position = "dodge") +
  labs(title = "Survival by Height", x = "Height (cm)", y = "Count", fill = "Survival") +
  theme_minimal()

# 5. Plot: Survival by number of flowers
ggplot(data, aes(x = flowers, fill = survival)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  labs(title = "Survival by Number of Flowers", x = "Number of Flowers", y = "Count", fill = "Survival") +
  theme_minimal()

# 6. Plot: Survival by number of shoots
ggplot(data, aes(x = shoots, fill = survival)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  labs(title = "Survival by Number of Shoots", x = "Number of Shoots", y = "Count", fill = "Survival") +
  theme_minimal()
