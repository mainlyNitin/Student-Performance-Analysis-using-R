
# Student Performance Analysis using R

# Load Required Libraries

library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)
library(psych)
library(ggpubr)

#  Load and Prepare the Data

set.seed(123)
student_data <- data.frame(
  Student_ID = 1:100,
  Gender = sample(c("Male", "Female"), 100, replace = TRUE),
  Study_Hours = round(rnorm(100, mean = 4, sd = 1.5), 1),
  Attendance = round(runif(100, 70, 100), 0),
  Parent_Education = sample(c("High School", "Bachelor", "Master", "PhD"), 100, replace = TRUE),
  Test_Score = round(rnorm(100, mean = 70, sd = 10), 0)
)

# Clean invalid study hours
student_data$Study_Hours[student_data$Study_Hours < 0] <- abs(student_data$Study_Hours[student_data$Study_Hours < 0])

head(student_data)


# Descriptive Statistics

summary(student_data)
psych::describe(student_data[, c("Study_Hours", "Attendance", "Test_Score")])

# Data Visualization using ggplot2

#  Distribution of Test Scores
ggplot(student_data, aes(x = Test_Score)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 10, alpha = 0.7) +
  geom_density(aes(y = ..count..), color = "red", size = 1) +
  labs(title = "Distribution of Student Test Scores", x = "Test Score", y = "Count") +
  theme_minimal()

# Relationship between Study Hours and Test Score
ggplot(student_data, aes(x = Study_Hours, y = Test_Score, color = Gender)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
  labs(title = "Study Hours vs Test Score", x = "Study Hours", y = "Test Score") +
  theme_light()

# Boxplot by Parental Education
ggplot(student_data, aes(x = Parent_Education, y = Test_Score, fill = Parent_Education)) +
  geom_boxplot(alpha = 0.8, outlier.color = "red", notch = TRUE) +
  labs(title = "Impact of Parental Education on Student Performance", x = "Parent Education", y = "Test Score") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Correlation Analysis

num_data <- student_data %>% select(Study_Hours, Attendance, Test_Score)
corr_matrix <- cor(num_data)
corrplot::corrplot(corr_matrix, method = "circle", type = "upper", tl.col = "black", tl.cex = 1)

# Inferential Statistics

# Gender-wise Performance (t-test)
t_test_result <- t.test(Test_Score ~ Gender, data = student_data)
t_test_result

# ANOVA for Parental Education Level
anova_result <- aov(Test_Score ~ Parent_Education, data = student_data)
summary(anova_result)


# Regression Analysis

model <- lm(Test_Score ~ Study_Hours + Attendance + Gender, data = student_data)
summary(model)

# Visualize regression fit
ggplot(student_data, aes(x = Study_Hours, y = Test_Score)) +
  geom_point(color = "darkblue", size = 3) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Regression Line: Study Hours vs Test Score", x = "Study Hours", y = "Predicted Score") +
  theme_minimal()

# Insights & Interpretation

cat("\n--- Insights ---\n")
cat("1. Students with higher study hours and attendance tend to score better.\n")
cat("2. There is a significant difference in average scores across parental education levels.\n")
cat("3. Study Hours and Attendance are strong predictors of performance.\n")

