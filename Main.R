### generate the data
library(dplyr)
setwd("C:/Users/Nie00004/OneDrive - Universiteit Utrecht/Desktop/github_sample")
set.seed(123)

n <- 500

simulated_data <- tibble(
  id = 1:n,
  age = round(rnorm(n, mean = 50, sd = 12)),
  sex = sample(c("Male", "Female"), n, replace = TRUE),
  bmi = round(rnorm(n, 27, 4), 1),
  smoker = sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.3, 0.7))
) %>%
  mutate(
    diabetes = rbinom(n, 1, plogis(-5 + 0.05 * age + 0.1 * bmi + 0.8 * (sex == "Male") + 0.5 * (smoker == "Yes")))
  )

write.csv(simulated_data, "simulated_diabetes_data.csv", row.names = FALSE)



### read the data, check for distribution
library(ggplot2)
data <- read.csv("simulated_diabetes_data.csv")

summary(data)

ggplot(data, aes(x = age, fill = factor(diabetes))) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.6) +
  labs(title = "Age Distribution by Diabetes Status")


### modeling
data <- read.csv("simulated_diabetes_data.csv")

model <- glm(diabetes ~ age + bmi + sex + smoker, data = data, family = binomial)

summary(model)

sink("model_summary.txt")
print(summary(model))
sink()

