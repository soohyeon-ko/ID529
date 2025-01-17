install.packages("devtools") 

# install our ID529tutorials package
devtools::install_github("ID529/ID529tutorials")
.
library(ID529tutorials)
available_tutorials('ID529tutorials')
run_tutorial('logic', 'ID529tutorials')

install.packages("renv")

library(readr)

##HW2

rm(list = ls())

devtools::install_github("ID529/ID529data")
library(ID529data)
library(dplyr)
library(ggplot2)

data <- data("nhanes_id529")

ggplot(data, aes(x = age)) +
  geom_area()


data <- data %>%
  mutate(height_m = height / 100,
         BMI = weight / (height_m^2))

summary(nhanes_id529$BMI)

library(ggplot2).

ggplot(nhanes_id529, aes(x = PFOS)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(
    title = "Distribution of PFOS Concentration",
    x = "PFOS Concentration",
    y = "Count"
  ) +
  theme_minimal()

ggplot(nhanes_id529, aes(x = height, y = PFOS)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Scatter Plot of BMI vs PFOS",
    x = "BMI",
    y = "PFOS Concentration"
  ) +
  theme_minimal()

ggplot(nhanes_id529, aes(x = PFOS, fill = sex_gender)) +
  geom_histogram(bins = 20, alpha = 0.7, position = "identity") +
  facet_wrap(~sex_gender) +
  labs(
    title = "PFOS Distribution by sex_gender",
    x = "PFOS Concentration",
    y = "Count"
  ) +
  theme_minimal()

cleaned_data <- nhanes_id529 %>% na.omit()

model <- lm(PFOS ~ BMI + age + sex_gender, data = cleaned_data)

summary(model)

library(broom)
model_tidy <- tidy(model)

write.csv(model_tidy, "regression_coefficients.csv", row.names = FALSE)

ggplot(model_tidy, aes(x = estimate, y = term)) +
  geom_pointrange(aes(xmin = estimate - std.error, xmax = estimate + std.error), color = "blue") +
  labs(
    title = "Forest Plot of Regression Coefficients",
    x = "Estimate",
    y = "Variable"
  ) +
  theme_minimal()
