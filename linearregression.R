install.packages("ggplot2")
install.packages("gridExtra")
install.packages("shiny")
install.packages("tidyverse")

library(tidyverse)
library(ggplot2)
library(gridExtra)
library(shiny)


insurance <- read.csv("insurance.csv")
insurance$sex <- as.factor(insurance$sex)
insurance$smoker <- as.factor(insurance$smoker)
insurance$region <- as.factor(insurance$region)
head(insurance)

#initial exploratory data analysis
response_histogram <- ggplot(insurance, aes(x=charges)) + geom_histogram()
response_histogram
h3("Figure 1: Distriution of the Response Variable (Medical Insurance Cost)")

options(repr.plot.width = 10, repr.plot.height = 10)
tags$div(HTML("<h3>Figure 2: Distriutions of the Response Variable (Medical Insurance Cost) <br> across Various Regions of the United States</h3>"))
ggplot(insurance, aes(x = region, y = charges)) +
  geom_boxplot() +
  labs(x = "Region", y = "Charges") +
  ggtitle("Box Plot of Charges by Region")

tags$div(HTML("<h3>Figure 3: Distriutions of the Response Variable (Medical Insurance Cost) <br> across Different Biological Sex</h3>"))
ggplot(insurance, aes(x = sex, y = charges)) +
  geom_boxplot() +
  labs(x = "Sex", y = "Charges") +
  ggtitle("Box Plot of Charges by Sex")

tags$div(HTML("<h3>Figure 4: Distriutions of the Response Variable (Medical Insurance Cost) <br> based on Beneficiary's Smoking Habit</h3>"))
ggplot(insurance, aes(x = smoker, y = charges)) +
  geom_boxplot() +
  labs(x = "Smoker", y = "Charges") +
  ggtitle("Box Plot of Charges by Smoker")

full_model <- lm(charges~age + sex + bmi + children + smoker + region, data=insurance)
summary(full_model)

model1 <- lm(charges~age + bmi + children + smoker + region, data=insurance)
summary(model1)

h3("Figure 5: Model 1 Residual Plot against the Fitted Values")
plot(x=model1$fitted, y=model1$residual)
abline(0,0)

h3("Figure 6: Model 1 Normal Q-Q Plot")
qqnorm(model1$residuals)
qqline(model1$residuals)

options(repr.plot.width = 20, repr.plot.height = 10)
plot_age_sex <- ggplot(insurance, aes(x=age, y=charges, color=sex)) +
  geom_point()
plot_age_smoker <- ggplot(insurance, aes(x=age, y=charges, color=smoker)) +
  geom_point()
plot_age_region <- ggplot(insurance, aes(x=age, y=charges, color=region)) +
  geom_point()

h3("Figure 7: Scatter Plots of Charges versus Age with Different Categorical Variables")
grid.arrange(plot_age_sex, plot_age_smoker, plot_age_region, ncol = 3)

options(repr.plot.width = 20, repr.plot.height = 10)
plot_bmi_sex <- ggplot(insurance, aes(x=bmi, y=charges, color=sex)) +
  geom_point()
plot_bmi_smoker <- ggplot(insurance, aes(x=bmi, y=charges, color=smoker)) +
  geom_point()
plot_bmi_region <- ggplot(insurance, aes(x=bmi, y=charges, color=region)) +
  geom_point()

h3("Figure 8: Scatter Plots of Charges versus BMI with Different Categorical Variables")
grid.arrange(plot_bmi_sex, plot_bmi_smoker, plot_bmi_region, ncol = 3)

model2 <- lm(charges~age*smoker + bmi*smoker + children + region, data=insurance)
summary(model2)

model3 <- lm(charges~age + bmi*smoker + children + region, data=insurance)
summary(model3)

h3("Figure 9: Model 3 Residual Plot against the Fitted Values")
plot(x=model3$fitted, y=model3$residual)
abline(0,0)

h3("Figure 10: Model 3 Normal Q-Q Plot")
qqnorm(model3$residuals)
qqline(model3$residuals)

h3("Figure 11: Model 3 Residual Plot against Age")
plot(x=insurance$age, y=model3$residual)
abline(0,0)

model4 <- lm(charges~I(age^2)+ age + bmi*smoker + children + region, data=insurance)
summary(model4)

cor(insurance$age^2, insurance$age)

age_mean <- mean(insurance$age)
model5 <- lm(charges~I((age-age_mean)^2)+ age + bmi*smoker + children + region, data=insurance)
summary(model5)

h3("Figure 12: Model 5 Residual Plot against Fitted Values")
plot(x=model5$fitted, y=model5$residuals)
abline(0,0)

h3("Figure 13: Model 5 Normal Q-Q Plot")
qqnorm(model5$residuals)
qqline(model5$residuals)

model5_residuals <- data.frame(residuals=model5$residuals)
residuals_histogram <- ggplot(model5_residuals, aes(x=residuals)) + geom_histogram()
residuals_histogram
h3("Figure 14: Distribution of Model 5 Residuals")

