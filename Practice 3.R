# Practice 3

# Install and Load necessary libraries
install.packages('car')
install.packages('alr4')
install.packages('faraway')

library(car)
library(alr4)
library(faraway)

# Question 2

# read CSV file
car <- read.csv('D:/UMKC/Spring 2024/CS5590/Assignment 3/car.data.csv')

View(car) # view data frame

# create a new data frame with all the columns we choose for our model
new_car <- car[, c('weight',"mpg", "cylinders", "displacement","hp", 'acceleration', 'modelyr','foreign')]

View(new_car) # view data frame

# summary statistics for all the variables in the model
summary(new_car$weight)
summary(new_car$mpg)
summary(new_car$cylinders)
summary(new_car$displacement)
summary(new_car$hp)
summary(new_car$acceleration)
summary(new_car$modelyr)
summary(new_car$foreign)

#standard deviation or all the variables in the model
sd(new_car$weight)
sd(new_car$mpg)
sd(new_car$cylinders)
sd(new_car$displacement)
sd(new_car$hp)
sd(new_car$acceleration)
sd(new_car$modelyr)
sd(new_car$foreign)

# Question 3

# pairwise correlation matrix
cor(new_car[, c("weight", "mpg", "cylinders", "displacement", "hp","acceleration","modelyr")])


# Question 4

# Test 1
# multiple regression
model <- lm(weight ~ mpg + cylinders + displacement + hp + acceleration + foreign + modyr70 + modyr71 
            + modyr72 + modyr73 + modyr74 + modyr75 + modyr76 + modyr77 + modyr78 + modyr79 
            + modyr80 + modyr81 + modyr82, data = car)

# Summarize the model
summary(model)

# Extract the independent variables from your model
independent_vars <- model.matrix(~ mpg + cylinders + displacement + hp + acceleration + foreign + modyr70 + modyr71 
                                 + modyr72 + modyr73 + modyr74 + modyr75 + modyr76 + modyr77 + modyr78 + modyr79 
                                 + modyr80 + modyr81 + modyr82, data = car)

# Calculate VIF for the independent variables
vif_values <- vif(independent_vars)
print(vif_values)


# Test 2
# multiple regression
model1<- lm(weight ~ mpg + cylinders + displacement + modelyr+ hp + acceleration + foreign, data = car)

# Summarize the model
summary(model1)

# Extract the independent variables from your model
independent_vars1 <- model.matrix(~ mpg + cylinders + displacement + hp + acceleration + foreign, data = car)

# Calculate VIF for the independent variables
vif_values1 <- vif(independent_vars1)
print(vif_values1)

# Test 3

# multiple regression
model2 <- lm(weight ~ mpg + hp + acceleration + foreign, data = car)

# Summarize the model
summary(model2)

# Extract the independent variables from your model
independent_vars2 <- model.matrix(~ mpg + hp + acceleration + foreign, data = car)

# Calculate VIF for the independent variables
vif_values2 <- vif(independent_vars2)
print(vif_values2)


# Question 5

# Linearity: 
# Scatter plot of observed vs. predicted values
plot(model$fitted.values, residuals(model2), xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs. Fitted Values")
abline(0, 0, col = "red") # Add a reference line for linearity


# Normality in Errors:
# Q-Q plot of residuals
qqnorm(residuals(model2))
qqline(residuals(model2), col = "red")

# Homoskedasticity
# Scatter plot of residuals vs. predicted values
plot(model$fitted.values, abs(residuals(model2)), xlab = "Fitted Values", ylab = "Absolute Residuals", main = "Residuals vs. Fitted Values")
abline(h = 0, col = "red") # Add a reference line for homoskedasticity

# Unusual and Influential Observations
# Diagnostic plots
par(mfrow = c(2, 2)) # Set up a 2x2 grid of plots
plot(model2) # Plot diagnostic plots

# Question 6

# Extract the coefficient for the 'foreign' variable from the model summary
coefficient_foreign <- coef(summary(model2))["foreign", "Estimate"]
cat("Coefficient for 'foreign' variable:", coefficient_foreign, "\n")
p_value <- coef(summary(model2))["foreign", "Pr(>|t|)"]
p_value

# Check significance level 
alpha <- 0.01

# Determine statistical significance
if (abs(coefficient_foreign) > 0 && p_value < alpha) {
  if (coefficient_foreign < 0) {
    cat("American cars run less mileage per gallon than foreign cars.\n")
  } else {
    cat("American cars run more mileage per gallon than foreign cars.\n")
  }
  cat("The difference is statistically significant.\n")
} else {
  cat("There is no statistically significant difference in mileage per gallon between American and foreign cars.\n")
}


# Question 7
anova_result <- anova(model1, model)
p_value <- anova_result$"Pr(>F)"[2]
p_value

# Question 8

# R-squared
rsquared <- summary(model2)$r.squared

# Adjusted R-squared
adj_rsquared <- summary(model2)$adj.r.squared

# RMSE
rmse <- sqrt(mean((residuals(model2))^2))

# RSE
rse <- summary(model2)$sigma

# Print the metrics
print(paste("R-squared:", round(rsquared, 3)))
print(paste("Adjusted R-squared:", round(adj_rsquared, 3)))
print(paste("RMSE:", round(rmse, 3)))
print(paste("RSE:", round(rse, 3)))

