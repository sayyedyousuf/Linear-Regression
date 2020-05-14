# Exmple of simple linear regression
# Demo uses the dataset "women" containing 
# height and weight for 15 women aged between 30 - 39
simple_linear_model <- lm(weight ~ height, data=women)

simple_linear_model
# Shows intercept and beta coefficient for height variable
# ie weight = -87.52 + 3.45 x height

plot(women$height,women$weight,
     xlab="Height (in inches)",
     ylab="Weight (in pounds)",
     main = "Scatter plot showing regression line
     for weight prdicted from height")
abline(simple_linear_model)

# Graph shows a linearly increasing relationshio between height and weight
#  Remember that one assumption of linear regression is that the relationship
# between response (weight) and predictor (height) variables is linear and additive

# Lets look at the data in more detail with the sumamry function
summary(simple_linear_model)

# Call. Shows the function call used to compute the regression model.
# Residuals - Provide a quick view of the distribution of the residuals, which by definition have a mean zero. 
# Therefore, median should not be far from zero, and the minimum and maximum 
# should be roughly equal in absolute value.
# Coefficients - shows the regression beta coefficients and their statistical significance. 
# Predictor variables that are significantly associated to the outcome variable, are marked by stars.
# Residual standard error (RSE), R-squared (R2) and the F-statistic are metrics that 
# are used to check how well the model fits to our data.

# the estimates of the beta coefficients
# the standard errors (SE), which defines the accuracy of beta coefficients. 
# For a given beta coefficient, the SE reflects how the coefficient varies under 
# repeated sampling. It can be used to compute the confidence intervals and the t-statistic.
# the t-statistic and the associated p-value, which defines the statistical significance of the beta coefficients.

# summary shows that the prediction equation for height i
# weight = -87.52 + 3.45 x height
# Since a height of 0 is imposible, we wouldn't give a physical
# representation to the intercept. It is just an adjustment constant

cor(women$height, women$weight)
# The correlation coefficient measures the level of the association between two variables 
# Its value ranges between -1 (perfect negative correlation: x increases, y decreases) 
# and +1 (perfect positive correlation: when x increases, y increases).
# A value closer to 0 suggests a weak relationship between the variables. A low 
# correlation (-0.2 < x < 0.2) probably suggests that much of variation of the outcome 
# variable (y) is not explained by the predictor (x). 
# In such case, we should probably look for better predictor variables.

# In our example, the correlation coefficient is large enough, so we can continue by building a linear model of y as a function of x.

# Examining the 95% confidence intervals of the model
confint(simple_linear_model)

# Lets examine the goodness of fit of the model
summary(simple_linear_model)

# Another example ----------------------------------------------------
# with cars dataset

library(car)
# Using the cars dataset
cars
head(cars)

# First step is to visualise any linear relationshios between the dependent
# response variables and the independent (predictor) variables
help(scatterplotMatrix)
# Scatter plots can help visualise any linear relationships between the 
# dependent (response) distance variable and independent (predictor) speed variables
#scatterplotMatrix(states_info, spread = FALSE, smoother.args = list(lty = 2), main = "Scatter Plot Matrix")

scatter.smooth(x = cars$speed, 
               y = cars$dist, 
               main = "Distance ~ Speed",
               xlab = "Car speed",
               ylab = "Stopping distance")

# The scatter plot along with the smoothing line above suggests a linearly increasing 
# relationship between the ‘dist’ and ‘speed’ variables which is good


par(mfrow = c(1, 2)) # divide graph area in 2 columns
boxplot(cars$speed, main = "Speed", sub = paste("Outlier rows: ", boxplot.stats(cars$speed)$out)) # box plot for 'speed'
boxplot(cars$dist, main = "Distance", sub = paste("Outlier rows: ", boxplot.stats(cars$dist)$out)) # box plot for 'distance'
# 1 outlier in distance

# Skewness function to examine normality of data
install.packages("e1071")
library(e1071)
# divide graph area in 2 columns
par(mfrow = c(1, 2))
# density plot for 'speed'
plot(density(cars$speed), main = "Density Plot: Speed", 
     ylab = "Frequency", 
     sub = paste("Skewness:", 
                 round(e1071::skewness(cars$speed), 2)))


# Lets fill in the area under the density plot in red
polygon(density(cars$speed), col = "red")
# Minimal ekewness = -0.11 - slightly skewed to the left
# NB - skewness <-1 or >1 = highly skewed
# -1 to -05 and 0.5 to 1 = moderately skewed
# -0.5 to 0-5 = approx symetric


# Density plot for stopping distance
plot(density(cars$dist), 
     main = "Density Plot: Distance", 
     ylab = "Frequency", 
     sub = paste("Skewness:", 
                 round(e1071::skewness(cars$dist), 2))) # density plot for 'dist'
# And also fill the area within the density plot to red
polygon(density(cars$dist), col = "red")
# Minimal skewness = 0.76 - skewed to the right

# calculate correlation between speed and distance
cor(cars$speed, cars$dist)

# build linear regression model on full data
linearMod <- lm(dist ~ speed, data = cars)
print(linearMod)

# model summary
summary(linearMod)

# demo of how to calculate t-statistic and p-values
# capture model summary as an object
model_summary <- summary(linearMod)

# model coefficients
model_coeffs <- model_summary$coefficients

# get beta estimate for speed
beta.estimate <- model_coeffs["speed", "Estimate"]

# get std.error for speed
std_error <- model_coeffs["speed", "Std. Error"]

# calc t statistic
t_value <- beta.estimate / std_error
p_value <- 2 * pt(-abs(t_value), df = nrow(cars) - ncol(cars)) # calc p Value
f_statistic <- linearMod$fstatistic[1] # fstatistic
f <- summary(linearMod)$fstatistic # parameters for model p-value calc
model_p <- pf(f[1], f[2], f[3], lower = FALSE)

# AIC => 419.1569
AIC(linearMod)

# BIC => 424.8929
BIC(linearMod)

# --------------------------------------------------------------------

# Create Training and Test data
# setting seed to reproduce results of random sampling
set.seed(200)

# sample chooses a random sample
# from 1:all records from cars, 80% of rows
no_of_records <- sample(1:nrow(cars), 0.8 * nrow(cars))
# model training data
training_data <- cars[no_of_records,]
training_data
# test data
testing_data <- cars[-no_of_records,]
testing_data

# Build the model on training data
# lm(formula, data) where
# formula describes the model to be fit
lr_model <- lm(dist ~ speed, data = training_data)

# model summary
summary(lr_model)

# predict distance from testing data
dist_predicted <- predict(lr_model, testing_data)

# make actuals_predicteds dataframe.
actuals_preds <- data.frame(cbind(actuals = testing_data$dist, 
                                  predicted = dist_predicted))
head(actuals_preds)

correlation_accuracy <- cor(actuals_preds)
correlation_accuracy

# Min - max accuracy
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
min_max_accuracy

# MAPE
mape <- mean(abs((actuals_preds$predicted - actuals_preds$actuals)) / actuals_preds$actuals)
mape

# k-fold cross-validation
install.packages("DAAG")
library(DAAG)
cvResults <- suppressWarnings(CVlm(data = cars, 
                                   form.lm = dist ~ speed, 
                                   m = 5, 
                                   dots = FALSE, 
                                   seed = 29, 
                                   legend.pos = "topleft", 
                                   printit = FALSE, 
                                   main = "Small symbols are predicted values while bigger ones are actuals."));

# Polynomial regression -----------------------------------------------
fit2 <- lm(weight ~ height + I(height ^ 2), data = women)
summary(fit2)
# The prediction equation now is 
# Weight = 261.88 - 7.35 × Height + 0.083 × Height2

# The curve provides a better fit
plot(women$height, women$weight, xlab = "Height (in inches)", ylab = "Weight (in lbs)")
lines(women$height, fitted(fit2))

# this enhanced plot provides the scatter plot of weight with height, 
# box plots for each variable in their respective margins, the linear 
# line of best fit, and a smoothed fit line. 
# spread=FALSE suppresses spread and asymmetry information. 
# smoother.args=list(lty=2) specifies the fit be rendered as a dashed line. 
# pch = 19 options display points as filled circles(the default is open circles) . 
# You can tell that the two variables are roughly symmetrical and that a curved line
# will fit the data points better than a straight line
install.packages("car")
library(car)
scatterplot(weight ~ height, data = women,
            spread = FALSE, smoother.args = list(lty = 2), pch = 19,
            main = "Women Age 30-39",
            xlab = "Height (inches)",
            ylab = "Weight (lbs.)")
