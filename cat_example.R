### Cat Example ###
# Study the relationship between a cat's body weight and heart weight.

# Load data and display basic information
library(MASS) #load library containing data
dim(cats)
help(cats) # heart and body weights of male and female cats
names(cats)
summary(cats)

# Explore relationship between body weight and heart weight
# Exploratory data analysis, linear regression, residuals, QQ plot

# par(mfrow = c(2,2)) #configure output window for 4 plots, 2x2

# EDA
boxplot(cats$Bwt, cats$Hwt, names = c("Body Weight (kg)", "Heart Weight (g)"),
        main = "Boxplot of Body and Heart Weights")

# Linear regression
regline <- lm(Hwt ~ Bwt, data = cats)
summary(regline)
plot(Hwt ~ Bwt, data = cats, xlab = "Body Weight (kg)", ylab = "Heart Weight (g)",
     main = "Scatterplot of Heart Weight v. Body Weight", pch = 19)
abline(regline, lwd = 2)

# Plot residuals
names(regline)
r <- residuals(regline)
plot(cats$Bwt, r, pch = 19, xlab = "Body Weight (kg)",
     ylab = "Residuals", main = "Plot of Body Weight v. Residuals")
abline(h = 0, col = "red", lwd = 2)

# QQ plot
r <- scale(r)
qqnorm(r)
qqline(r)

# Coefficients and standard confidence interval
coefficients(regline)
confint(regline)

