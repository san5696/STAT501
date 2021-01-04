class_analysis = read.table('HW1.csv', sep=',', header=T) # object "class_analysis" for the HW1 data
class_analysis = read.table('coursera1.csv', sep=',', header=T) # object "class_analysis" for the HW1 data
model = lm(Y~X, data=class_analysis)
 summary(model)#Inspect for outliers in data
boxplot(class_analysis[,-2],xlab="Readiness", col="green")
boxplot(class_analysis[,-1],xlab="Achievement", col="yellow")

#Predicting the Linear Regression model
model = lm(Achievement~Readiness, data=class_analysis)
summary(model)

#Plotting the model and the best fitted line
plot(Achievement~Readiness,data=class_analysis, main= 'Saniya Naphade')
abline(model, col="blue")
print(model)

summary(model)

confint(model, level=0.95)

anova(model)

library(alr3) # alr3 package must be installed first
pureErrorAnova(model) 

#problem3
plot(x=class_analysis$Readiness, y=residuals(model),
     xlab="Readiness", ylab="Residuals", main= 'Residuals VS Predictor; Saniya Naphade(spn5272)',
     panel.last = abline(h=0, col="blue"))

plot(x=fitted(model), y=residuals(model),
     xlab="Fitted values", ylab="Residuals",main= 'Residuals VS Fits; Saniya Naphade(spn5272)',
     panel.last = abline(h=0, col="blue"))

cor(class_analysis$Readiness,class_analysis$Achievement)

cor.test(class_analysis$Readiness,class_analysis$Achievement)

hist(model$residuals, main="Histogram of Residuals;Saniya Naphade(spn5272)")#data name precedes $
qqPlot(model, main="QQ Plot;Saniya Naphade(spn5272)")#want all or majority points within bounds

shapiro.test(model$residuals)#Shapiro-Wilk normality test

bptest(model)#Breusch-Pagan for constant variance

plot(model$residuals, main="Residuals vs Order; Saniya Naphade(spn5272)",
     panel.last = abline(h=0, col="blue"))
acf(model$residuals, main="ACF; Saniya Naphade(spn5272)")
dwtest(model)

predict(model, interval="confidence", se.fit=T, newdata=data.frame(Readiness=3))#95% CI for mean response of 400

predict(model, interval="prediction", se.fit=F, newdata=data.frame(Readiness=3))#95% PI for invididual response of 400

predict(model, interval="confidence", se.fit=T, newdata=data.frame(Readiness=c(4,5)))

index <- c("1", "2", "3", "4", "5", "6","7", "8", "9", "10", "11", "12", "13",
             "14","15","16","17","18","19","20")

# Create data frame for sample data
data <- data.frame(Gene_id, class_analysis$Readiness, class_analysis$Achievement)

# Calculate residuals
regression <- lm(class_analysis$Achievement~class_analysis$Readiness)
plot(Achievement~Readiness,data=class_analysis, main= 'Saniya Naphade')
abline(regression)

# Show regression formula
print(regression)
data$residuals <- residuals(regression)

# Choose a threshhold
outlier_threshold <- 0.8442

# Print only names of outliers
outliers <- data[ abs(data$residuals) > outlier_threshold, ]
print(outliers$Gene_id)


Height <- c(72,73,74,76,76,79,79,80,81,82,82,83,83)
Weight <- c(175,195,180,200,200,205,235,220,240,235,245,230,225)
model <- lm(Weight~Height)
confint(model, level=0.95)

     
     