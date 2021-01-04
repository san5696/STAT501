bf = read.table('BodyFat.csv', sep=',', header=T)
mr = lm(BodyFat~Triceps+Thigh+Midarm, data=bf)
library(MASS)
library(lmtest)
library(nortest)
library(car)
library(Hmisc)

#95% CI for mean Body Fat level for invididual with listed measurements
predict(mr, interval="confidence", se.fit=T, newdata=data.frame(Triceps = 20, Thigh = 45,Midarm = 21))

#95 CI for the model slopes
confint(mr)

#Normality check with QQ plot and Anderson-Darling Test
qqPlot(mr$residuals, main="QQ Plot")
ad.test(mr$residuals)

#Constant variance check with Res vs Fits plot and Breusch Pagan test
plot(mr$fitted,mr$residuals, ylab="Residuals", xlab="Fitted Values")
abline(0,0)
bptest(mr)#Breusch-Pagan for constant variance

#Linearity check
rcorr(as.matrix(bf))

#Independence check
plot(mr$residuals, main="Residuals vs Order")
acf(mr$residuals)
dwtest(mr)