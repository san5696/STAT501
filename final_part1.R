part2 = read.table('Chol.csv', sep=',', header=T)#read in BodyFat data - download and save to working directory
mr = lm(Chol~Age+SBP+Height+Weight, data=part2)#full model
summary(mr)

predict(mr, interval="confidence", se.fit=T,
        newdata=data.frame(Age=45, SBP=135, Height = 68, Weight=185))
reeduced_model = lm(Chol~Age+SBP+Weight, data=part2)
anova(reeduced_model,mr)

confint(mr, 'Height', level=0.95)

#Normality check with QQ plot and Shapiro-Wilk Test
qqPlot(mr$residuals, main="QQ Plot;Saniya Naphade(spn5272)")#want all or majority points within bounds
shapiro.test(mr$residuals)#Shapiro-Wilk normality test

#Constant variance check with Res vs Fits plot and Breusch Pagan test
plot(mr$fitted,mr$residuals, ylab="Residuals", xlab="Fitted Values",main= 'Residuals VS Fits; Saniya Naphade(spn5272)')
abline(0,0)
bptest(mr)#Breusch-Pagan for constant variance


#Linearity check
rcorr(as.matrix(part2))

#Independence check
plot(mr$residuals, main="Residuals vs Order;Saniya Naphade(spn5272)")
abline(0,0)
acf(mr$residuals)
dwtest(mr)
