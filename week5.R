cldt = read.table('ClassData.csv', sep=',', header=T)

# Linear regression approach
regmod = lm(SexID~Height, data=cldt)
with(cldt,plot(Height,SexID))
abline(regmod)
predict(regmod, interval="confidence", se.fit=T, newdata=data.frame(Height=60))

# Logistic Regression approach
# NOTE: use SexID as response must be 0 (Male),1 (Female) and not text

#fit probit regression model
lprobit <- glm(SexID ~ Height, family = binomial(link = "probit"), data = cldt)
summary(lprobit)

#fit logit regression model
#Note the default link is logit-doesn't need to be named
llogit <- glm(SexID ~ Height, family = binomial, data = cldt)
summary(llogit)

#CI for slope estimates
#NOTE: these are profiled confidence intervals by default...
#...created by profiling the likelihood function and may not be symmetric
confint(lprobit)
confint(llogit)

#Alternative - fit based on asymptotic normality
confint.default(lprobit)
confint.default(llogit)

#Predicted probabilities for new observations
#Can use the SE to construct CI for observations
predict(lprobit, newdata=data.frame(Height=c(67,68)), type="response",
        se.fit=TRUE)#uses probit link
predict(llogit, newdata=data.frame(Height=c(67,68)), type="response",
        se.fit=TRUE)#uses logit link

#Fitted plot - probit link
plot(cldt$Height, cldt$SexID, pch = 16, xlab = "Height (inches)", ylab = "Probability Female")
curve(predict(lprobit,data.frame(Height=x),type="resp"),add=TRUE, col="blue")

#Fitted plot - logit link
plot(cldt$Height, cldt$SexID, pch = 16, xlab = "Height (inches)", ylab = "Probability Female")
curve(predict(llogit,data.frame(Height=x),type="resp"),add=TRUE, col="red")

#Fitted plot with both links in one graph
plot(cldt$Height, cldt$SexID, pch = 16, xlab = "Height (inches)", ylab = "Probability Female")
curve(predict(lprobit,data.frame(Height=x),type="resp"),add=TRUE, col="blue")
curve(predict(llogit,data.frame(Height=x),type="resp"),add=TRUE, col="red")

#Some diagnostic graphs - see online notes for forumulas
#Best results are no patterns or residual values > |2|
plot(residuals(lprobit, type="pearson"), type="b", main="Pearson Res - Probit")
plot(residuals(lprobit, type="deviance"), type="b", main="Deviance Res - Probit")
plot(residuals(llogit, type="pearson"), type="b", main="Pearson Res - Logit")
plot(residuals(llogit, type="deviance"), type="b", main="Deviance Res - Logit")

