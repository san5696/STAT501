library(psych)
wt = read.table("ClassData.csv", sep=',', header=T)
summary(wt$Weight)
describeBy(wt$Weight, wt$Sex)

library(ggplot2)
ggplot(wt, aes(x=Height, y=Weight, color=Sex, shape=Sex)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  scale_color_manual(values=c('gray','green'))

model1 <- lm(Weight~Sex, data=wt)
summary(model1)

model2 <- lm(Weight~SexID, data=wt)
summary(model2)

#additive model

model3<- lm(Weight ~Height + Sex, data=wt)
summary(model3)

model4<- lm(Weight~ Height*Sex, data = wt)
summary(model4)

predict(catmod2, interval="confidence", newdata=data.frame(Height=c(66, 66), Sex=c("Male","Female")))

li = read.table("LifeInsurance.csv", sep=',', header=T)
library(car)
library(ggplot2)

model = lm(LifeInsurance ~., data=li)
qqPlot(model, main="QQ Plot") #qq plot for studentized resid


leveragePlots(model) # leverage plots identify possible influential outliers

# identify Di (Cook's Distance) values > 4/n
cutoff = 4/((nrow(li)))
plot(model, which=4, cook.levels=cutoff)
#alternative plot for Di
plot(cooks.distance(lm(LifeInsurance ~., data=li)))
abline(4/(nrow(li)),0)

#create new data set with the QQplot outliers removed
li2 = li[-c(3,7), ]
#Run regression on new data set and compare the two models
model2 = lm(LifeInsurance ~., data=li2)
summary(model)
summary(model2)

#plot regression lines from the full and reduced data sets
ggplot(li, aes(AvgAnnualIncome, RiskAversionScore, LifeInsurance)) +
  geom_point() +
  geom_smooth(method="lm", se=F, aes(color="With")) +
  geom_smooth(data = li[-c(3,7), ], method="lm", se=F, aes(color="Without"))+
  scale_colour_manual(name='',values=c("red","green"))

#check for multicollinearity among predictor variables
#general rull of thumb is that multicollinearity present when
#VIF values for a predictor greater than 10 - i.e. that predictor
#highly correlated with other predictors in model
vif(lm(LifeInsurance ~., data=li))