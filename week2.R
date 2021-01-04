library(car)
library(Hmisc)
library(rsq)
bf = read.table('BodyFat.csv', sep=',', header=T)#read in BodyFat data - download and save to working directorymr = lm(BodyFat~Triceps+Thigh+Midarm, data=bf)#full model
cor(bf)
mr = lm(BodyFat~Triceps+Thigh+Midarm, data=bf)#full model
summary(mr)
anova(mr)#Type 1 sum of squares)

red1 = lm(BodyFat ~ Triceps, data=bf)#reduced model with only one predictor
summary(red1)

red2 = lm(BodyFat ~ Thigh+Triceps, data=bf)#reduced model with two predictors
summary(red2)

rsq.partial(mr,red1)#partial R-square for adding adding Thigh and and Midarm to model already w/Triceps
rsq.partial(red2,red1)#partial R-square for adding Thigh to model already w/Triceps

anova(mr)
anova(red1)
anova(red2)

anova(red1,mr)

((143.12-98.4)/(18-16)) / (98.4/16) # General linear F-stat = 3.635772
pf(3.635772, 2, 16, lower.tail=F) # 0.04992962

Anova(mr, type=3)#Type 3 sum of squares NOTE: this begins with a capital "A" compared to prior anova with lower case.
