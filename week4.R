install.packages("leaps")
library(leaps)
bodyfat = read.table('BodyFat.csv', sep=',', header=T)
#Best subset routine
bestsub1=regsubsets(BodyFat~Triceps+Thigh+Midarm,
                    data=bodyfat, nbest=10)
plot(bestsub1, scale="adjr2")
plot(bestsub1, scale="Cp")
plot(bestsub1, scale="bic")
summary(bestsub1)

#Automatic selection - Forward
null=lm(BodyFat~1, data=bodyfat) #intercept only model
#full model- Regresses y on all variables in dataset
full=lm(BodyFat~., data=bodyfat)#full model- Regresses y on all variables in dataset
step(null, scope=list(lower=null, upper=full),
     direction="forward")

#Automatic selection - backward
step(full, data=bodyfat, direction="backward")

#Automatic selection - Stepwise
step(null, scope = list(upper=full), data=bodyfat, direction="both")


