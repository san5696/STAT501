library(leaps)
HW_dataset = read.table('HW6.csv', sep=',', header=T)
#Best subset routine
bestsub_HW=regsubsets(Y~X1+X2+X3+X4+X5+X6+X7+X8,
                    data=HW_dataset, nbest=3)
plot(bestsub_HW, scale="adjr2")
plot(bestsub_HW, scale="Cp")
plot(bestsub_HW, scale="bic")
summary(bestsub_HW)

#Automatic selection - Forward
null=lm(Y~1, data=HW_dataset) #intercept only model
#full model- Regresses y on all variables in dataset
full=lm(Y~., data=HW_dataset)#full model- Regresses y on all variables in dataset
step(null, scope=list(lower=null, upper=full),
     direction="forward")

#Automatic selection - backward
step(full, data=HW_dataset, direction="backward")

#Automatic selection - Stepwise
step(null, scope = list(upper=full), data=HW_dataset, direction="both")
