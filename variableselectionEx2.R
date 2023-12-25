library(car)
data(Highway1)

#Backward Elimination with AIC
m1 <- lm(rate~.,data=Highway1)
step(m1)

#Forward Selection with AIC
Lmod1 <- lm(rate~1,data=Highway1)
Umod1 <- lm(rate~.,data=Highway1)
Fmod1 <- step(Lmod1, scope=list(lower=Lmod1, upper=Umod1)
              , direction="forward")
summary(Fmod1)

#Backward Stepwise with AIC
step(m1, data=Ericksen, direction="both")

#Backward Elimination with BIC
n <- nrow(Highway1)
step(m1,k=log(n))

step(m1, k=log(n),direction="both")

###Criterions-Based Procedures 
library(leaps)
subx=regsubsets(rate~., nbest=2, nvmax=ncol(Highway1) ,data=Highway1)
#subsets(subx,statistic="cp")
#abline(a=0,b=1)
#subsets(subx,statistic="cp",min.size=4, max.size=7)
#abline(a=0,b=1)

subsets(subx,statistic="bic")
subsets(subx,statistic="bic",min.size=2, max.size=5)

subsets(subx,statistic="adjr2")
subsets(subx,statistic="adjr2",min.size=3, max.size=7)

subsets(subx,statistic="rsq")
subsets(subx,statistic="rsq",min.size=8, max.size=12)

data(swiss)
View(swiss)
subx=regsubsets(Fertility~., nbest=2, data=swiss)
subsets(subx,statistic="bic")
subsets(subx,statistic="adjr2")
subsets(subx,statistic="rsq")