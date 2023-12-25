
#U.S. census example in John Fox Book
library(car)
data(Ericksen);View(Ericksen)
pairs(undercount~.,data=Ericksen)

#Backward Elimination with AIC
m1 <- step(lm(undercount~.,data=Ericksen))
summary(m1)
#Forward Selection with AIC
Lmod1 <- lm(undercount~1,data=Ericksen)
Umod1 <- lm(undercount~.,data=Ericksen)
Fmod1 <- step(Lmod1, scope=list(lower=Lmod1, upper=Umod1)
              , direction="forward")
summary(Fmod1)

#Backward Stepwise with AIC
step(Umod1, data=Ericksen, direction="both")

#Backward Elimination with BIC
n <- nrow(Ericksen)
step(lm(undercount~.,data=Ericksen),k=log(n))

step(Umod1, data=Ericksen, k=log(n),direction="both")

###Criterions-Based Procedures 
library(leaps)
subx=regsubsets(undercount~., nbest=2, data=Ericksen)
#subsets(subx,statistic="cp")
#abline(a=0,b=1)
#subsets(subx,statistic="cp",min.size=4, max.size=7)
#abline(a=0,b=1)

subsets(subx,statistic="bic")
subsets(subx,statistic="bic",min.size=3, max.size=5)

subsets(subx,statistic="adjr2")
subsets(subx,statistic="adjr2",min.size=4, max.size=8)

subsets(subx,statistic="rsq")
subsets(subx,statistic="rsq",min.size=4, max.size=8)


m1 <- lm(undercount~., data=Ericksen)
summary(m1)
m1a <- update(m1,.~.-highschool)
summary(m1a)
m1b <- update(m1a,.~.-housing)
summary(m1b)
m1c <- update(m1b,.~.-city)
summary(m1c)
m1d <- update(m1c,.~.-poverty)
summary(m1d)




data(swiss)
View(swiss)
subx=regsubsets(Fertility~., nbest=2, data=swiss)
subsets(subx,statistic="bic")
subsets(subx,statistic="adjr2")
subsets(subx,statistic="adjr2",min.size=3, max.size=5)
subsets(subx,statistic="rsq")


n <- nrow(swiss)
set.seed(123)
ind <- sample(n,37)
Train <- swiss[ind,]
Test <- swiss[-ind,]

m1 <- lm(Fertility~.,data=Train)
m2 <- update(m1,.~.-Examination)
summary(m2)

p1 <- predict(m1,newdata=Test)
r1 <- p1-Test$Fertility
RMSE1 <- sqrt(mean(r1^2))
p2 <- predict(m2,newdata=Test)
r2 <- p2-Test$Fertility
RMSE2 <- sqrt(mean(r2^2))

summary(m1)
summary(m2)

#An example for stepwise regression in Generalized Additive Models by Simon N. Wood
library(mgcv)
library(gamair)
data(sperm.comp1)
?sperm.comp1
#Scatter Plot Matrix
pairs(count~.,data=sperm.comp1)

#Backward Elimination using AIC and BIC
s1=step(lm(count~time.ipc+prop.partner,data=sperm.comp1))
s2=step(lm(count~time.ipc+prop.partner,data=sperm.comp1),k=log(15))

#Model selection based on the t-test
tmod1=lm(count~time.ipc+prop.partner,data=sperm.comp1)
summary(tmod1)
tmod2=lm(count~prop.partner,data=sperm.comp1)
summary(tmod2)