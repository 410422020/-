#Phase I: Identify the problem
#建立病患存活時間(Survival Time)與其他病患本身特徵間的迴歸關係,
#以了解病患本身特徵與存活時間(Survival Time)間的關係,
#以提供醫師及病人參考

#Phase II: MAKE ASSUMPTIONS AND DEFINE ESSENTIAL VARIABLES
#Four basic assumptions
#All the patients' features are measured correctly.
#Depenent Varialbe: Stime
#Independent Variable: Other variables in the dataset.

#Phase III : DO THE MATH
setwd("C:\\Users\\a0987\\OneDrive\\桌面\\迴歸分析")
STdata=read.table("SurvivalTime.txt",header=T)
STdata$AU=as.factor(STdata$AU)
STdata$Gender=as.factor(STdata$Gender) #把明顯為類別變數的變數轉為factor

#attach(STdata)
plot(STdata$AU)
table(STdata$AU)
#barplot(table(STdata$AU))
stem(STdata$BCS)
stem(STdata$PI)
stem(STdata$ET)
stem(STdata$LT)
stem(STdata$AGE) #檢測各變數中有沒有離群值
plot(STdata$Gender)
table(STdata$Gender)
#barplot(table(STdata$Gender))
stem(STdata$Stime)

#detach(STdata)12
set.seed(234)
Sindex=sample(nrow(STdata),70) #隨機抽取70筆要保留的部分資料
#Sindex=sample(N, round(N*0.8)) #抽取80%作為要保留的部分資料
Train=STdata[Sindex,]
Test=STdata[-Sindex,] #把資料分為訓練資料跟測試資料

#Phase IV : Diagnostic
pairs(Stime~.,data=Train) #用散布圖檢查變數間的關係
cor(Train[,c(2,3,4,5,6,8)]) #correlation 越高可能代表貢共線性越明顯

M1=lm(Stime~.,data=Train)
summary(M1)

library(car)
library(lmtest)
library(nortest)
library(randtests)
###Function Form and Homogeneity
#e=residuals(M1)
es=rstandard(M1) #求出殘差
yhat=fitted.values(M1)

plot(yhat,es,col='2')
#plot(yhat,es,col='2')
abline(h=0) #e-yhat plot
residualPlot(M1,type="rstandard",quadratic=F) 
#畫出殘差圖，看資料的中心是否平穩在0的虛線上(function form);
#以及資料變異的一致性，看點是否均勻(齊一性)

resettest(M1,power=2,type='regressor') 
#檢驗屬量變數的二次項是否同時為零
#p value 小於0.05，拒絕了!代表此模型有潛在的二次趨勢

ncvTest(M1)
#Breusch-Pagan test
#檢定齊一性的，看sigma i 是否不論i為和皆相同
#拒絕了!代表sigma i之間存在顯著差異

#Normality
qqPlot(M1)
lillie.test(es)
#KS test for normality
#H0 : Yi服從常態 H1: Yi不服從常態
shapiro.test(es)
#Shapiro-Wilk Normality Test
#H0 : Yi服從常態 H1: Yi不服從常態

plot(es,type = "l",col='2')
acf(es, ci=0.99)
#dwtest(M1)#Durbin-Watson test

runs.test(es)
# H0: 殘差是隨機的  H1: 殘差不是隨機的

#結論: 四大基本假設除了隨機性，其餘的齊一性、常態性、和函數形式皆有問題

#Phase V : 矯正和做變數選擇，Refined and extend the model
#Step 1:矯正
library(MASS)
boxcox(M1) #取靠近極值，更好解釋的那個

#Stime1=log(Stime)，選擇取log
M2=lm(log(Stime)~.,data=Train)
summary(M2) #新模型

#再做一次第四步的診斷
#e2=residuals(M2)
e2s=rstandard(M2)
yhat2=fitted.values(M2)
plot(yhat2,e2s)
abline(h=0,col=2)
residualPlot(M2,type="rstandard",quadratic=F)

resettest(M2,power=2,type='regressor')
ncvTest(M2)#This test is often called the Breusch-Pagan test; 

qqPlot(M2)
lillie.test(e2s)#KS test for normality
shapiro.test(e2s)#Shapiro-Wilk Normality Test

plot(e2s,type = "l",col='2')
acf(e2s,ci=0.99)
#dwtest(M2)#Durbin-Watson test
runs.test(es)

#Step 2 :做變數的選擇，用逐步回歸或是criteria
#criteria
library(leaps)
subx=regsubsets(log(Stime)~., nbest=3, data=Train)

subsets(subx,statistic="bic")
subsets(subx,statistic="bic",min.size=3, max.size=6)

subsets(subx,statistic="adjr2", legend=F)
subsets(subx,statistic="adjr2",min.size=4, max.size=6)

#subsets(subx,statistic="cp")
#abline(a=0,b=1)
#subsets(subx,statistic="cp",min.size=8, max.size=9)
#abline(a=0,b=1)

#逐步回歸
s1=step(M2) #預設是用AIC
s2=step(M2,k=log(dim(Train)[1])) #改成用BIC


M2a=lm(log(Stime)~AU+BCS+PI+ET+LT,data=Train)
summary(M2a)
M2b=lm(log(Stime)~AU+BCS+PI+ET,data=Train)
summary(M2b)
M2br = lm(log(Stime)~BCS+PI+ET,data=Train) #拿掉不顯著(可能可以拿掉)的AU
anova(M2b, M2br) 
#去做general linear test(拿掉一個變數的reduced model 和原本的full model比)
#此處是在檢驗AU的dummy variable是否同時為零

#M2b or M2a can be our final model

#Outliers might existed
vif(M1)
vif(M2a)
vif(M2b) #檢測共線性是否太過嚴重，看該模型vif有沒有超過10