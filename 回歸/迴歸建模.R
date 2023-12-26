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