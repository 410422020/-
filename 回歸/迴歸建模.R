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