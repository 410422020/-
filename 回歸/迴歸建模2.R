#Phase I: Identify the problem
#建立產品市場份額(Market Share)與其他影響因子間的迴歸關係,
#以了其他影響因子與產品市場份額(Market Share)間的關係,
#並做為後續預測產品市場份額之用,
#以提供廠商參考

#Phase II: MAKE ASSUMPTIONS AND DEFINE ESSENTIAL VARIABLES
#Four basic assumptions
#All the important factors are included in the study.
#Depenent Varialbe: MShare
#Independent Variable: Other variables in the dataset.

#Phase III: Do the Math

setwd('C:\\shawn\\迴歸分析資料夾\\最終上傳檔\\主題12 Modeling Overall Example 2 Market Share')
Market=read.table("MarketShare.txt",header=T)
Market$DiscP <- as.factor(Market$DiscP)
Market$PProm <- as.factor(Market$PProm)

stem(Market$MShare)
stem(Market$Price)
stem(Market$GNrate)
plot(Market$DiscP)
table(Market$DiscP)
plot(Market$PProm)
table(Market$PProm)

set.seed(123)
Sindex=sample(nrow(Market),30)
Train=Market[Sindex,]
Test=Market[-Sindex,]

pairs(MShare~.,data=Train)
#cor(Train[,c(2,3,4,5,6,8)])

M1=lm(MShare~.,data=Train)
summary(M1)