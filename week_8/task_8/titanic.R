raw_data<-read.csv("titanicTrain.csv")
raw_data<-raw_data[-c(1001:1310),]
quest<-read.csv("titanicQuestion.csv")
all<-rbind(raw_data,quest)

all$boat = as.numeric(all$boat != "")
agelm <- lm(age ~ pclass+sex+parch+sibsp+boat, data=all[!is.na(all$age),])
summary(agelm)
all$agelm<-predict(agelm,all)
hist(all$age[!is.na(all$age)], main='Original data, non-missing', xlab='age')
hist(all$agelm, main= 'LM predictions', xlab='age', xlim=range(0:80))
indexMissingAge <- which(is.na(all$age))
all$age[indexMissingAge] <- all$agelm[indexMissingAge]
#透過曲線擬和，將age的missing data消除

fare_NAindex = which(is.na(all$fare))
#發現持有3701號票根的客人在fare factor處有na
temp = all$fare[nchar( as.character(all$ticket)) == 4 & grepl("^3",as.character(all$ticket))]
average = sum(temp[!is.na(temp)])/(length(temp)-1)
all[fare_NAindex,"fare"] = average
#將類似票根號的票價取平均

#將資料拆分成四個區塊
female_insea = all[all$sex == "female" & all$boat == 0,c("age","fare","survived")]
female_onboat = all[all$sex == "female" & all$boat == 1,c("age","fare","survived")]
male_insea = all[all$sex == "male" & all$boat == 0,c("age","fare","survived")]
male_onboat = all[all$sex == "male" & all$boat == 1,c("age","fare","survived")]

female_insea[,3] = as.factor(female_insea[,3])
female_onboat[,3] = as.factor(female_onboat[,3])
male_insea[,3] = as.factor(male_insea[,3])
male_onboat[,3] = as.factor(male_onboat[,3])

#對fare取log10
female_insea[,2] = log10(female_insea[,2])
female_onboat[,2] = as.factor(female_onboat[,2])
male_insea[,2] = as.factor(male_insea[,2])
male_onboat[,2] = as.factor(male_onboat[,2])

#分別對每個區塊做SVM
library(e1071)
plot_svm <- function(dat){
  survived = as.numeric(dat[,3])
  train_index = !is.na(survived)
  svmfit = svm(survived[train_index] ~ ., data = dat[train_index,],gamma = 1, cost = 100)
  return(svmfit)
}
par(mfrow=c(2,2))

svm1 = plot_svm(female_insea)
#plot(temp,female_insea[!is.na(female_insea$survived),])
svm2 = plot_svm(female_onboat)
# plot(temp,female_onboat[!is.na(female_onboat$survived),])
svm3 = plot_svm(male_insea)
# plot(temp,male_insea[!is.na(male_insea$survived),])
svm4 = plot_svm(male_onboat)
# plot(temp,male_onboat[!is.na(male_onboat$survived),])

outcome1 = data.frame(predict(svm1,female_insea[is.na(female_insea$survived),c(1:2)]))
outcome2 = data.frame(predict(svm2,female_onboat[is.na(female_onboat$survived),c(1:2)]))
outcome3 = data.frame(predict(svm3,male_insea[is.na(male_insea$survived),c(1:2)]))
outcome4 = data.frame(predict(svm4,male_onboat[is.na(male_onboat$survived),c(1:2)]))

#合併結果進行輸出
names(outcome1) = "y"
names(outcome2) = "y"
names(outcome3) = "y"
names(outcome4) = "y"

ans = rbind(outcome1,outcome2,outcome3,outcome4)
index = order(row.names(ans))
ans = ans[index,]
write.csv(ans,file="titanicPrediction.csv")
