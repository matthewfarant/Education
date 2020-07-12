library(ggplot2)
library(pROC)
#load data
train_titanic<-read.csv('train.csv')
test_titanic<-read.csv('test.csv')
View(train_titanic)
View(test_titanic)
str(train_titanic)

#check missing values
table(is.na(test_titanic$survive_prob))

#structure changes (train_titanic)
train_titanic$Survived<-as.factor(train_titanic$Survived)
train_titanic$Pclass<-as.factor(train_titanic$Pclass)
train_titanic$Sex<-as.factor(train_titanic$Sex)
#train_titanic: check missing values
table(is.na(train_titanic$Survived))
table(is.na(train_titanic$Pclass))
table(is.na(train_titanic$Sex))
table(is.na(train_titanic$Age))
table(is.na(train_titanic$SibSp))
train_titanic$Age_imp<-ifelse(is.na(train_titanic$Age),28,train_titanic$Age)

#test data structure check & missing values
str(test_titanic)
test_titanic$Pclass<-as.factor(test_titanic$Pclass)
test_titanic$Sex<-as.factor(test_titanic$Sex)
table(is.na(test_titanic$Pclass))
table(is.na(test_titanic$Age))
table(is.na(test_titanic$Sex))
table(is.na(test_titanic$SibSp))
summary(test_titanic$Age)
hist(test_titanic$Age)
test_titanic$Age_imp<-ifelse(is.na(test_titanic$Age),27,test_titanic$Age)
table(is.na(test_titanic$Age_imp))

#model
m<-glm(Survived~Pclass+Sex+Age_imp+SibSp,data=train_titanic,family='binomial')
summary(m)
#prediction
test_titanic$survive_prob<-predict(m,test_titanic,type='response')
test_titanic$Survived<-ifelse(test_titanic$survive_prob>=0.65,1,0)

#ROC Curve
train_titanic$survive_prob<-predict(m,type='response')
ROC<-roc(train_titanic$Survived,train_titanic$survive_prob)
plot(ROC,col='red')
auc(ROC)

#Confusion Matrix
train_titanic$Survived_pred<-ifelse(train_titanic$survive_prob>=0.65,1,0)
table(train_titanic$Survived,train_titanic$Survived_pred)

#Submission to Kaggle
survived_subs<-subset(test_titanic,select=c(PassengerId,Survived))
survived_subs
rownames(survived_subs)<-survived_subs$PassengerId
survived_subs<-subset(survived_subs,select=Survived)
write.csv(survived_subs,'survived_subs5.csv')

ggplot(train_titanic,aes(PassengerId,survive_prob,col=Survived))+
  geom_point()
