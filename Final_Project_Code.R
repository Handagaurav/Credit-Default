cred<- read.csv("C:/Users/Gaurav/Downloads/default of credit card clients (1).csv",header = T,skip=1)
View(cred)

summary(cred)
cred<- cred[-1,]
str(cred)
cred$SEX<- factor(cred$SEX)
cred$MARRIAGE<- factor(cred$MARRIAGE)
cred$PAY_0<- factor(cred$PAY_0)
cred$PAY_2<- factor(cred$PAY_2)
cred$PAY_3<- factor(cred$PAY_3)
cred$PAY_4<- factor(cred$PAY_4)
cred$PAY_5<- factor(cred$PAY_5)
cred$PAY_6<- factor(cred$PAY_6)
cred$EDUCATION<- factor(cred$EDUCATION)
cred$default.payment.next.month<- factor(cred$default.payment.next.month)
nv.cred<- cred
nv.cred$AGE<- factor(round(nv.cred$AGE/100))
nv.cred$LIMIT_BAL<- factor(round(nv.cred$LIMIT_BAL/100))
nv.cred$BILL_AMT1<- factor(round(nv.cred$BILL_AMT1/100))
nv.cred$BILL_AMT2<- factor(round(nv.cred$BILL_AMT2/100))
nv.cred$BILL_AMT3<- factor(round(nv.cred$BILL_AMT3/100))
nv.cred$BILL_AMT4<- factor(round(nv.cred$BILL_AMT4/100))
nv.cred$BILL_AMT5<- factor(round(nv.cred$BILL_AMT5/100))
nv.cred$BILL_AMT6<- factor(round(nv.cred$BILL_AMT6/100))
nv.cred$PAY_AMT1<- factor(round(nv.cred$PAY_AMT1/100))
nv.cred$PAY_AMT2<- factor(round(nv.cred$PAY_AMT2/100))
nv.cred$PAY_AMT3<- factor(round(nv.cred$PAY_AMT3/100))
nv.cred$PAY_AMT4<- factor(round(nv.cred$PAY_AMT4/100))
nv.cred$PAY_AMT5<- factor(round(nv.cred$PAY_AMT5/100))
nv.cred$PAY_AMT6<- factor(round(nv.cred$PAY_AMT6/100))
View(nv.cred)
nv.cred<- nv.cred[,-1]
str(nv.cred)
library(e1071)
set.seed(10)
nv.train.index <- sample(c(1:dim(nv.cred)[1]), dim(nv.cred)[1]*0.7)  
nv.train<- nv.cred[nv.train.index, ]
nv.valid <- nv.cred[-nv.train.index, ]
naive.cred<- naiveBayes(default.payment.next.month ~ .,data= nv.train)
View(nv.train)
nvpred.prob <- predict(naive.cred, newdata = nv.valid, type = "raw")

nvpred.class <- predict(naive.cred, newdata = nv.valid)
View(nvpred.prob)
nvdf <- data.frame(actual = nv.valid$default.payment.next.month, predicted = nvpred.class, nvpred.prob)
View(nvdf)

confusionMatrix(nvpred.class, nv.valid$default.payment.next.month)
library(gains)
nvgain <- gains(as.integer(as.vector(nvdf$actual)),nvdf[,4],groups=10)
plot(c(0,as.integer(nvgain$cume.pct.of.total*sum(as.integer(nvdf$actual))))~c(0,nvgain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="Lift Chart Naive Bayes", type="l")
lines(c(0,sum(as.integer(nvdf$actual)))~c(0, dim(nvdf)[1]), lty=2)

logit.cred<- cred
set.seed(45)
logtrindex<- sample(c(1:dim(logit.cred)[1]), dim(logit.cred)[1]*0.7)  
log.train<- logit.cred[logtrindex, ]
log.valid <- logit.cred[-logtrindex, ]
log.train<- log.train[,-1]
log.valid<- log.valid[,-1]
logit.reg <- glm(default.payment.next.month ~ ., data = log.train, family = "binomial") 
options(scipen=999)
logit.reg.pred <- predict(logit.reg, log.valid[, -24], type = "response")
loggain <- gains(as.integer(log.valid$default.payment.next.month), logit.reg.pred, groups=10)
plot(c(0,loggain$cume.pct.of.total*sum(as.integer(log.valid$default.payment.next.month)))~c(0,loggain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="Logistic regression lift chart", type="l")
lines(c(0,sum(as.integer(log.valid$default.payment.next.month)))~c(0, dim(log.valid)[1]), lty=2)
logdf<- data.frame(logit.reg.pred,log.valid$default.payment.next.month)
View(logdf)
logclass<-ifelse(logit.reg.pred<0.5,0,1)
logclass<-as.factor(logclass)
confusionMatrix(logclass,log.valid$default.payment.next.month)

library(MASS)
lda.cred<- lda(default.payment.next.month~., log.train)
lda.pred<- predict(lda.cred, log.valid[,-24])
ldagain <- gains(as.integer(log.valid$default.payment.next.month), lda.pred$posterior[,2], groups=10)
plot(c(0,ldagain$cume.pct.of.total*sum(as.integer(log.valid$default.payment.next.month)))~c(0,ldagain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="Linear Discriminant lift chart", type="l")
lines(c(0,sum(as.integer(log.valid$default.payment.next.month)))~c(0, dim(log.valid)[1]), lty=2)
confusionMatrix(lda.pred$class,log.valid$default.payment.next.month)

library(randomForest)
cred.rf<- randomForest(default.payment.next.month~., log.train, ntree = 500, mtry = 4, nodesize = 5, importance = TRUE)
#
cred.rf.pred <- predict(cred.rf, log.valid[,-24])
View(cred.rf.pred)
confusionMatrix(cred.rf.pred,log.valid$default.payment.next.month)
cred.rf.pred1 <- predict(cred.rf, log.valid[,-24],type = "prob")
rfgain <- gains(as.integer(log.valid$default.payment.next.month), cred.rf.pred1[,2], groups=10)
plot(c(0,rfgain$cume.pct.of.total*sum(as.integer(log.valid$default.payment.next.month)))~c(0,rfgain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="Random Forest Lift Chart", type="l")
lines(c(0,sum(as.integer(log.valid$default.payment.next.month)))~c(0, dim(log.valid)[1]), lty=2)



svm.cred<- svm(default.payment.next.month~.,data = log.train)
svm.pred<- predict(svm.cred,log.valid[,-24])
svm.pred.prob<- predict(svm.cred,log.valid[,-24],decision.values = T, probability = TRUE)
confusionMatrix(svm.pred,log.valid$default.payment.next.month)



cred1<- read.csv("C:/Users/Gaurav/Downloads/default of credit card clients (1).csv",header = T,skip=1)
knn.cred<- cred1
knn.cred <- knn.cred[,-1]
mynorm <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}
knn.cred<-as.data.frame(lapply(knn.cred[,1:23], mynorm))
knn.cred$default<-cred1$default.payment.next.month
ktrain.index <- sample(c(1:dim(knn.cred)[1]), 0.7*dim(knn.cred)[1])
k.train<- knn.cred[ktrain.index,] 
k.valid<- knn.cred[-ktrain.index,]

knn.cred.reg <- knnreg(default~., k.train, k=20)
knn.cred.pred2 <- predict(knn.cred.reg, k.valid)                
knn.class<- ifelse(knn.cred.pred2<0.5,0,1)
kgain <- gains(as.integer(k.valid$default),knn.cred.pred2, groups=10)
kgain
plot(c(0,kgain$cume.pct.of.total*sum(as.integer(k.valid$default)))~c(0,kgain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="KNN Lift Chart", type="l")
lines(c(0,sum(as.integer(k.valid$default)))~c(0, dim(k.valid)[1]), lty=2)
confusionMatrix(as.factor(knn.class),as.factor(k.valid$default))
View(k.valid)

library(neuralnet)
n= names(k.train)

f <- as.formula(paste("default~", paste(n[!n %in% "default"], collapse = "+")))
n
f


cred.nn <- neuralnet(f, data = k.train, hidden = 5)
View(cred.nn)
cred.nn$data
cred.nn.pred <- compute(cred.nn,k.valid[,1:23]) 
cred.nn.pred.class <- ifelse(cred.nn.pred$net.result >=0.5, 1, 0)
confusionMatrix(as.factor(cred.nn.pred.class),as.factor(k.valid$default))
nngain <- gains(as.integer(k.valid$default),cred.nn.pred$net.result, groups=10)

plot(c(0,nngain$cume.pct.of.total*sum(as.integer(k.valid$default)))~c(0,nngain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="Neural Net Lift Chart", type="l")
lines(c(0,sum(as.integer(k.valid$default)))~c(0, dim(k.valid)[1]), lty=2)
