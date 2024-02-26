train = read.csv("Train_MAS648.csv")
summary(train)
library(ISLR)
library(glmnet)
library(MASS)
library(leaps)
library(gbm)
library(caret)
library(class)
library(faraway)
library(e1071)
library(pROC)
library(randomForest)

test = read.csv("Test_MAS648.csv")
train = na.omit(train)

summary(train)

#seperate train and test sets
set.seed(3)
test_rows = sample(1:nrow(train), nrow(train)*0.1)
train_te = train[test_rows,]
train_tr = train[-test_rows,]
#=======glm===
model = glm(Y~., family = binomial,data = train)

#predict self
probs.tr=predict(model,newdata = train, type = "response")
roc.glm = roc(train$Y, probs.tr)
auc(roc.glm)

#predict test set's
Y = predict(object = gbm.model,newdata = test,n.trees = optimal_cv, type = "response")

#predict test set
probs.te=predict(model,newdata = test, type = "response")
mean(pred.te == train_te$Y)
Y = probs.tr

#===========byes==
train_tr$Y = as.factor(train_tr$Y)
x = train_tr[,-1]
y = train_tr$Y
model_b = train(x,y,'nb',trControl=trainControl(method='cv',number=10))



model_nb = naiveBayes(Y~., data = train, laplace = 1)

probs.te=predict(model_nb,newdata = train_te, type = "response")
mean(probs.te == train_te$Y)

#predict ture test data
Y=predict(model_b,newdata = test)


#====log====
train$Y = as.factor(train$Y)
model = glm(Y~., family = binomial,data = train)


lev <- hatvalues(model)
halfnorm(lev,labs=labels,ylab="Leverages",xlim=c(0,4))





probs.train=predict(model,train, type = "response")
pred.train = ifelse(probs.train>0.5, 1, 0)
mean(pred.train == train$Y)

#====knn==
ran <- sample(1:nrow(iris), 0.9 * nrow(iris))
model = knn(train[ran,], train[-ran,],k = 13, cl=train[ran,1],use.all = TRUE)

tab = table(model, train[-ran,1])

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)

#=======gbm(0.63242)============
gbm.model = gbm(formula = Y~.,distribution = "bernoulli", data = train, n.trees = 10000, shrinkage = 0.01, interaction.depth = 4, cv.folds = 3)
summary(gbm.model)

#=====result of gbm=====
optimal_cv = gbm.perf(gbm.model, method = "cv")

#predict train set's and get auc result
resultpre = predict(object = gbm.model,newdata = train[,-1],n.trees = optimal_cv, type = "response")
roc.bt = roc(train$Y, resultpre)
auc(roc.bt)

#predict test set's
Y = predict(object = gbm.model,newdata = test,n.trees = optimal_cv, type = "response")

#===========lda(0.6334)============

lda.fit=lda(Y~X6+X14+X2+X1+X16+X7,data=train)
lda.pred = predict(lda.fit, train)
lda.pred_test = predict(lda.fit, test)


mean(lda.pred$class ==train$Y)

#====================glm===========
model = glm(Y~X6+X14+X2+X1+X16+X7+X3+X11+X13, family = binomial,data = train)

probs.train=predict(model,train, type = "response")
pred.train = ifelse(probs.train>0.5, 1, 0)
mean(pred.train == train$Y)

#===================stepwise========


regfit.full=regsubsets(Y~., train,nvmax=19)
summary(regfit.full)
reg.summary=summary(regfit.full)
names(reg.summary)

par(mfrow=c(2,2))
plot(reg.summary$rss ,xlab="Number of Variables ",ylab="RSS")
plot(reg.summary$adjr2 ,xlab="Number of Variables ", ylab="Adjusted RSq")

plot(reg.summary$cp, xlab="Number of Variables", ylab="Cp")
which.min(reg.summary$cp)

plot(reg.summary$bic, xlab="Number of Variables", ylab="BIC")
which.min(reg.summary$bic)

coef(regfit.full ,13)
regfit.full = regsubsets(Y~., train,nvmax=16)
summary(regfit.full)

model_select = glm(Y~.-X4-X8-X9-X13, family = binomial,data = train)
#summary(model_select)



probs.y = predict(model_select, test, type = "response")
Y = ifelse(probs.y>0.5, 1, 0)


#===================regide==========

x=model.matrix(Y~.,train)[,-1]
y=train$Y

grid=10^seq(10,-2,length=100)

ridge.mod=glmnet(x,y,alpha=0,lambda=grid,thresh=1e-12)

summary(ridge.mod)

set.seed(0)
cv.out=cv.glmnet(x,y,alpha=0)
plot(cv.out)

bestlam=cv.out$lambda.min
bestlam

ridge.pred=predict(ridge.mod,s=bestlam ,newx=x)
mean((ridge.pred-y)^2)


pred.train = ifelse(ridge.pred > 0.5, 1, 0)
mean(pred.train == train$Y)

te = as.matrix(test)

ridge.pred=predict(ridge.mod,s=bestlam ,newx=as.matrix(test))

Y1 = ifelse(ridge.pred > 0.5, 1, 0)





#====================lasso=======
ridge_lasso=glmnet(x,y,alpha=1,lambda=grid)

set.seed(0)
cv.out=cv.glmnet(x,y,alpha=1)
plot(cv.out)

bestlam_la=cv.out$lambda.min
bestlam_la

#predict train and get auc
ridge.pred=predict(ridge_lasso,s=bestlam_la ,newx=x)
roc.lasso = roc(train$Y, ridge.pred)
auc(roc.lasso)



ridge.pred=predict(ridge_lasso,s=bestlam ,newx=as.matrix(test))
Y = ifelse(ridge.pred > 0.5, 1, 0)
#===================submission

ID = seq(1:10000)

sample_submission = data.frame(ID,Y)

write.csv(sample_submission, "T_BOOST.csv", row.names = FALSE)

