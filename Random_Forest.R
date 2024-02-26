#Read in datasets==============================
train = read.csv("Train_MAS648.csv")
train = na.omit(train)
test = read.csv("Test_MAS648.csv")

train = na.omit(train)
train$Y = as.factor(train$Y)

# x as the splited Train set, y as the splited Test set
set.seed(3)
rows = sample(1:nrow(train), nrow(train)*0.2)
x = train[-rows,]
y = train[rows,]


# New-way using gbm=======
train_control = trainControl(method = "repeatedcv",
                              number = 10, repeats = 3)
model <- train(Y~., data = train, method = "gbm", trControl = train_control)

y = predict(model, newdata = test, type = "prob")
Y = y$`1`

# New-way using FR =======
rf.cvmodel <- train(Y~., data = train, method = "rf", trControl = train_control)

cv_opt = predict(rf.cvmodel, newdata = test, type = "prob")
Y = cv_opt$`1`


# Tune model by finding bestmtry==============================
bestmtry = tuneRF(train[,2:19], train[,1], stepFactor=1.5, improve=1e-5, ntree=500)
print(bestmtry)


#=============Create RF Model=====================================
set.seed(1)
rf.train=randomForest(Y~., data=x,mtry = 3,  importance=TRUE)

#=============Predict Test Y value below==========================
yhat.rf=predict(rf.train, newdata = y, type="prob")
yhat.rf=as.data.frame(yhat.rf)
yhat.rf=yhat.rf$`1`

Y = yhat.rf

# ======== looking for AUC score here =============
y$Y = as.numeric(y$Y)
roc.rf = roc(train$Y, Try$`1`)
auc(roc.rf)

#Submission================================
ID = seq(1:10000)

sample_submission = data.frame(ID,Y)

write.csv(sample_submission, "T_RF_OPT.csv", row.names = FALSE)