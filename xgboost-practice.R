getwd()
setwd("C:/Users/onycom/Desktop/DataAnalysis/ig-repo")
#install.packages("readr")
#install.packages("xgboost")

library(readr)
library(xgboost)

# Set a random seed for reproducibility
set.seed(1)


cat("reading the train and test data\n")
train <- read_csv("train.csv")
test  <- read_csv("test.csv")
head(train)
tail(train)
dim(train)

feature.names <- names(train)[2:ncol(train)-1]

#xgboost 알고리즘은 결측치를 학습과정에서 자동으로 처리
#https://github.com/dmlc/xgboost/issues/21


cat("assuming text variables are categorical & replacing them with numeric ids\n")
for (f in feature.names) {
  if (class(train[[f]])=="character") {
    levels <- unique(c(train[[f]], test[[f]]))
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
    test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
  }
}

cat("training a XGBoost classifier\n")
clf <- xgboost(data        = data.matrix(train[,feature.names]), #matrix형을 input data로 활용
               label       = train$Response,
               eta         = 0.025, #gradient descent 알고리즘에서의 learning rate
               depth       = 10,
               nrounds     = 2500, #maximum number of iterations (steps) required for gradient descent to converge. (?)
               objective   = "reg:linear",
               eval_metric = "rmse") #회귀모델에서는 RMSE를 모델 accuracy 평가지표로 활용 

cat("making predictions\n")
submission <- data.frame(Id=test$Id)
submission$Response <- as.integer(round(predict(clf, data.matrix(test[,feature.names]))))

# 결과값 범위 조정
submission[submission$Response<1, "Response"] <- 1
submission[submission$Response>8, "Response"] <- 8

cat("saving the submission file\n")
write_csv(submission, "xgboost_submission.csv")


