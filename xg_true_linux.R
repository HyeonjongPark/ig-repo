library(DataExplorer)
library(data.table)
library(dplyr)
library(xgboost)

join_train_ver1 = fread("/root/data/join_train_ver3.csv")
join_test_ver1 = fread("/root/data/join_test_ver3.csv")


join_train_ver1 = join_train_ver1[,c(2:29,1)]
join_train_ver1[join_train_ver1 == ""] = NA 
join_test_ver1[join_test_ver1 == ""] = NA 


join_train_ver1$event_datetime = substr(join_train_ver1$event_datetime, 12, 13)
join_test_ver1$event_datetime = substr(join_test_ver1$event_datetime, 12, 13)

join_train_ver1 = as.data.frame(join_train_ver1)
join_test_ver1 = as.data.frame(join_test_ver1)


### xg boost  이용

feature.names <- names(join_train_ver1)[1:ncol(join_train_ver1)-1]
feature.names


cat("assuming text variables are categorical & replacing them with numeric ids\n")
for (f in feature.names) {
  if (class(join_train_ver1[[f]])=="character") {
    levels <- unique(c(join_train_ver1[[f]], join_test_ver1[[f]]))
    join_train_ver1[[f]] <- as.integer(factor(join_train_ver1[[f]], levels=levels))
    join_test_ver1[[f]]  <- as.integer(factor(join_test_ver1[[f]],  levels=levels))
  }
}


cat("training a XGBoost classifier\n")
clf <- xgboost(data        = data.matrix(join_train_ver1[,feature.names]), #matrix형을 input data로 활용
               label       = join_train_ver1$click,
               eta         = 0.025, #gradient descent 알고리즘에서의 learning rate
               depth       = 10,
               nrounds     = 1000, #maximum number of iterations (steps) required for gradient descent to converge. (?)
               objective   = "reg:linear",
               eval_metric = "rmse") #회귀모델에서는 RMSE를 모델 accuracy 평가지표로 활용 

# 4시 19분 시작 ~ 4시 59분 종료 -> 40분
# 5시 10분 시작 ~ 

cat("making predictions\n")
submission <- data.frame(bid_id=join_test_ver1$bid_id)
submission$click <-predict(clf, data.matrix(join_test_ver1[,feature.names]))

sub = fread("/root/data/submission.csv",header=F)
colnames(sub) = c("bid_id","click")

sub = as.data.frame(sub)

### bid_id_label 참고
join_test_ver1_raw = fread("/root/data/join_test_ver3.csv")

bid_label = data.frame(submission$bid_id, join_test_ver1_raw$bid_id)

sub_fin = left_join(sub, bid_label, by = c("bid_id" = "join_test_ver1_raw.bid_id"))
sub_fin = sub_fin[,-2]
colnames(sub_fin) = c("bid_id_raw","bid_id")

sub_fin_tot = left_join(sub_fin , submission, by = c("bid_id", "bid_id"))


sub_fin_tot = sub_fin_tot[,-2]
sub_fin_tot_lin = sub_fin_tot
sub_fin_tot_lin

normalize <-function(x) {
  return ((x-min(x)) / (max(x) -min(x)))
}

if(min(sub_fin_tot$click) < 0) {
  sub_fin_tot_lin$click = normalize(sub_fin_tot_lin$click)
}

quantile(sub_fin_tot_lin$click,probs = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99,0.999)) 

write.table(sub_fin_tot_lin, file="C:/Users/onycom/Desktop/DataAnalysis/ig-repo/sub_fin_tot_lin_1.csv", sep=",", row.names=FALSE, col.names=FALSE, quote=FALSE)


