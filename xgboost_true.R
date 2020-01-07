library(DataExplorer)
library(data.table)
library(dplyr)
library(xgboost)
## 테스트 데이터와 오디언스 프로파일 레프트 조인 필요.
#
#
#

join_train_ver1 = fread("C:/Users/onycom/Desktop/igaworks/all/join_train_ver3.csv")
join_test_ver1 = fread("C:/Users/onycom/Desktop/igaworks/all/join_test_ver3.csv")


join_train_ver1 = join_train_ver1[,c(2:29,1)]
join_train_ver1[join_train_ver1 == ""] = NA 
join_test_ver1[join_test_ver1 == ""] = NA 


join_train_ver1$event_datetime = substr(join_train_ver1$event_datetime, 12, 13)
join_test_ver1$event_datetime = substr(join_test_ver1$event_datetime, 12, 13)


class(join_train_ver1)
class(join_test_ver1)

table(is.na(join_train_ver1$age))
#join_train_ver1 = join_train_ver1[(!is.na(join_train_ver1$age)) ,]
table(is.na(join_train_ver1$age))


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
               nrounds     = 2500, #maximum number of iterations (steps) required for gradient descent to converge. (?)
               objective   = "reg:linear",
               eval_metric = "rmse") #회귀모델에서는 RMSE를 모델 accuracy 평가지표로 활용 

# 4시 19분 시작 ~ 4시 59분 종료 -> 40분
# 5시 10분 시작 ~ 

cat("making predictions\n")
submission <- data.frame(bid_id=join_test_ver1$bid_id)
submission$click <-predict(clf, data.matrix(join_test_ver1[,feature.names]))
head(submission)
tail(submission)
min(submission$click)


sub = fread("C:/Users/onycom/Desktop/igaworks/all/submission.csv",header=F)

colnames(sub) = c("bid_id","click")

sub = as.data.frame(sub)
head(sub)




### bid_id_label 참고
join_test_ver1_raw = fread("C:/Users/onycom/Desktop/igaworks/all/join_test_ver3.csv")

bid_label = data.frame(submission$bid_id, join_test_ver1_raw$bid_id)
bid_label %>% head
bid_label %>% tail


sub_fin = left_join(sub, bid_label, by = c("bid_id" = "join_test_ver1_raw.bid_id"))
sub_fin = sub_fin[,-2]
sub_fin %>% head
colnames(sub_fin) = c("bid_id_raw","bid_id")



sub_fin %>% str()
submission %>% str()

sub_fin %>% head()
submission %>% head
submission %>% tail


sub_fin_tot = left_join(sub_fin , submission, by = c("bid_id", "bid_id"))
sub_fin_tot %>% head

#sub_fin_tot_not0 = sub_fin_tot
#sub_fin_tot = sub_fin_tot_not0

sub_fin_tot$click[sub_fin_tot$click < 0] = 0





sub_fin_tot = sub_fin_tot[,-2]
sub_fin_tot %>% head()
sub_fin_tot$click %>% max()
sub_fin_tot8 = sub_fin_tot
sub_fin_tot8

normalize <-function(x) {
  return ((x-min(x)) / (max(x) -min(x)))
}

sub_fin_tot8$click = normalize(sub_fin_tot8$click)
sub_fin_tot8$click %>% min()
sub_fin_tot8$click %>% max()

quantile(sub_fin_tot8$click,probs = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99,0.999)) 

write.table(sub_fin_tot8, file="C:/Users/onycom/Desktop/DataAnalysis/ig-repo/sub_fin_tot8_2.csv", sep=",", row.names=FALSE, col.names=FALSE, quote=FALSE)



# //MS_DUZON\scan