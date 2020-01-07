library(randomForest)
library(dplyr)
library(data.table)
rm(list=ls())

join_train_ver1 = fread("C:/Users/onycom/Desktop/igaworks/all/join_train_ver3.csv")
join_test_ver1 = fread("C:/Users/onycom/Desktop/igaworks/all/join_test_ver3.csv")

join_train_ver1 %>% head
join_test_ver1 %>% head
length(join_train_ver1)

join_train_ver1 = join_train_ver1[,c(2:29,1)]
join_train_ver1[join_train_ver1 == ""] = NA 
join_test_ver1[join_test_ver1 == ""] = NA 


join_train_ver1$event_datetime = substr(join_train_ver1$event_datetime, 12, 13)
join_test_ver1$event_datetime = substr(join_test_ver1$event_datetime, 12, 13)



join_train_ver1 = as.data.frame(join_train_ver1)
join_test_ver1 = as.data.frame(join_test_ver1)

join_train_ver1 = subset(join_train_ver1, select = -c(media_domain,device_os,device_country,device_language))
join_test_ver1 = subset(join_test_ver1, select = -c(media_domain,device_os,device_country,device_language))

# na 를 하나의 캐릭터형으로 변환
head(join_train_ver1)
join_train_ver1[is.na(join_train_ver1$age),]$age = "Not Available"
join_train_ver1[is.na(join_train_ver1$gender),]$gender = "Not Available"
join_train_ver1[is.na(join_train_ver1$marry),]$marry = "Not Available"
join_train_ver1 = join_train_ver1[!is.na(join_train_ver1$predicted_house_price),]

join_test_ver1[is.na(join_test_ver1$age),]$age = "Not Available"
join_test_ver1[is.na(join_test_ver1$gender),]$gender = "Not Available"
join_test_ver1[is.na(join_test_ver1$marry),]$marry = "Not Available"
join_test_ver1[is.na(join_test_ver1$predicted_house_price),]$predicted_house_price = "Not Available"


join_test_ver1$predicted_house_price = as.integer(join_test_ver1$predicted_house_price)
join_test_ver1$predicted_house_price[is.na(join_test_ver1$predicted_house_price)] = mean(join_test_ver1$predicted_house_price[!is.na(join_test_ver1$predicted_house_price)])

head(join_train_ver1)
head(join_test_ver1)

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


join_train_ver1 %>% head()
join_test_ver1 %>% head()


set.seed(123)


train_rf = join_train_ver1
test_rf = join_test_ver1
#house_price_mean = train_rf$predicted_house_price[!is.na(train_rf$predicted_house_price)] %>% mean()
#train_rf$predicted_house_price[is.na(train_rf$predicted_house_price)] = house_price_mean

colSums(is.na(train_rf))
head(train_rf)


clf_rf <- randomForest(train_rf[,feature.names], 
                       log(train_rf$click+1),
                       mtry=5,
                       ntree=1000,
                       sampsize=100000,
                       do.trace=TRUE)
# 8시 49qnsdp tlwkr


clf_rf

importance(clf_rf)
importance(clf_rf, type = 1)
importance(clf_rf, type = 2)
plot(clf_rf)

test_rf = join_test_ver1
#house_price_mean = test_rf$predicted_house_price[!is.na(test_rf$predicted_house_price)] %>% mean()
#test_rf$predicted_house_price[is.na(test_rf$predicted_house_price)] = house_price_mean

test_rf
test_rf = test_rf %>% as.data.frame()
class(test_rf)
#test_rf_imputed = rfImpute(bid_id ~ ., test_rf)


plot(importance(clf_rf), lty=2, pch=16)

submission <- data.frame(bid_id=test_rf$bid_id)
nrow(submission)
submission$click <-predict(clf_rf, data.matrix(test_rf[,feature.names]))

nrow(submission)
head(submission)


sub = fread("C:/Users/onycom/Desktop/igaworks/all/submission.csv",header=F)
colnames(sub)
colnames(sub) = c("bid_id","click")

sub = as.data.frame(sub)
head(sub)




### bid_id_label 참고
join_test_ver1_raw = fread("C:/Users/onycom/Desktop/igaworks/all/join_test_ver3.csv")

bid_label = data.frame(submission$bid_id, join_test_ver1_raw$bid_id)



sub_fin = left_join(sub, bid_label, by = c("bid_id" = "join_test_ver1_raw.bid_id"))
sub_fin = sub_fin[,-2]
sub_fin %>% head
colnames(sub_fin) = c("bid_id_raw","bid_id")





sub_fin_tot = left_join(sub_fin , submission, by = c("bid_id", "bid_id"))
sub_fin_tot %>% head




sub_fin_tot$click %>% min
sub_fin_tot$click %>% max
#sub_fin_tot$click[sub_fin_tot$click < 0] = 0


sub_fin_tot = sub_fin_tot[,-2]
sub_fin_tot %>% head()
sub_fin_tot$click %>% max()
sub_fin_tot9 = sub_fin_tot
sub_fin_tot9
quantile(sub_fin_tot9$click,probs = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99,0.999)) 

normalize <-function(x) {
  return ((x-min(x)) / (max(x) -min(x)))
}

sub_fin_tot9$click = normalize(sub_fin_tot9$click)
sub_fin_tot9$click %>% min()
sub_fin_tot9$click %>% max()


write.table(sub_fin_tot9, file="C:/Users/onycom/Desktop/igaworks/all/리더보드제출용/sub_fin_tot9_3.csv", sep=",", row.names=FALSE, col.names=FALSE, quote=FALSE)



# gpu 설치
# r
