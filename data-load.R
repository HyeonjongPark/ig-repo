
#install.packages("DataExplorer")
library(data.table)
library(DataExplorer)
library(stringr)

setwd("C:/Users/onycom/Desktop/igaworks/all")

aud = fread("audience_profile.csv" , fill =T)
sub = fread("submission.csv")
test = fread("test.csv")
train = fread("train.csv")

train
nrow(aud)
head(aud)
colnames(aud)



## sample 100개
aud_sample_100 = aud[1:100,]
head(aud_sample_100)

df_100 = data.frame(device_ifa=NA, age=NA, gender=NA, marry=NA, install_pack=NA, cate_code=NA, predicted_house_price=NA, asset_index=NA)
df_100



str_split(aud_sample_100[1] , "!@#")[[1]][1]

for(i in 1:100) {
  split = str_split(aud_sample_100[i] , "!@#")
  for(j in 1:8){
    df_100[i,j] = split[[1]][j]
  }
}

df_100 = df_100[,-8]

head(df_100)

df_100[df_100 == ""] = NA 


df_100









## sample 1000개

aud_sample_1000 = aud_sample[1:1000,]
head(aud_sample_1000)
tail(aud_sample_1000)

df_1000 = data.frame(device_ifa=NA, age=NA, gender=NA, marry=NA, install_pack=NA, cate_code=NA, predicted_house_price=NA, asset_index=NA)
df_1000


for(i in 1:1000) {
  split = str_split(aud_sample_1000[i] , "!@#")
  for(j in 1:8){
    df_1000[i,j] = split[[1]][j]
  }
}
head(df_1000)
tail(df_1000)

df_1000[df_1000 == ""] = NA 

plot_missing(df_1000)




