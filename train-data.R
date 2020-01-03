#install.packages("dplyr")
library(dplyr)
train
sub
test

train
unique(train$click)

# train 필요 변수 추출.
# click , event_datetime, placement_type, device_ifa, device_language, device_country, device_region

train_handle = train %>% 
  select(click, event_datetime, placement_type, device_ifa, device_language, device_country, device_region)

train_handle
str(train_handle)
dim(train_handle)

train_handle_sp = train_handle[sample(1:nrow(train_handle), 5000),]

# train_handle_sp$event_datetime = as.Date(train_handle_sp$event_datetime, format = "%Y-%m-%d")

table(train_handle_sp$click) ## 1/10 정도 클릭
table(train_handle_sp$placement_type)
table(train_handle_sp$device_ifa) %>% length()
table(train_handle_sp$device_language)
table(train_handle_sp$device_country)
table(train_handle_sp$device_region)















