library(DataExplorer)
library(data.table)
library(dplyr)

aud = fread("/root/data/audience_profile_pre.csv")
aud = aud[,-c(1,6,7,9)]
aud[aud == ""] = NA

fwrite(aud,"/root/data/aud_ver1.csv")

train = fread("/root/data/train.csv")
test = fread("/root/data/test.csv")


join_train_ver1 = left_join(train, aud, by = c("device_ifa" = "device_ifa"))
join_train_ver1[join_train_ver1 == ""] = NA
fwrite(join1, "/root/data/join_train_ver3_2.csv")

join_test_ver1 = left_join(test, aud, by = c("device_ifa" = "device_ifa"))
join_test_ver1[join_test_ver1 == ""] = NA
fwrite(join_test_ver1, "join_test_ver3_2.csv")


