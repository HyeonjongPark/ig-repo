#install.packages("DataExplorer")

library(data.table)
library(DataExplorer)

setwd("C:/Users/onycom/Desktop/igaworks/all")

src_dir <- c(setwd("C:/Users/onycom/Desktop/igaworks/all"))
src_file <- list.files(src_dir) # list
src_file

aud = fread(src_file[1] , fill = T)
aud
sub = fread(src_file[2])
sub
test = fread(src_file[3])
test
train = fread(src_file[4])
train



