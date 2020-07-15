require(data.table)
require(dplyr)
require(tidyr)
require(glmnet)
set.seed(100)
trendyol=fread("/Users/ayhanertuglu/Desktop/Okul/II.Dönem/Business Analytics/Project/Data/Boun Content Challenge - Daily Actions(04-07-2020).csv")
trendyol[,date:=as.Date(event_date,'%Y-%m-%d')]
trendyol=trendyol[,list(product_content_id,date,sold_count)]
str(trendyol)
trendyol$sold_count=as.numeric(trendyol$sold_count)
trendyol <- trendyol[order(date,product_content_id),]
trendyol[,lag_40:=shift(sold_count,40)]
part_d_matrix=data.table(trendyol[,sold_count])
names(part_d_matrix)[names(part_d_matrix) == "V1"] <- "sold_count"
part_d_matrix$lag_40=data.table(trendyol[,lag_40])
part_d_matrix$MAPE=with(part_d_matrix,abs((sold_count-lag_40)/sold_count)*100)
is.na(part_d_matrix$MAPE)<-sapply(part_d_matrix$MAPE, is.infinite)
part_d_matrix$MAPE[is.na(part_d_matrix$MAPE)]<-mean(complete.cases(part_d_matrix$MAPE))
part_d_matrix$overallMAPE=with(part_d_matrix,sum(part_d_matrix$MAPE)/nrow(part_d_matrix))

trendyol=tail(trendyol, 8)