require(data.table)
require(dplyr)
require(tidyr)
require(glmnet)
require(ggplot2)
set.seed(100)
consumption=fread("Desktop/Okul/II.Dönem/Business Analytics/Homework 2-3/GercekZamanliTuketim-01012016-19052020.csv")
consumption
setnames(consumption,names(consumption)[3],'value')
consumption[,date:=as.Date(Tarih,'%d.%m.%Y')]
consumption[,hour:=as.numeric(substr(Saat,1,2))]
consumption=consumption[,list(date,hour,value)]
consumption[,value:=gsub(".", "",value, fixed = TRUE)]
consumption[,value:=as.numeric(gsub(",", ".",value, fixed = TRUE))]
head(consumption)
consumption[,lag_168:=shift(value,168)]
consumption[,lag_48:=shift(value,48)]
consumption[1:169]
full_data=consumption[complete.cases(consumption)]
full_data
full_data_train <- full_data %>%filter(date >= "2016-01-01" & date < "2020-03-01")
full_data_test <- full_data %>%filter(date >= "2020-03-01" & date <= "2020-05-19")
fit_lr=lm(value~+lag_168+lag_48,full_data_train)
summary(fit_lr)
pred.full_data_test <- predict(fit_lr,full_data_test)
pred.full_data_test
pred.full_data_test <- as.data.table(pred.full_data_test)
full_data_test$predicted <- pred.full_data_test
full_data_test$predicted_APE=with(full_data_test,abs((value-predicted)/value)*100)
full_data_test$MAPE_b=with(full_data_test,sum(full_data_test$predicted_APE)/nrow(full_data_test))
mse_b=sum(full_data_test$predicted_MAPE)/nrow(full_data_test)