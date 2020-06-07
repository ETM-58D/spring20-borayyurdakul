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
wide_predictor=dcast(full_data,date~paste0('lag_hours_',hour),value.var='value')
wide_predictor_2=dcast(full_data,date~paste0('lag_hours_48_',hour),value.var='lag_48',)
wide_predictor_3=dcast(full_data,date~paste0('lag_hours_168_',hour),value.var='lag_168',)

target=wide_predictor_2
target1=wide_predictor_3
final_feature_set=merge(wide_predictor,target,by='date')
final_feature_set
final_feature_set_1=merge(final_feature_set,target1,by='date')

full_data_train_2 <- final_feature_set_1 %>%filter(date >= "2016-01-01" & date < "2020-03-01")
full_data_test_2 <- final_feature_set_1 %>%filter(date >= "2020-03-01" & date <= "2020-05-19")

fit_lr_0=lm(lag_hours_0~-lag_hours_1-lag_hours_2-lag_hours_3-lag_hours_4-lag_hours_5-lag_hours_6-lag_hours_7
            -lag_hours_8-lag_hours_9-lag_hours_10-lag_hours_11-lag_hours_12-lag_hours_13-lag_hours_14-lag_hours_15
            -lag_hours_16-lag_hours_17-lag_hours_18-lag_hours_19-lag_hours_20-lag_hours_21
            -lag_hours_22-lag_hours_23,full_data_train_2)
fit_lr_1=lm(lag_hours_1~-lag_hours_0-lag_hours_2-lag_hours_3-lag_hours_4-lag_hours_5-lag_hours_6-lag_hours_7
            -lag_hours_8-lag_hours_9-lag_hours_10-lag_hours_11-lag_hours_12-lag_hours_13-lag_hours_14-lag_hours_15
            -lag_hours_16-lag_hours_17-lag_hours_18-lag_hours_19-lag_hours_20-lag_hours_21
            -lag_hours_22-lag_hours_23,full_data_train_2)
fit_lr_2=lm(lag_hours_2~-lag_hours_0-lag_hours_1-lag_hours_3-lag_hours_4-lag_hours_5-lag_hours_6-lag_hours_7
            -lag_hours_8-lag_hours_9-lag_hours_10-lag_hours_11-lag_hours_12-lag_hours_13-lag_hours_14-lag_hours_15
            -lag_hours_16-lag_hours_17-lag_hours_18-lag_hours_19-lag_hours_20-lag_hours_21
            -lag_hours_22-lag_hours_23,full_data_train_2)
fit_lr_3=lm(lag_hours_3~-lag_hours_0-lag_hours_1-lag_hours_2-lag_hours_4-lag_hours_5-lag_hours_6-lag_hours_7
            -lag_hours_8-lag_hours_9-lag_hours_10-lag_hours_11-lag_hours_12-lag_hours_13-lag_hours_14-lag_hours_15
            -lag_hours_16-lag_hours_17-lag_hours_18-lag_hours_19-lag_hours_20-lag_hours_21
            -lag_hours_22-lag_hours_23,full_data_train_2)
fit_lr_4=lm(lag_hours_4~-lag_hours_0-lag_hours_1-lag_hours_2-lag_hours_3-lag_hours_5-lag_hours_6-lag_hours_7
            -lag_hours_8-lag_hours_9-lag_hours_10-lag_hours_11-lag_hours_12-lag_hours_13-lag_hours_14-lag_hours_15
            -lag_hours_16-lag_hours_17-lag_hours_18-lag_hours_19-lag_hours_20-lag_hours_21
            -lag_hours_22-lag_hours_23,full_data_train_2)
fit_lr_5=lm(lag_hours_5~-lag_hours_0-lag_hours_1-lag_hours_2-lag_hours_3-lag_hours_4-lag_hours_6-lag_hours_7
            -lag_hours_8-lag_hours_9-lag_hours_10-lag_hours_11-lag_hours_12-lag_hours_13-lag_hours_14-lag_hours_15
            -lag_hours_16-lag_hours_17-lag_hours_18-lag_hours_19-lag_hours_20-lag_hours_21
            -lag_hours_22-lag_hours_23,full_data_train_2)
fit_lr_6=lm(lag_hours_6~-lag_hours_0-lag_hours_1-lag_hours_2-lag_hours_3-lag_hours_4-lag_hours_5-lag_hours_7
            -lag_hours_8-lag_hours_9-lag_hours_10-lag_hours_11-lag_hours_12-lag_hours_13-lag_hours_14-lag_hours_15
            -lag_hours_16-lag_hours_17-lag_hours_18-lag_hours_19-lag_hours_20-lag_hours_21
            -lag_hours_22-lag_hours_23,full_data_train_2)
fit_lr_7=lm(lag_hours_7~-lag_hours_0-lag_hours_1-lag_hours_2-lag_hours_3-lag_hours_4-lag_hours_5-lag_hours_6
            -lag_hours_8-lag_hours_9-lag_hours_10-lag_hours_11-lag_hours_12-lag_hours_13-lag_hours_14-lag_hours_15
            -lag_hours_16-lag_hours_17-lag_hours_18-lag_hours_19-lag_hours_20-lag_hours_21
            -lag_hours_22-lag_hours_23,full_data_train_2)
fit_lr_8=lm(lag_hours_8~-lag_hours_0-lag_hours_1-lag_hours_2-lag_hours_3-lag_hours_4-lag_hours_5-lag_hours_6
            -lag_hours_7-lag_hours_9-lag_hours_10-lag_hours_11-lag_hours_12-lag_hours_13-lag_hours_14-lag_hours_15
            -lag_hours_16-lag_hours_17-lag_hours_18-lag_hours_19-lag_hours_20-lag_hours_21
            -lag_hours_22-lag_hours_23,full_data_train_2)
fit_lr_9=lm(lag_hours_9~-lag_hours_0-lag_hours_1-lag_hours_2-lag_hours_3-lag_hours_4-lag_hours_5-lag_hours_6
            -lag_hours_7-lag_hours_8-lag_hours_10-lag_hours_11-lag_hours_12-lag_hours_13-lag_hours_14-lag_hours_15
            -lag_hours_16-lag_hours_17-lag_hours_18-lag_hours_19-lag_hours_20-lag_hours_21
            -lag_hours_22-lag_hours_23,full_data_train_2)
fit_lr_10=lm(lag_hours_10~-lag_hours_0-lag_hours_1-lag_hours_2-lag_hours_3-lag_hours_4-lag_hours_5-lag_hours_6
             -lag_hours_7-lag_hours_8-lag_hours_9-lag_hours_11-lag_hours_12-lag_hours_13-lag_hours_14-lag_hours_15
             -lag_hours_16-lag_hours_17-lag_hours_18-lag_hours_19-lag_hours_20-lag_hours_21
             -lag_hours_22-lag_hours_23,full_data_train_2)
fit_lr_11=lm(lag_hours_10~-lag_hours_0-lag_hours_1-lag_hours_2-lag_hours_3-lag_hours_4-lag_hours_5-lag_hours_6
             -lag_hours_7-lag_hours_8-lag_hours_9-lag_hours_10-lag_hours_12-lag_hours_13-lag_hours_14-lag_hours_15
             -lag_hours_16-lag_hours_17-lag_hours_18-lag_hours_19-lag_hours_20-lag_hours_21
             -lag_hours_22-lag_hours_23,full_data_train_2)
fit_lr_12=lm(lag_hours_10~-lag_hours_0-lag_hours_1-lag_hours_2-lag_hours_3-lag_hours_4-lag_hours_5-lag_hours_6
             -lag_hours_7-lag_hours_8-lag_hours_9-lag_hours_10-lag_hours_11-lag_hours_13-lag_hours_14-lag_hours_15
             -lag_hours_16-lag_hours_17-lag_hours_18-lag_hours_19-lag_hours_20-lag_hours_21
             -lag_hours_22-lag_hours_23,full_data_train_2)
fit_lr_13=lm(lag_hours_13~-lag_hours_0-lag_hours_1-lag_hours_2-lag_hours_3-lag_hours_4-lag_hours_5-lag_hours_6
             -lag_hours_7-lag_hours_8-lag_hours_9-lag_hours_10-lag_hours_11-lag_hours_13-lag_hours_14-lag_hours_15
             -lag_hours_16-lag_hours_17-lag_hours_18-lag_hours_19-lag_hours_20-lag_hours_21
             -lag_hours_22-lag_hours_23,full_data_train_2)
fit_lr_14=lm(lag_hours_14~-lag_hours_0-lag_hours_1-lag_hours_2-lag_hours_3-lag_hours_4-lag_hours_5-lag_hours_6
             -lag_hours_7-lag_hours_8-lag_hours_9-lag_hours_10-lag_hours_11-lag_hours_12-lag_hours_13-lag_hours_15
             -lag_hours_16-lag_hours_17-lag_hours_18-lag_hours_19-lag_hours_20-lag_hours_21
             -lag_hours_22-lag_hours_23,full_data_train_2)
fit_lr_15=lm(lag_hours_15~-lag_hours_0-lag_hours_1-lag_hours_2-lag_hours_3-lag_hours_4-lag_hours_5-lag_hours_6
             -lag_hours_7-lag_hours_8-lag_hours_9-lag_hours_10-lag_hours_11-lag_hours_12-lag_hours_13-lag_hours_14
             -lag_hours_16-lag_hours_17-lag_hours_18-lag_hours_19-lag_hours_20-lag_hours_21
             -lag_hours_22-lag_hours_23,full_data_train_2)
fit_lr_16=lm(lag_hours_16~-lag_hours_0-lag_hours_1-lag_hours_2-lag_hours_3-lag_hours_4-lag_hours_5-lag_hours_6
             -lag_hours_7-lag_hours_8-lag_hours_9-lag_hours_10-lag_hours_11-lag_hours_12-lag_hours_13-lag_hours_14
             -lag_hours_15-lag_hours_17-lag_hours_18-lag_hours_19-lag_hours_20-lag_hours_21
             -lag_hours_22-lag_hours_23,full_data_train_2)
fit_lr_17=lm(lag_hours_17~-lag_hours_0-lag_hours_1-lag_hours_2-lag_hours_3-lag_hours_4-lag_hours_5-lag_hours_6
             -lag_hours_7-lag_hours_8-lag_hours_9-lag_hours_10-lag_hours_11-lag_hours_12-lag_hours_13-lag_hours_14
             -lag_hours_15-lag_hours_16-lag_hours_18-lag_hours_19-lag_hours_20-lag_hours_21
             -lag_hours_22-lag_hours_23,full_data_train_2)
fit_lr_18=lm(lag_hours_18~-lag_hours_0-lag_hours_1-lag_hours_2-lag_hours_3-lag_hours_4-lag_hours_5-lag_hours_6
             -lag_hours_7-lag_hours_8-lag_hours_9-lag_hours_10-lag_hours_11-lag_hours_12-lag_hours_13-lag_hours_14
             -lag_hours_15-lag_hours_16-lag_hours_17-lag_hours_19-lag_hours_20-lag_hours_21
             -lag_hours_22-lag_hours_23,full_data_train_2)
fit_lr_19=lm(lag_hours_19~-lag_hours_0-lag_hours_1-lag_hours_2-lag_hours_3-lag_hours_4-lag_hours_5-lag_hours_6
             -lag_hours_7-lag_hours_8-lag_hours_9-lag_hours_10-lag_hours_11-lag_hours_12-lag_hours_13-lag_hours_14
             -lag_hours_15-lag_hours_16-lag_hours_17-lag_hours_18-lag_hours_20-lag_hours_21
             -lag_hours_22-lag_hours_23,full_data_train_2)
fit_lr_20=lm(lag_hours_20~-lag_hours_0-lag_hours_1-lag_hours_2-lag_hours_3-lag_hours_4-lag_hours_5-lag_hours_6
             -lag_hours_7-lag_hours_8-lag_hours_9-lag_hours_10-lag_hours_11-lag_hours_12-lag_hours_13-lag_hours_14
             -lag_hours_15-lag_hours_16-lag_hours_17-lag_hours_18-lag_hours_19-lag_hours_21
             -lag_hours_22-lag_hours_23,full_data_train_2)
fit_lr_21=lm(lag_hours_21~-lag_hours_0-lag_hours_1-lag_hours_2-lag_hours_3-lag_hours_4-lag_hours_5-lag_hours_6
             -lag_hours_7-lag_hours_8-lag_hours_9-lag_hours_10-lag_hours_11-lag_hours_12-lag_hours_13-lag_hours_14
             -lag_hours_15-lag_hours_16-lag_hours_17-lag_hours_18-lag_hours_19-lag_hours_20
             -lag_hours_22-lag_hours_23,full_data_train_2)
fit_lr_22=lm(lag_hours_22~-lag_hours_0-lag_hours_1-lag_hours_2-lag_hours_3-lag_hours_4-lag_hours_5-lag_hours_6
             -lag_hours_7-lag_hours_8-lag_hours_9-lag_hours_10-lag_hours_11-lag_hours_12-lag_hours_13-lag_hours_14
             -lag_hours_15-lag_hours_16-lag_hours_17-lag_hours_18-lag_hours_19-lag_hours_20
             -lag_hours_21-lag_hours_23,full_data_train_2)
fit_lr_23=lm(lag_hours_23~-lag_hours_0-lag_hours_1-lag_hours_2-lag_hours_3-lag_hours_4-lag_hours_5-lag_hours_6
             -lag_hours_7-lag_hours_8-lag_hours_9-lag_hours_10-lag_hours_11-lag_hours_12-lag_hours_13-lag_hours_14
             -lag_hours_15-lag_hours_16-lag_hours_17-lag_hours_18-lag_hours_19-lag_hours_20
             -lag_hours_21-lag_hours_22,full_data_train_2)


full_data_test_2$predicted_0 <- as.data.table(predict(fit_lr_0,full_data_test_2))
full_data_test_2$predicted_1 <- as.data.table(predict(fit_lr_1,full_data_test_2))
full_data_test_2$predicted_2 <- as.data.table(predict(fit_lr_2,full_data_test_2))
full_data_test_2$predicted_3 <- as.data.table(predict(fit_lr_3,full_data_test_2))
full_data_test_2$predicted_4 <- as.data.table(predict(fit_lr_4,full_data_test_2))
full_data_test_2$predicted_5 <- as.data.table(predict(fit_lr_5,full_data_test_2))
full_data_test_2$predicted_6 <- as.data.table(predict(fit_lr_6,full_data_test_2))
full_data_test_2$predicted_7 <- as.data.table(predict(fit_lr_7,full_data_test_2))
full_data_test_2$predicted_8 <- as.data.table(predict(fit_lr_8,full_data_test_2))
full_data_test_2$predicted_9 <- as.data.table(predict(fit_lr_9,full_data_test_2))
full_data_test_2$predicted_10 <- as.data.table(predict(fit_lr_10,full_data_test_2))
full_data_test_2$predicted_11 <- as.data.table(predict(fit_lr_11,full_data_test_2))
full_data_test_2$predicted_12 <- as.data.table(predict(fit_lr_12,full_data_test_2))
full_data_test_2$predicted_13 <- as.data.table(predict(fit_lr_13,full_data_test_2))
full_data_test_2$predicted_14 <- as.data.table(predict(fit_lr_14,full_data_test_2))
full_data_test_2$predicted_15 <- as.data.table(predict(fit_lr_15,full_data_test_2))
full_data_test_2$predicted_16 <- as.data.table(predict(fit_lr_16,full_data_test_2))
full_data_test_2$predicted_17 <- as.data.table(predict(fit_lr_17,full_data_test_2))
full_data_test_2$predicted_18 <- as.data.table(predict(fit_lr_18,full_data_test_2))
full_data_test_2$predicted_19 <- as.data.table(predict(fit_lr_19,full_data_test_2))
full_data_test_2$predicted_20 <- as.data.table(predict(fit_lr_20,full_data_test_2))
full_data_test_2$predicted_21 <- as.data.table(predict(fit_lr_21,full_data_test_2))
full_data_test_2$predicted_22 <- as.data.table(predict(fit_lr_22,full_data_test_2))
full_data_test_2$predicted_23 <- as.data.table(predict(fit_lr_23,full_data_test_2))

full_data_test_2$predicted_MAPE_0=with(full_data_test_2,abs((lag_hours_0-predicted_0)/lag_hours_0)*100)
full_data_test_2$mse_b_0=with(full_data_test_2,sum(full_data_test_2$predicted_MAPE_0)/nrow(full_data_test_2))
full_data_test_2$predicted_MAPE_1=with(full_data_test_2,abs((lag_hours_1-predicted_1)/lag_hours_1)*100)
full_data_test_2$mse_b_1=with(full_data_test_2,sum(full_data_test_2$predicted_MAPE_1)/nrow(full_data_test_2))
full_data_test_2$predicted_MAPE_2=with(full_data_test_2,abs((lag_hours_2-predicted_2)/lag_hours_2)*100)
full_data_test_2$mse_b_2=with(full_data_test_2,sum(full_data_test_2$predicted_MAPE_2)/nrow(full_data_test_2))
full_data_test_2$predicted_MAPE_3=with(full_data_test_2,abs((lag_hours_3-predicted_3)/lag_hours_3)*100)
full_data_test_2$mse_b_3=with(full_data_test_2,sum(full_data_test_2$predicted_MAPE_3)/nrow(full_data_test_2))
full_data_test_2$predicted_MAPE_4=with(full_data_test_2,abs((lag_hours_4-predicted_4)/lag_hours_4)*100)
full_data_test_2$mse_b_4=with(full_data_test_2,sum(full_data_test_2$predicted_MAPE_4)/nrow(full_data_test_2))
full_data_test_2$predicted_MAPE_5=with(full_data_test_2,abs((lag_hours_5-predicted_5)/lag_hours_5)*100)
full_data_test_2$mse_b_5=with(full_data_test_2,sum(full_data_test_2$predicted_MAPE_5)/nrow(full_data_test_2))
full_data_test_2$predicted_MAPE_6=with(full_data_test_2,abs((lag_hours_6-predicted_6)/lag_hours_6)*100)
full_data_test_2$mse_b_6=with(full_data_test_2,sum(full_data_test_2$predicted_MAPE_6)/nrow(full_data_test_2))
full_data_test_2$predicted_MAPE_7=with(full_data_test_2,abs((lag_hours_7-predicted_7)/lag_hours_7)*100)
full_data_test_2$mse_b_7=with(full_data_test_2,sum(full_data_test_2$predicted_MAPE_7)/nrow(full_data_test_2))
full_data_test_2$predicted_MAPE_8=with(full_data_test_2,abs((lag_hours_8-predicted_8)/lag_hours_8)*100)
full_data_test_2$mse_b_8=with(full_data_test_2,sum(full_data_test_2$predicted_MAPE_8)/nrow(full_data_test_2))
full_data_test_2$predicted_MAPE_9=with(full_data_test_2,abs((lag_hours_9-predicted_9)/lag_hours_9)*100)
full_data_test_2$mse_b_9=with(full_data_test_2,sum(full_data_test_2$predicted_MAPE_9)/nrow(full_data_test_2))
full_data_test_2$predicted_MAPE_10=with(full_data_test_2,abs((lag_hours_10-predicted_10)/lag_hours_10)*100)
full_data_test_2$mse_b_10=with(full_data_test_2,sum(full_data_test_2$predicted_MAPE_10)/nrow(full_data_test_2))
full_data_test_2$predicted_MAPE_11=with(full_data_test_2,abs((lag_hours_11-predicted_11)/lag_hours_11)*100)
full_data_test_2$mse_b_11=with(full_data_test_2,sum(full_data_test_2$predicted_MAPE_11)/nrow(full_data_test_2))
full_data_test_2$predicted_MAPE_12=with(full_data_test_2,abs((lag_hours_12-predicted_12)/lag_hours_12)*100)
full_data_test_2$mse_b_12=with(full_data_test_2,sum(full_data_test_2$predicted_MAPE_12)/nrow(full_data_test_2))
full_data_test_2$predicted_MAPE_13=with(full_data_test_2,abs((lag_hours_13-predicted_13)/lag_hours_13)*100)
full_data_test_2$mse_b_13=with(full_data_test_2,sum(full_data_test_2$predicted_MAPE_13)/nrow(full_data_test_2))
full_data_test_2$predicted_MAPE_14=with(full_data_test_2,abs((lag_hours_14-predicted_14)/lag_hours_14)*100)
full_data_test_2$mse_b_14=with(full_data_test_2,sum(full_data_test_2$predicted_MAPE_14)/nrow(full_data_test_2))
full_data_test_2$predicted_MAPE_15=with(full_data_test_2,abs((lag_hours_15-predicted_15)/lag_hours_15)*100)
full_data_test_2$mse_b_15=with(full_data_test_2,sum(full_data_test_2$predicted_MAPE_15)/nrow(full_data_test_2))
full_data_test_2$predicted_MAPE_16=with(full_data_test_2,abs((lag_hours_16-predicted_16)/lag_hours_16)*100)
full_data_test_2$mse_b_16=with(full_data_test_2,sum(full_data_test_2$predicted_MAPE_16)/nrow(full_data_test_2))
full_data_test_2$predicted_MAPE_17=with(full_data_test_2,abs((lag_hours_17-predicted_17)/lag_hours_17)*100)
full_data_test_2$mse_b_17=with(full_data_test_2,sum(full_data_test_2$predicted_MAPE_17)/nrow(full_data_test_2))
full_data_test_2$predicted_MAPE_18=with(full_data_test_2,abs((lag_hours_18-predicted_18)/lag_hours_18)*100)
full_data_test_2$mse_b_18=with(full_data_test_2,sum(full_data_test_2$predicted_MAPE_18)/nrow(full_data_test_2))
full_data_test_2$predicted_MAPE_19=with(full_data_test_2,abs((lag_hours_19-predicted_19)/lag_hours_19)*100)
full_data_test_2$mse_b_19=with(full_data_test_2,sum(full_data_test_2$predicted_MAPE_19)/nrow(full_data_test_2))
full_data_test_2$predicted_MAPE_20=with(full_data_test_2,abs((lag_hours_20-predicted_20)/lag_hours_20)*100)
full_data_test_2$mse_b_20=with(full_data_test_2,sum(full_data_test_2$predicted_MAPE_20)/nrow(full_data_test_2))
full_data_test_2$predicted_MAPE_21=with(full_data_test_2,abs((lag_hours_21-predicted_21)/lag_hours_21)*100)
full_data_test_2$mse_b_21=with(full_data_test_2,sum(full_data_test_2$predicted_MAPE_21)/nrow(full_data_test_2))
full_data_test_2$predicted_MAPE_22=with(full_data_test_2,abs((lag_hours_22-predicted_22)/lag_hours_22)*100)
full_data_test_2$mse_b_22=with(full_data_test_2,sum(full_data_test_2$predicted_MAPE_22)/nrow(full_data_test_2))
full_data_test_2$predicted_MAPE_23=with(full_data_test_2,abs((lag_hours_23-predicted_23)/lag_hours_23)*100)
full_data_test_2$mse_b_23=with(full_data_test_2,sum(full_data_test_2$predicted_MAPE_23)/nrow(full_data_test_2))