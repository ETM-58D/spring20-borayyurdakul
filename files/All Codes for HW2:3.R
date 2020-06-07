require(data.table)
require(dplyr)
require(tidyr)
require(glmnet)
require(ggplot2)
set.seed(100)

#a

consumption=fread("/Users/Boray/Documents/school/bogazici/ETM58d/hw2-3/GercekZamanliTuketim-01012016-19052020.csv")
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
full_data_2 <- full_data %>%filter(date >= "2020-03-01" & date <= "2020-05-19")
full_data_2
full_data_2$lag_168_MAPE=with(full_data_2,abs((value-lag_168)/value)*100)
full_data_2$MAPE_a_168=with(full_data_2,sum(full_data_2$lag_168_MAPE)/nrow(full_data_2))
full_data_2$lag_48_MAPE=with(full_data_2,abs((value-lag_48)/value)*100)
full_data_2$MAPE_a_48=with(full_data_2,sum(full_data_2$lag_48_MAPE)/nrow(full_data_2))
ggplot(full_data_2, aes(hour, lag_168_MAPE, fill = hour, group = hour)) +
  geom_boxplot() +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  stat_summary(geom="text", fun=quantile, colour = "dark grey",
               aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(y=0.5), size=3.5) +
  stat_summary(geom="text", fun=mean, colour = "white",
               aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(y=0.5), size=3.5) +
  scale_x_continuous(breaks=seq(0, 24, by = 1)) + 
  labs(title= "Hourly Distribution of LAG_168 MAPE Values",
       x= "Hour",
       y= "MAPE")

ggplot(full_data_2, aes(hour, lag_48_MAPE, fill = hour, group = hour)) +
  geom_boxplot() +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  stat_summary(geom="text", fun=quantile, colour = "dark grey",
               aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(y=0.5), size=3.5) +
  stat_summary(geom="text", fun=mean, colour = "white",
               aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(y=0.5), size=3.5) +
  scale_x_continuous(breaks=seq(0, 24, by = 1)) + 
  labs(title= "Hourly Distribution of LAG_48 MAPE Values",
       x= "Hour",
       y= "MAPE")

#b

#full_data
full_data_train <- full_data %>%filter(date >= "2016-01-01" & date <= "2020-03-01")
full_data_test <- full_data %>%filter(date >= "2020-03-02" & date <= "2020-05-19")
fit_lr=lm(value~+lag_168+lag_48,full_data_train)
#summary(fit_lr)
pred.full_data_test <- predict(fit_lr,full_data_test)
pred.full_data_test
pred.full_data_test <- as.data.table(pred.full_data_test)
full_data_test$predicted <- pred.full_data_test
full_data_test$predicted_MAPE=with(full_data_test,abs((value-predicted)/value)*100)
mse_b=sum(full_data_test$predicted_MAPE)/nrow(full_data_test)

#c

wide_predictor=dcast(full_data,date~paste0('lag_hours_',hour),value.var='value')
wide_predictor_2=dcast(full_data,date~paste0('lag_hours_48_',hour),value.var='lag_48',)
wide_predictor_3=dcast(full_data,date~paste0('lag_hours_168_',hour),value.var='lag_168',)

target=wide_predictor_2
target1=wide_predictor_3
final_feature_set=merge(wide_predictor,target,by='date')
final_feature_set
final_feature_set_1=merge(final_feature_set,target1,by='date')

full_data_train_2 <- final_feature_set_1 %>%filter(date >= "2016-01-01" & date <= "2020-03-01")
full_data_test_2 <- final_feature_set_1 %>%filter(date >= "2020-03-02" & date <= "2020-05-19")

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

#d

#hour0
wide_predictor_d_2=dcast(full_data,date~paste0('lag_hours_48_',hour),value.var='lag_48')
wide_predictor_d_3=dcast(full_data,date~paste0('lag_hours_168_',hour),value.var='lag_168')

target1=full_data[hour==0]
final_feature_set=merge(wide_predictor_d_2,wide_predictor_d_3,by='date')
final_feature_set_1=merge(final_feature_set,target1[,list(date,value)],by='date')
full_data_train_3 <- final_feature_set_1 %>%filter(date >= "2016-01-01" & date <= "2020-03-01")
full_data_test_3 <- final_feature_set_1 %>%filter(date >= "2020-03-02" & date <= "2020-05-19")
names(full_data_test_3)[names(full_data_test_3) == "value"] <- "value_0"
full_data_train_3_mat <- model.matrix( ~ ., full_data_train_3[,-50])
full_data_test_3_mat <- model.matrix( ~ ., full_data_test_3[,-50])
train_target=as.matrix(full_data_train_3[,50])
fit_glmnet_0=cv.glmnet(full_data_train_3_mat,train_target,type.measure='mse', trace.it = 1)
plot(fit_glmnet_0)
coef(fit_glmnet_0,s='lambda.min')
part_d_matrix=data.table(full_data_test_3[,value_0])
names(part_d_matrix)[names(part_d_matrix) == "V1"] <- "value_hour_0"
predicted_val_0=predict(fit_glmnet_0,s='lambda.min',full_data_test_3_mat)
part_d_matrix$predicted_hour_0=data.table(predicted_val_0)
part_d_matrix$hour_0_MAPE=with(full_data_test_3,abs((value_0-predicted_val_0)/value_0)*100)
part_d_matrix$hour_0_overallMAPE=with(part_d_matrix,sum(part_d_matrix$hour_0_MAPE)/nrow(part_d_matrix))

#hour1
wide_predictor_d_2=dcast(full_data,date~paste0('lag_hours_48_',hour),value.var='lag_48')
wide_predictor_d_3=dcast(full_data,date~paste0('lag_hours_168_',hour),value.var='lag_168')

target1=full_data[hour==1]
final_feature_set=merge(wide_predictor_d_2,wide_predictor_d_3,by='date')
final_feature_set_1=merge(final_feature_set,target1[,list(date,value)],by='date')
full_data_train_3 <- final_feature_set_1 %>%filter(date >= "2016-01-01" & date <= "2020-03-01")
full_data_test_3 <- final_feature_set_1 %>%filter(date >= "2020-03-02" & date <= "2020-05-19")
names(full_data_test_3)[names(full_data_test_3) == "value"] <- "value_1"
full_data_train_3_mat <- model.matrix( ~ ., full_data_train_3[,-50])
full_data_test_3_mat <- model.matrix( ~ ., full_data_test_3[,-50])
train_target=as.matrix(full_data_train_3[,50])
fit_glmnet_1=cv.glmnet(full_data_train_3_mat,train_target,type.measure='mse', trace.it = 1)
predicted_val_1=predict(fit_glmnet_1,s='lambda.min',full_data_test_3_mat)
part_d_matrix$value_hour_1=data.table(full_data_test_3[,value_1])
part_d_matrix$predicted_val_1=data.table(predicted_val_1)
part_d_matrix$hour_1_MAPE=with(full_data_test_3,abs((value_1-predicted_val_1)/value_1)*100)
part_d_matrix$hour_1_overallMAPE=with(part_d_matrix,sum(part_d_matrix$hour_1_MAPE)/nrow(part_d_matrix))

#hour2
wide_predictor_d_2=dcast(full_data,date~paste0('lag_hours_48_',hour),value.var='lag_48')
wide_predictor_d_3=dcast(full_data,date~paste0('lag_hours_168_',hour),value.var='lag_168')

target2=full_data[hour==2]
final_feature_set=merge(wide_predictor_d_2,wide_predictor_d_3,by='date')
final_feature_set_1=merge(final_feature_set,target1[,list(date,value)],by='date')
full_data_train_3 <- final_feature_set_1 %>%filter(date >= "2016-01-01" & date <= "2020-03-01")
full_data_test_3 <- final_feature_set_1 %>%filter(date >= "2020-03-02" & date <= "2020-05-19")
names(full_data_test_3)[names(full_data_test_3) == "value"] <- "value_2"
full_data_train_3_mat <- model.matrix( ~ ., full_data_train_3[,-50])
full_data_test_3_mat <- model.matrix( ~ ., full_data_test_3[,-50])
train_target=as.matrix(full_data_train_3[,50])
fit_glmnet_2=cv.glmnet(full_data_train_3_mat,train_target,type.measure='mse', trace.it = 1)
predicted_val_2=predict(fit_glmnet_2,s='lambda.min',full_data_test_3_mat)
part_d_matrix$value_hour_2=data.table(full_data_test_3[,value_2])
part_d_matrix$predicted_val_2=data.table(predicted_val_2)
part_d_matrix$hour_2_MAPE=with(full_data_test_3,abs((value_2-predicted_val_2)/value_2)*100)
part_d_matrix$hour_2_overallMAPE=with(part_d_matrix,sum(part_d_matrix$hour_2_MAPE)/nrow(part_d_matrix))

#hour3
wide_predictor_d_2=dcast(full_data,date~paste0('lag_hours_48_',hour),value.var='lag_48')
wide_predictor_d_3=dcast(full_data,date~paste0('lag_hours_168_',hour),value.var='lag_168')

target3=full_data[hour==3]
final_feature_set=merge(wide_predictor_d_2,wide_predictor_d_3,by='date')
final_feature_set_1=merge(final_feature_set,target1[,list(date,value)],by='date')
full_data_train_3 <- final_feature_set_1 %>%filter(date >= "2016-01-01" & date <= "2020-03-01")
full_data_test_3 <- final_feature_set_1 %>%filter(date >= "2020-03-02" & date <= "2020-05-19")
names(full_data_test_3)[names(full_data_test_3) == "value"] <- "value_3"
full_data_train_3_mat <- model.matrix( ~ ., full_data_train_3[,-50])
full_data_test_3_mat <- model.matrix( ~ ., full_data_test_3[,-50])
train_target=as.matrix(full_data_train_3[,50])
fit_glmnet_3=cv.glmnet(full_data_train_3_mat,train_target,type.measure='mse', trace.it = 1)
predicted_val_3=predict(fit_glmnet_3,s='lambda.min',full_data_test_3_mat)
part_d_matrix$value_hour_3=data.table(full_data_test_3[,value_3])
part_d_matrix$predicted_val_3=data.table(predicted_val_3)
part_d_matrix$hour_3_MAPE=with(full_data_test_3,abs((value_3-predicted_val_3)/value_3)*100)
part_d_matrix$hour_3_overallMAPE=with(part_d_matrix,sum(part_d_matrix$hour_3_MAPE)/nrow(part_d_matrix))

#hour4
wide_predictor_d_2=dcast(full_data,date~paste0('lag_hours_48_',hour),value.var='lag_48')
wide_predictor_d_3=dcast(full_data,date~paste0('lag_hours_168_',hour),value.var='lag_168')

target4=full_data[hour==4]
final_feature_set=merge(wide_predictor_d_2,wide_predictor_d_3,by='date')
final_feature_set_1=merge(final_feature_set,target1[,list(date,value)],by='date')
full_data_train_3 <- final_feature_set_1 %>%filter(date >= "2016-01-01" & date <= "2020-03-01")
full_data_test_3 <- final_feature_set_1 %>%filter(date >= "2020-03-02" & date <= "2020-05-19")
names(full_data_test_3)[names(full_data_test_3) == "value"] <- "value_4"
full_data_train_3_mat <- model.matrix( ~ ., full_data_train_3[,-50])
full_data_test_3_mat <- model.matrix( ~ ., full_data_test_3[,-50])
train_target=as.matrix(full_data_train_3[,50])
fit_glmnet_4=cv.glmnet(full_data_train_3_mat,train_target,type.measure='mse', trace.it = 1)
predicted_val_4=predict(fit_glmnet_4,s='lambda.min',full_data_test_3_mat)
part_d_matrix$value_hour_4=data.table(full_data_test_3[,value_4])
part_d_matrix$predicted_val_4=data.table(predicted_val_4)
part_d_matrix$hour_4_MAPE=with(full_data_test_3,abs((value_4-predicted_val_4)/value_4)*100)
part_d_matrix$hour_4_overallMAPE=with(part_d_matrix,sum(part_d_matrix$hour_4_MAPE)/nrow(part_d_matrix))

#hour5
wide_predictor_d_2=dcast(full_data,date~paste0('lag_hours_48_',hour),value.var='lag_48')
wide_predictor_d_3=dcast(full_data,date~paste0('lag_hours_168_',hour),value.var='lag_168')

target5=full_data[hour==5]
final_feature_set=merge(wide_predictor_d_2,wide_predictor_d_3,by='date')
final_feature_set_1=merge(final_feature_set,target1[,list(date,value)],by='date')
full_data_train_3 <- final_feature_set_1 %>%filter(date >= "2016-01-01" & date <= "2020-03-01")
full_data_test_3 <- final_feature_set_1 %>%filter(date >= "2020-03-02" & date <= "2020-05-19")
names(full_data_test_3)[names(full_data_test_3) == "value"] <- "value_5"
full_data_train_3_mat <- model.matrix( ~ ., full_data_train_3[,-50])
full_data_test_3_mat <- model.matrix( ~ ., full_data_test_3[,-50])
train_target=as.matrix(full_data_train_3[,50])
fit_glmnet_5=cv.glmnet(full_data_train_3_mat,train_target,type.measure='mse', trace.it = 1)
predicted_val_5=predict(fit_glmnet_5,s='lambda.min',full_data_test_3_mat)
part_d_matrix$value_hour_5=data.table(full_data_test_3[,value_5])
part_d_matrix$predicted_val_5=data.table(predicted_val_5)
part_d_matrix$hour_5_MAPE=with(full_data_test_3,abs((value_5-predicted_val_5)/value_5)*100)
part_d_matrix$hour_5_overallMAPE=with(part_d_matrix,sum(part_d_matrix$hour_5_MAPE)/nrow(part_d_matrix))

#hour6
wide_predictor_d_2=dcast(full_data,date~paste0('lag_hours_48_',hour),value.var='lag_48')
wide_predictor_d_3=dcast(full_data,date~paste0('lag_hours_168_',hour),value.var='lag_168')

target6=full_data[hour==6]
final_feature_set=merge(wide_predictor_d_2,wide_predictor_d_3,by='date')
final_feature_set_1=merge(final_feature_set,target1[,list(date,value)],by='date')
full_data_train_3 <- final_feature_set_1 %>%filter(date >= "2016-01-01" & date <= "2020-03-01")
full_data_test_3 <- final_feature_set_1 %>%filter(date >= "2020-03-02" & date <= "2020-05-19")
names(full_data_test_3)[names(full_data_test_3) == "value"] <- "value_6"
full_data_train_3_mat <- model.matrix( ~ ., full_data_train_3[,-50])
full_data_test_3_mat <- model.matrix( ~ ., full_data_test_3[,-50])
train_target=as.matrix(full_data_train_3[,50])
fit_glmnet_6=cv.glmnet(full_data_train_3_mat,train_target,type.measure='mse', trace.it = 1)
predicted_val_6=predict(fit_glmnet_6,s='lambda.min',full_data_test_3_mat)
part_d_matrix$value_hour_6=data.table(full_data_test_3[,value_6])
part_d_matrix$predicted_val_6=data.table(predicted_val_6)
part_d_matrix$hour_6_MAPE=with(full_data_test_3,abs((value_6-predicted_val_6)/value_6)*100)
part_d_matrix$hour_6_overallMAPE=with(part_d_matrix,sum(part_d_matrix$hour_6_MAPE)/nrow(part_d_matrix))

#hour7
wide_predictor_d_2=dcast(full_data,date~paste0('lag_hours_48_',hour),value.var='lag_48')
wide_predictor_d_3=dcast(full_data,date~paste0('lag_hours_168_',hour),value.var='lag_168')

target7=full_data[hour==7]
final_feature_set=merge(wide_predictor_d_2,wide_predictor_d_3,by='date')
final_feature_set_1=merge(final_feature_set,target1[,list(date,value)],by='date')
full_data_train_3 <- final_feature_set_1 %>%filter(date >= "2016-01-01" & date <= "2020-03-01")
full_data_test_3 <- final_feature_set_1 %>%filter(date >= "2020-03-02" & date <= "2020-05-19")
names(full_data_test_3)[names(full_data_test_3) == "value"] <- "value_7"
full_data_train_3_mat <- model.matrix( ~ ., full_data_train_3[,-50])
full_data_test_3_mat <- model.matrix( ~ ., full_data_test_3[,-50])
train_target=as.matrix(full_data_train_3[,50])
fit_glmnet_7=cv.glmnet(full_data_train_3_mat,train_target,type.measure='mse', trace.it = 1)
predicted_val_7=predict(fit_glmnet_7,s='lambda.min',full_data_test_3_mat)
part_d_matrix$value_hour_7=data.table(full_data_test_3[,value_7])
part_d_matrix$predicted_val_7=data.table(predicted_val_7)
part_d_matrix$hour_7_MAPE=with(full_data_test_3,abs((value_7-predicted_val_7)/value_7)*100)
part_d_matrix$hour_7_overallMAPE=with(part_d_matrix,sum(part_d_matrix$hour_7_MAPE)/nrow(part_d_matrix))

#hour8
wide_predictor_d_2=dcast(full_data,date~paste0('lag_hours_48_',hour),value.var='lag_48')
wide_predictor_d_3=dcast(full_data,date~paste0('lag_hours_168_',hour),value.var='lag_168')

target8=full_data[hour==8]
final_feature_set=merge(wide_predictor_d_2,wide_predictor_d_3,by='date')
final_feature_set_1=merge(final_feature_set,target1[,list(date,value)],by='date')
full_data_train_3 <- final_feature_set_1 %>%filter(date >= "2016-01-01" & date <= "2020-03-01")
full_data_test_3 <- final_feature_set_1 %>%filter(date >= "2020-03-02" & date <= "2020-05-19")
names(full_data_test_3)[names(full_data_test_3) == "value"] <- "value_8"
full_data_train_3_mat <- model.matrix( ~ ., full_data_train_3[,-50])
full_data_test_3_mat <- model.matrix( ~ ., full_data_test_3[,-50])
train_target=as.matrix(full_data_train_3[,50])
fit_glmnet_8=cv.glmnet(full_data_train_3_mat,train_target,type.measure='mse', trace.it = 1)
predicted_val_8=predict(fit_glmnet_8,s='lambda.min',full_data_test_3_mat)
part_d_matrix$value_hour_8=data.table(full_data_test_3[,value_8])
part_d_matrix$predicted_val_8=data.table(predicted_val_8)
part_d_matrix$hour_8_MAPE=with(full_data_test_3,abs((value_8-predicted_val_8)/value_8)*100)
part_d_matrix$hour_8_overallMAPE=with(part_d_matrix,sum(part_d_matrix$hour_8_MAPE)/nrow(part_d_matrix))

#hour9
wide_predictor_d_2=dcast(full_data,date~paste0('lag_hours_48_',hour),value.var='lag_48')
wide_predictor_d_3=dcast(full_data,date~paste0('lag_hours_168_',hour),value.var='lag_168')

target9=full_data[hour==9]
final_feature_set=merge(wide_predictor_d_2,wide_predictor_d_3,by='date')
final_feature_set_1=merge(final_feature_set,target1[,list(date,value)],by='date')
full_data_train_3 <- final_feature_set_1 %>%filter(date >= "2016-01-01" & date <= "2020-03-01")
full_data_test_3 <- final_feature_set_1 %>%filter(date >= "2020-03-02" & date <= "2020-05-19")
names(full_data_test_3)[names(full_data_test_3) == "value"] <- "value_9"
full_data_train_3_mat <- model.matrix( ~ ., full_data_train_3[,-50])
full_data_test_3_mat <- model.matrix( ~ ., full_data_test_3[,-50])
train_target=as.matrix(full_data_train_3[,50])
fit_glmnet_9=cv.glmnet(full_data_train_3_mat,train_target,type.measure='mse', trace.it = 1)
predicted_val_9=predict(fit_glmnet_9,s='lambda.min',full_data_test_3_mat)
part_d_matrix$value_hour_9=data.table(full_data_test_3[,value_9])
part_d_matrix$predicted_val_9=data.table(predicted_val_9)
part_d_matrix$hour_9_MAPE=with(full_data_test_3,abs((value_9-predicted_val_9)/value_9)*100)
part_d_matrix$hour_9_overallMAPE=with(part_d_matrix,sum(part_d_matrix$hour_9_MAPE)/nrow(part_d_matrix))

#hour10
wide_predictor_d_2=dcast(full_data,date~paste0('lag_hours_48_',hour),value.var='lag_48')
wide_predictor_d_3=dcast(full_data,date~paste0('lag_hours_168_',hour),value.var='lag_168')

target10=full_data[hour==10]
final_feature_set=merge(wide_predictor_d_2,wide_predictor_d_3,by='date')
final_feature_set_1=merge(final_feature_set,target1[,list(date,value)],by='date')
full_data_train_3 <- final_feature_set_1 %>%filter(date >= "2016-01-01" & date <= "2020-03-01")
full_data_test_3 <- final_feature_set_1 %>%filter(date >= "2020-03-02" & date <= "2020-05-19")
names(full_data_test_3)[names(full_data_test_3) == "value"] <- "value_10"
full_data_train_3_mat <- model.matrix( ~ ., full_data_train_3[,-50])
full_data_test_3_mat <- model.matrix( ~ ., full_data_test_3[,-50])
train_target=as.matrix(full_data_train_3[,50])
fit_glmnet_10=cv.glmnet(full_data_train_3_mat,train_target,type.measure='mse', trace.it = 1)
predicted_val_10=predict(fit_glmnet_10,s='lambda.min',full_data_test_3_mat)
part_d_matrix$value_hour_10=data.table(full_data_test_3[,value_10])
part_d_matrix$predicted_val_10=data.table(predicted_val_10)
part_d_matrix$hour_10_MAPE=with(full_data_test_3,abs((value_10-predicted_val_10)/value_10)*100)
part_d_matrix$hour_10_overallMAPE=with(part_d_matrix,sum(part_d_matrix$hour_10_MAPE)/nrow(part_d_matrix))

#hour11
wide_predictor_d_2=dcast(full_data,date~paste0('lag_hours_48_',hour),value.var='lag_48')
wide_predictor_d_3=dcast(full_data,date~paste0('lag_hours_168_',hour),value.var='lag_168')

target11=full_data[hour==11]
final_feature_set=merge(wide_predictor_d_2,wide_predictor_d_3,by='date')
final_feature_set_1=merge(final_feature_set,target1[,list(date,value)],by='date')
full_data_train_3 <- final_feature_set_1 %>%filter(date >= "2016-01-01" & date <= "2020-03-01")
full_data_test_3 <- final_feature_set_1 %>%filter(date >= "2020-03-02" & date <= "2020-05-19")
names(full_data_test_3)[names(full_data_test_3) == "value"] <- "value_11"
full_data_train_3_mat <- model.matrix( ~ ., full_data_train_3[,-50])
full_data_test_3_mat <- model.matrix( ~ ., full_data_test_3[,-50])
train_target=as.matrix(full_data_train_3[,50])
fit_glmnet_11=cv.glmnet(full_data_train_3_mat,train_target,type.measure='mse', trace.it = 1)
predicted_val_11=predict(fit_glmnet_11,s='lambda.min',full_data_test_3_mat)
part_d_matrix$value_hour_11=data.table(full_data_test_3[,value_11])
part_d_matrix$predicted_val_11=data.table(predicted_val_11)
part_d_matrix$hour_11_MAPE=with(full_data_test_3,abs((value_11-predicted_val_11)/value_11)*100)
part_d_matrix$hour_11_overallMAPE=with(part_d_matrix,sum(part_d_matrix$hour_11_MAPE)/nrow(part_d_matrix))

#hour12
wide_predictor_d_2=dcast(full_data,date~paste0('lag_hours_48_',hour),value.var='lag_48')
wide_predictor_d_3=dcast(full_data,date~paste0('lag_hours_168_',hour),value.var='lag_168')

target12=full_data[hour==12]
final_feature_set=merge(wide_predictor_d_2,wide_predictor_d_3,by='date')
final_feature_set_1=merge(final_feature_set,target1[,list(date,value)],by='date')
full_data_train_3 <- final_feature_set_1 %>%filter(date >= "2016-01-01" & date <= "2020-03-01")
full_data_test_3 <- final_feature_set_1 %>%filter(date >= "2020-03-02" & date <= "2020-05-19")
names(full_data_test_3)[names(full_data_test_3) == "value"] <- "value_12"
full_data_train_3_mat <- model.matrix( ~ ., full_data_train_3[,-50])
full_data_test_3_mat <- model.matrix( ~ ., full_data_test_3[,-50])
train_target=as.matrix(full_data_train_3[,50])
fit_glmnet_12=cv.glmnet(full_data_train_3_mat,train_target,type.measure='mse', trace.it = 1)
predicted_val_12=predict(fit_glmnet_12,s='lambda.min',full_data_test_3_mat)
part_d_matrix$value_hour_12=data.table(full_data_test_3[,value_12])
part_d_matrix$predicted_val_12=data.table(predicted_val_12)
part_d_matrix$hour_12_MAPE=with(full_data_test_3,abs((value_12-predicted_val_12)/value_12)*100)
part_d_matrix$hour_12_overallMAPE=with(part_d_matrix,sum(part_d_matrix$hour_12_MAPE)/nrow(part_d_matrix))

#hour13
wide_predictor_d_2=dcast(full_data,date~paste0('lag_hours_48_',hour),value.var='lag_48')
wide_predictor_d_3=dcast(full_data,date~paste0('lag_hours_168_',hour),value.var='lag_168')

target1=full_data[hour==13]
final_feature_set=merge(wide_predictor_d_2,wide_predictor_d_3,by='date')
final_feature_set_1=merge(final_feature_set,target1[,list(date,value)],by='date')
full_data_train_3 <- final_feature_set_1 %>%filter(date >= "2016-01-01" & date <= "2020-03-01")
full_data_test_3 <- final_feature_set_1 %>%filter(date >= "2020-03-02" & date <= "2020-05-19")
names(full_data_test_3)[names(full_data_test_3) == "value"] <- "value_13"
full_data_train_3_mat <- model.matrix( ~ ., full_data_train_3[,-50])
full_data_test_3_mat <- model.matrix( ~ ., full_data_test_3[,-50])
train_target=as.matrix(full_data_train_3[,50])
fit_glmnet_13=cv.glmnet(full_data_train_3_mat,train_target,type.measure='mse', trace.it = 1)
predicted_val_13=predict(fit_glmnet_13,s='lambda.min',full_data_test_3_mat)
part_d_matrix$value_hour_13=data.table(full_data_test_3[,value_13])
part_d_matrix$predicted_val_13=data.table(predicted_val_13)
part_d_matrix$hour_13_MAPE=with(full_data_test_3,abs((value_13-predicted_val_13)/value_13)*100)
part_d_matrix$hour_13_overallMAPE=with(part_d_matrix,sum(part_d_matrix$hour_13_MAPE)/nrow(part_d_matrix))

#hour14
wide_predictor_d_2=dcast(full_data,date~paste0('lag_hours_48_',hour),value.var='lag_48')
wide_predictor_d_3=dcast(full_data,date~paste0('lag_hours_168_',hour),value.var='lag_168')

target1=full_data[hour==14]
final_feature_set=merge(wide_predictor_d_2,wide_predictor_d_3,by='date')
final_feature_set_1=merge(final_feature_set,target1[,list(date,value)],by='date')
full_data_train_3 <- final_feature_set_1 %>%filter(date >= "2016-01-01" & date <= "2020-03-01")
full_data_test_3 <- final_feature_set_1 %>%filter(date >= "2020-03-02" & date <= "2020-05-19")
names(full_data_test_3)[names(full_data_test_3) == "value"] <- "value_14"
full_data_train_3_mat <- model.matrix( ~ ., full_data_train_3[,-50])
full_data_test_3_mat <- model.matrix( ~ ., full_data_test_3[,-50])
train_target=as.matrix(full_data_train_3[,50])
fit_glmnet_14=cv.glmnet(full_data_train_3_mat,train_target,type.measure='mse', trace.it = 1)
predicted_val_14=predict(fit_glmnet_14,s='lambda.min',full_data_test_3_mat)
part_d_matrix$value_hour_14=data.table(full_data_test_3[,value_14])
part_d_matrix$predicted_val_14=data.table(predicted_val_14)
part_d_matrix$hour_14_MAPE=with(full_data_test_3,abs((value_14-predicted_val_14)/value_14)*100)
part_d_matrix$hour_14_overallMAPE=with(part_d_matrix,sum(part_d_matrix$hour_14_MAPE)/nrow(part_d_matrix))

#hour15
wide_predictor_d_2=dcast(full_data,date~paste0('lag_hours_48_',hour),value.var='lag_48')
wide_predictor_d_3=dcast(full_data,date~paste0('lag_hours_168_',hour),value.var='lag_168')

target1=full_data[hour==15]
final_feature_set=merge(wide_predictor_d_2,wide_predictor_d_3,by='date')
final_feature_set_1=merge(final_feature_set,target1[,list(date,value)],by='date')
full_data_train_3 <- final_feature_set_1 %>%filter(date >= "2016-01-01" & date <= "2020-03-01")
full_data_test_3 <- final_feature_set_1 %>%filter(date >= "2020-03-02" & date <= "2020-05-19")
names(full_data_test_3)[names(full_data_test_3) == "value"] <- "value_15"
full_data_train_3_mat <- model.matrix( ~ ., full_data_train_3[,-50])
full_data_test_3_mat <- model.matrix( ~ ., full_data_test_3[,-50])
train_target=as.matrix(full_data_train_3[,50])
fit_glmnet_15=cv.glmnet(full_data_train_3_mat,train_target,type.measure='mse', trace.it = 1)
predicted_val_15=predict(fit_glmnet_15,s='lambda.min',full_data_test_3_mat)
part_d_matrix$value_hour_15=data.table(full_data_test_3[,value_15])
part_d_matrix$predicted_val_15=data.table(predicted_val_15)
part_d_matrix$hour_15_MAPE=with(full_data_test_3,abs((value_15-predicted_val_15)/value_15)*100)
part_d_matrix$hour_15_overallMAPE=with(part_d_matrix,sum(part_d_matrix$hour_15_MAPE)/nrow(part_d_matrix))

#hour16
wide_predictor_d_2=dcast(full_data,date~paste0('lag_hours_48_',hour),value.var='lag_48')
wide_predictor_d_3=dcast(full_data,date~paste0('lag_hours_168_',hour),value.var='lag_168')

target1=full_data[hour==16]
final_feature_set=merge(wide_predictor_d_2,wide_predictor_d_3,by='date')
final_feature_set_1=merge(final_feature_set,target1[,list(date,value)],by='date')
full_data_train_3 <- final_feature_set_1 %>%filter(date >= "2016-01-01" & date <= "2020-03-01")
full_data_test_3 <- final_feature_set_1 %>%filter(date >= "2020-03-02" & date <= "2020-05-19")
names(full_data_test_3)[names(full_data_test_3) == "value"] <- "value_16"
full_data_train_3_mat <- model.matrix( ~ ., full_data_train_3[,-50])
full_data_test_3_mat <- model.matrix( ~ ., full_data_test_3[,-50])
train_target=as.matrix(full_data_train_3[,50])
fit_glmnet_16=cv.glmnet(full_data_train_3_mat,train_target,type.measure='mse', trace.it = 1)
predicted_val_16=predict(fit_glmnet_16,s='lambda.min',full_data_test_3_mat)
part_d_matrix$value_hour_16=data.table(full_data_test_3[,value_16])
part_d_matrix$predicted_val_16=data.table(predicted_val_16)
part_d_matrix$hour_16_MAPE=with(full_data_test_3,abs((value_16-predicted_val_16)/value_16)*100)
part_d_matrix$hour_16_overallMAPE=with(part_d_matrix,sum(part_d_matrix$hour_16_MAPE)/nrow(part_d_matrix))

#hour17
wide_predictor_d_2=dcast(full_data,date~paste0('lag_hours_48_',hour),value.var='lag_48')
wide_predictor_d_3=dcast(full_data,date~paste0('lag_hours_168_',hour),value.var='lag_168')

target1=full_data[hour==17]
final_feature_set=merge(wide_predictor_d_2,wide_predictor_d_3,by='date')
final_feature_set_1=merge(final_feature_set,target1[,list(date,value)],by='date')
full_data_train_3 <- final_feature_set_1 %>%filter(date >= "2016-01-01" & date <= "2020-03-01")
full_data_test_3 <- final_feature_set_1 %>%filter(date >= "2020-03-02" & date <= "2020-05-19")
names(full_data_test_3)[names(full_data_test_3) == "value"] <- "value_17"
full_data_train_3_mat <- model.matrix( ~ ., full_data_train_3[,-50])
full_data_test_3_mat <- model.matrix( ~ ., full_data_test_3[,-50])
train_target=as.matrix(full_data_train_3[,50])
fit_glmnet_17=cv.glmnet(full_data_train_3_mat,train_target,type.measure='mse', trace.it = 1)
predicted_val_17=predict(fit_glmnet_17,s='lambda.min',full_data_test_3_mat)
part_d_matrix$value_hour_17=data.table(full_data_test_3[,value_17])
part_d_matrix$predicted_val_17=data.table(predicted_val_17)
part_d_matrix$hour_17_MAPE=with(full_data_test_3,abs((value_17-predicted_val_17)/value_17)*100)
part_d_matrix$hour_17_overallMAPE=with(part_d_matrix,sum(part_d_matrix$hour_17_MAPE)/nrow(part_d_matrix))

#hour18
wide_predictor_d_2=dcast(full_data,date~paste0('lag_hours_48_',hour),value.var='lag_48')
wide_predictor_d_3=dcast(full_data,date~paste0('lag_hours_168_',hour),value.var='lag_168')

target1=full_data[hour==18]
final_feature_set=merge(wide_predictor_d_2,wide_predictor_d_3,by='date')
final_feature_set_1=merge(final_feature_set,target1[,list(date,value)],by='date')
full_data_train_3 <- final_feature_set_1 %>%filter(date >= "2016-01-01" & date <= "2020-03-01")
full_data_test_3 <- final_feature_set_1 %>%filter(date >= "2020-03-02" & date <= "2020-05-19")
names(full_data_test_3)[names(full_data_test_3) == "value"] <- "value_18"
full_data_train_3_mat <- model.matrix( ~ ., full_data_train_3[,-50])
full_data_test_3_mat <- model.matrix( ~ ., full_data_test_3[,-50])
train_target=as.matrix(full_data_train_3[,50])
fit_glmnet_18=cv.glmnet(full_data_train_3_mat,train_target,type.measure='mse', trace.it = 1)
predicted_val_18=predict(fit_glmnet_18,s='lambda.min',full_data_test_3_mat)
part_d_matrix$value_hour_18=data.table(full_data_test_3[,value_18])
part_d_matrix$predicted_val_18=data.table(predicted_val_18)
part_d_matrix$hour_18_MAPE=with(full_data_test_3,abs((value_18-predicted_val_18)/value_18)*100)
part_d_matrix$hour_18_overallMAPE=with(part_d_matrix,sum(part_d_matrix$hour_18_MAPE)/nrow(part_d_matrix))

#hour19
wide_predictor_d_2=dcast(full_data,date~paste0('lag_hours_48_',hour),value.var='lag_48')
wide_predictor_d_3=dcast(full_data,date~paste0('lag_hours_168_',hour),value.var='lag_168')

target1=full_data[hour==19]
final_feature_set=merge(wide_predictor_d_2,wide_predictor_d_3,by='date')
final_feature_set_1=merge(final_feature_set,target1[,list(date,value)],by='date')
full_data_train_3 <- final_feature_set_1 %>%filter(date >= "2016-01-01" & date <= "2020-03-01")
full_data_test_3 <- final_feature_set_1 %>%filter(date >= "2020-03-02" & date <= "2020-05-19")
names(full_data_test_3)[names(full_data_test_3) == "value"] <- "value_19"
full_data_train_3_mat <- model.matrix( ~ ., full_data_train_3[,-50])
full_data_test_3_mat <- model.matrix( ~ ., full_data_test_3[,-50])
train_target=as.matrix(full_data_train_3[,50])
fit_glmnet_19=cv.glmnet(full_data_train_3_mat,train_target,type.measure='mse', trace.it = 1)
predicted_val_19=predict(fit_glmnet_19,s='lambda.min',full_data_test_3_mat)
part_d_matrix$value_hour_19=data.table(full_data_test_3[,value_19])
part_d_matrix$predicted_val_19=data.table(predicted_val_19)
part_d_matrix$hour_19_MAPE=with(full_data_test_3,abs((value_19-predicted_val_19)/value_19)*100)
part_d_matrix$hour_19_overallMAPE=with(part_d_matrix,sum(part_d_matrix$hour_19_MAPE)/nrow(part_d_matrix))

#hour20
wide_predictor_d_2=dcast(full_data,date~paste0('lag_hours_48_',hour),value.var='lag_48')
wide_predictor_d_3=dcast(full_data,date~paste0('lag_hours_168_',hour),value.var='lag_168')

target1=full_data[hour==20]
final_feature_set=merge(wide_predictor_d_2,wide_predictor_d_3,by='date')
final_feature_set_1=merge(final_feature_set,target1[,list(date,value)],by='date')
full_data_train_3 <- final_feature_set_1 %>%filter(date >= "2016-01-01" & date <= "2020-03-01")
full_data_test_3 <- final_feature_set_1 %>%filter(date >= "2020-03-02" & date <= "2020-05-19")
names(full_data_test_3)[names(full_data_test_3) == "value"] <- "value_20"
full_data_train_3_mat <- model.matrix( ~ ., full_data_train_3[,-50])
full_data_test_3_mat <- model.matrix( ~ ., full_data_test_3[,-50])
train_target=as.matrix(full_data_train_3[,50])
fit_glmnet_20=cv.glmnet(full_data_train_3_mat,train_target,type.measure='mse', trace.it = 1)
predicted_val_20=predict(fit_glmnet_20,s='lambda.min',full_data_test_3_mat)
part_d_matrix$value_hour_20=data.table(full_data_test_3[,value_20])
part_d_matrix$predicted_val_20=data.table(predicted_val_20)
part_d_matrix$hour_20_MAPE=with(full_data_test_3,abs((value_20-predicted_val_20)/value_20)*100)
part_d_matrix$hour_20_overallMAPE=with(part_d_matrix,sum(part_d_matrix$hour_20_MAPE)/nrow(part_d_matrix))

#hour21
wide_predictor_d_2=dcast(full_data,date~paste0('lag_hours_48_',hour),value.var='lag_48')
wide_predictor_d_3=dcast(full_data,date~paste0('lag_hours_168_',hour),value.var='lag_168')

target1=full_data[hour==21]
final_feature_set=merge(wide_predictor_d_2,wide_predictor_d_3,by='date')
final_feature_set_1=merge(final_feature_set,target1[,list(date,value)],by='date')
full_data_train_3 <- final_feature_set_1 %>%filter(date >= "2016-01-01" & date <= "2020-03-01")
full_data_test_3 <- final_feature_set_1 %>%filter(date >= "2020-03-02" & date <= "2020-05-19")
names(full_data_test_3)[names(full_data_test_3) == "value"] <- "value_21"
full_data_train_3_mat <- model.matrix( ~ ., full_data_train_3[,-50])
full_data_test_3_mat <- model.matrix( ~ ., full_data_test_3[,-50])
train_target=as.matrix(full_data_train_3[,50])
fit_glmnet_21=cv.glmnet(full_data_train_3_mat,train_target,type.measure='mse', trace.it = 1)
predicted_val_21=predict(fit_glmnet_21,s='lambda.min',full_data_test_3_mat)
part_d_matrix$value_hour_21=data.table(full_data_test_3[,value_21])
part_d_matrix$predicted_val_21=data.table(predicted_val_21)
part_d_matrix$hour_21_MAPE=with(full_data_test_3,abs((value_21-predicted_val_21)/value_21)*100)
part_d_matrix$hour_21_overallMAPE=with(part_d_matrix,sum(part_d_matrix$hour_21_MAPE)/nrow(part_d_matrix))
#hour22
wide_predictor_d_2=dcast(full_data,date~paste0('lag_hours_48_',hour),value.var='lag_48')
wide_predictor_d_3=dcast(full_data,date~paste0('lag_hours_168_',hour),value.var='lag_168')

target1=full_data[hour==22]
final_feature_set=merge(wide_predictor_d_2,wide_predictor_d_3,by='date')
final_feature_set_1=merge(final_feature_set,target1[,list(date,value)],by='date')
full_data_train_3 <- final_feature_set_1 %>%filter(date >= "2016-01-01" & date <= "2020-03-01")
full_data_test_3 <- final_feature_set_1 %>%filter(date >= "2020-03-02" & date <= "2020-05-19")
names(full_data_test_3)[names(full_data_test_3) == "value"] <- "value_22"
full_data_train_3_mat <- model.matrix( ~ ., full_data_train_3[,-50])
full_data_test_3_mat <- model.matrix( ~ ., full_data_test_3[,-50])
train_target=as.matrix(full_data_train_3[,50])
fit_glmnet_22=cv.glmnet(full_data_train_3_mat,train_target,type.measure='mse', trace.it = 1)
predicted_val_22=predict(fit_glmnet_22,s='lambda.min',full_data_test_3_mat)
part_d_matrix$value_hour_22=data.table(full_data_test_3[,value_22])
part_d_matrix$predicted_val_22=data.table(predicted_val_22)
part_d_matrix$hour_22_MAPE=with(full_data_test_3,abs((value_22-predicted_val_22)/value_22)*100)
part_d_matrix$hour_22_overallMAPE=with(part_d_matrix,sum(part_d_matrix$hour_22_MAPE)/nrow(part_d_matrix))


#hour23
wide_predictor_d_2=dcast(full_data,date~paste0('lag_hours_48_',hour),value.var='lag_48')
wide_predictor_d_3=dcast(full_data,date~paste0('lag_hours_168_',hour),value.var='lag_168')

target1=full_data[hour==23]
final_feature_set=merge(wide_predictor_d_2,wide_predictor_d_3,by='date')
final_feature_set_1=merge(final_feature_set,target1[,list(date,value)],by='date')
full_data_train_3 <- final_feature_set_1 %>%filter(date >= "2016-01-01" & date <= "2020-03-01")
full_data_test_3 <- final_feature_set_1 %>%filter(date >= "2020-03-02" & date <= "2020-05-19")
names(full_data_test_3)[names(full_data_test_3) == "value"] <- "value_23"
full_data_train_3_mat <- model.matrix( ~ ., full_data_train_3[,-50])
full_data_test_3_mat <- model.matrix( ~ ., full_data_test_3[,-50])
train_target=as.matrix(full_data_train_3[,50])
fit_glmnet_23=cv.glmnet(full_data_train_3_mat,train_target,type.measure='mse', trace.it = 1)
predicted_val_23=predict(fit_glmnet_23,s='lambda.min',full_data_test_3_mat)
part_d_matrix$value_hour_23=data.table(full_data_test_3[,value_23])
part_d_matrix$predicted_val_23=data.table(predicted_val_23)
part_d_matrix$hour_23_MAPE=with(full_data_test_3,abs((value_23-predicted_val_23)/value_23)*100)
part_d_matrix$hour_23_overallMAPE=with(part_d_matrix,sum(part_d_matrix$hour_23_MAPE)/nrow(part_d_matrix))