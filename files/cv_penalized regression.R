require(data.table)
require(dplyr)
require(tidyr)
require(glmnet)
require(ggplot2)
set.seed(100)
trendyol=fread("/Users/ayhanertuglu/Desktop/Okul/II.Dönem/Business Analytics/Project/Data/Boun Content Challenge - Daily Actions(04-07-2020).csv")
trendyol[,date:=as.Date(event_date,'%Y-%m-%d')]
str(trendyol)
trendyol$sold_count=as.numeric(trendyol$sold_count)
trendyol <- trendyol[order(date,product_content_id),]
trendyol <- trendyol[,-2]
#85004
target_85044=data.table(trendyol[product_content_id==85004])
target_85044_1= target_85044 %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))          
target_85044_1$date=target_85044$date
full_data_train_85004 <- target_85044_1 %>%filter(date >= "2019-05-13" & date < "2020-03-01")
full_data_test_85004 <- target_85044_1 %>%filter(date >= "2020-03-01" & date <= "2020-07-04")
full_data_train_85004_mat <- model.matrix( ~ ., full_data_train_85004[,-3])
full_data_test_85004_mat <- model.matrix( ~ ., full_data_test_85004[,-3])
train_target=as.matrix(full_data_train_85004[,3])
fit_glmnet_85004=cv.glmnet(full_data_train_85004_mat,train_target,type.measure='mse', trace.it = 1)
fit_glmnet_85004
predicted_85004=predict(fit_glmnet_85004,s='lambda.min',full_data_test_85004_mat)
part_d_matrix=data.table(full_data_test_85004[,sold_count])
names(part_d_matrix)[names(part_d_matrix) == "V1"] <- "sold_count_85004"
part_d_matrix$sold_count_85004=data.table(full_data_test_85004[,sold_count])
part_d_matrix$predicted_85004=data.table(predicted_85004)
part_d_matrix$predicted_85004_MAPE=with(full_data_test_85004,abs((sold_count-predicted_85004)/sold_count)*100)
is.na(part_d_matrix$predicted_85004_MAPE)<-sapply(part_d_matrix$predicted_85004_MAPE, is.infinite)
part_d_matrix$predicted_85004_MAPE[is.na(part_d_matrix$predicted_85004_MAPE)]<-mean(complete.cases(part_d_matrix$predicted_85004_MAPE))
part_d_matrix$predicted_85004_overallMAPE=with(part_d_matrix,sum(part_d_matrix$predicted_85004_MAPE)/nrow(part_d_matrix))
part_d_matrix$date=full_data_test_85004$date
sse=sum((full_data_test_85004$sold_count-predicted_85004)^2)
mse=sse/nrow(part_d_matrix)
#31515569
target_31515569=data.table(trendyol[product_content_id==31515569])
target_31515569_1= target_31515569 %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))          
target_31515569_1$date=target_31515569$date
full_data_train_31515569 <- target_31515569_1 %>%filter(date >= "2019-05-13" & date < "2020-03-01")
full_data_test_31515569 <- target_31515569_1 %>%filter(date >= "2020-03-01" & date <= "2020-07-04")
full_data_train_31515569_mat <- model.matrix( ~ ., full_data_train_31515569[,-3])
full_data_test_31515569_mat <- model.matrix( ~ ., full_data_test_31515569[,-3])
train_target=as.matrix(full_data_train_31515569[,3])
fit_glmnet_31515569=cv.glmnet(full_data_train_31515569_mat,train_target,type.measure='mse', trace.it = 1)
predicted_31515569=predict(fit_glmnet_31515569,s='lambda.min',full_data_test_31515569_mat)
part_d_matrix$sold_count_31515569=data.table(full_data_test_31515569[,sold_count])
part_d_matrix$predicted_31515569=data.table(predicted_31515569)
part_d_matrix$predicted_31515569_MAPE=with(full_data_test_31515569,abs((sold_count-predicted_31515569)/sold_count)*100)
is.na(part_d_matrix$predicted_31515569_MAPE)<-sapply(part_d_matrix$predicted_31515569_MAPE, is.infinite)
part_d_matrix$predicted_31515569_MAPE[is.na(part_d_matrix$predicted_31515569_MAPE)]<-mean(complete.cases(part_d_matrix$predicted_31515569_MAPE))
part_d_matrix$predicted_31515569_overallMAPE=with(part_d_matrix,sum(part_d_matrix$predicted_31515569_MAPE)/nrow(part_d_matrix))
part_d_matrix$date=full_data_test_31515569$date
sse=sum((full_data_test_31515569$sold_count-predicted_31515569)^2)
mse=sse/nrow(part_d_matrix)
#7061886
target_7061886=data.table(trendyol[product_content_id==7061886])
target_7061886_1= target_7061886 %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))          
target_7061886_1$date=target_7061886$date
full_data_train_7061886 <- target_7061886_1 %>%filter(date >= "2019-05-13" & date < "2020-03-01")
full_data_test_7061886 <- target_7061886_1 %>%filter(date >= "2020-03-01" & date <= "2020-07-04")
full_data_train_7061886_mat <- model.matrix( ~ ., full_data_train_7061886[,-3])
full_data_test_7061886_mat <- model.matrix( ~ ., full_data_test_7061886[,-3])
train_target=as.matrix(full_data_train_7061886[,3])
fit_glmnet_7061886=cv.glmnet(full_data_train_7061886_mat,train_target,type.measure='mse', trace.it = 1)
predicted_7061886=predict(fit_glmnet_7061886,s='lambda.min',full_data_test_7061886_mat)
part_d_matrix$sold_count_7061886=data.table(full_data_test_7061886[,sold_count])
part_d_matrix$predicted_7061886=data.table(predicted_7061886)
part_d_matrix$predicted_7061886_MAPE=with(full_data_test_7061886,abs((sold_count-predicted_7061886)/sold_count)*100)
is.na(part_d_matrix$predicted_7061886_MAPE)<-sapply(part_d_matrix$predicted_7061886_MAPE, is.infinite)
part_d_matrix$predicted_7061886_MAPE[is.na(part_d_matrix$predicted_7061886_MAPE)]<-mean(complete.cases(part_d_matrix$predicted_7061886_MAPE))
part_d_matrix$predicted_7061886_overallMAPE=with(part_d_matrix,sum(part_d_matrix$predicted_7061886_MAPE)/nrow(part_d_matrix))
part_d_matrix$date=full_data_test_7061886$date
sse=sum((full_data_test_7061886$sold_count-predicted_7061886)^2)
mse=sse/nrow(part_d_matrix)
#6676673
target_6676673=data.table(trendyol[product_content_id==6676673])
target_6676673_1= target_6676673 %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))          
target_6676673_1$date=target_6676673$date
full_data_train_6676673 <- target_6676673_1 %>%filter(date >= "2019-05-13" & date < "2020-03-01")
full_data_test_6676673 <- target_6676673_1 %>%filter(date >= "2020-03-01" & date <= "2020-07-04")
full_data_train_6676673_mat <- model.matrix( ~ ., full_data_train_6676673[,-3])
full_data_test_6676673_mat <- model.matrix( ~ ., full_data_test_6676673[,-3])
train_target=as.matrix(full_data_train_6676673[,3])
fit_glmnet_6676673=cv.glmnet(full_data_train_6676673_mat,train_target,type.measure='mse', trace.it = 1)
predicted_6676673=predict(fit_glmnet_6676673,s='lambda.min',full_data_test_6676673_mat)
part_d_matrix$sold_count_6676673=data.table(full_data_test_6676673[,sold_count])
part_d_matrix$predicted_6676673=data.table(predicted_6676673)
part_d_matrix$predicted_6676673_MAPE=with(full_data_test_6676673,abs((sold_count-predicted_6676673)/sold_count)*100)
is.na(part_d_matrix$predicted_6676673_MAPE)<-sapply(part_d_matrix$predicted_6676673_MAPE, is.infinite)
part_d_matrix$predicted_6676673_MAPE[is.na(part_d_matrix$predicted_6676673_MAPE)]<-mean(complete.cases(part_d_matrix$predicted_6676673_MAPE))
part_d_matrix$predicted_6676673_overallMAPE=with(part_d_matrix,sum(part_d_matrix$predicted_6676673_MAPE)/nrow(part_d_matrix))
part_d_matrix$date=full_data_test_6676673$date
sse=sum((full_data_test_6676673$sold_count-predicted_6676673)^2)
mse=sse/nrow(part_d_matrix)
#5926527
target_5926527=data.table(trendyol[product_content_id==5926527])
target_5926527_1= target_5926527 %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))          
target_5926527_1$date=target_5926527$date
full_data_train_5926527 <- target_5926527_1 %>%filter(date >= "2019-05-13" & date < "2020-03-01")
full_data_test_5926527 <- target_5926527_1 %>%filter(date >= "2020-03-01" & date <= "2020-07-04")
full_data_train_5926527_mat <- model.matrix( ~ ., full_data_train_5926527[,-3])
full_data_test_5926527_mat <- model.matrix( ~ ., full_data_test_5926527[,-3])
train_target=as.matrix(full_data_train_5926527[,3])
fit_glmnet_5926527=cv.glmnet(full_data_train_5926527_mat,train_target,type.measure='mse', trace.it = 1)
predicted_5926527=predict(fit_glmnet_5926527,s='lambda.min',full_data_test_5926527_mat)
part_d_matrix$sold_count_5926527=data.table(full_data_test_5926527[,sold_count])
part_d_matrix$predicted_5926527=data.table(predicted_5926527)
part_d_matrix$predicted_5926527_MAPE=with(full_data_test_5926527,abs((sold_count-predicted_5926527)/sold_count)*100)
is.na(part_d_matrix$predicted_5926527_MAPE)<-sapply(part_d_matrix$predicted_5926527_MAPE, is.infinite)
part_d_matrix$predicted_5926527_MAPE[is.na(part_d_matrix$predicted_5926527_MAPE)]<-mean(complete.cases(part_d_matrix$predicted_5926527_MAPE))
part_d_matrix$predicted_5926527_overallMAPE=with(part_d_matrix,sum(part_d_matrix$predicted_5926527_MAPE)/nrow(part_d_matrix))
part_d_matrix$date=full_data_test_5926527$date
sse=sum((full_data_test_5926527$sold_count-predicted_5926527)^2)
mse=sse/nrow(part_d_matrix)
#3904356
target_3904356=data.table(trendyol[product_content_id==3904356])
target_3904356_1= target_3904356 %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))          
target_3904356_1$date=target_3904356$date
full_data_train_3904356 <- target_3904356_1 %>%filter(date >= "2019-05-13" & date < "2020-03-01")
full_data_test_3904356 <- target_3904356_1 %>%filter(date >= "2020-03-01" & date <= "2020-07-04")
full_data_train_3904356_mat <- model.matrix( ~ ., full_data_train_3904356[,-3])
full_data_test_3904356_mat <- model.matrix( ~ ., full_data_test_3904356[,-3])
train_target=as.matrix(full_data_train_3904356[,3])
fit_glmnet_3904356=cv.glmnet(full_data_train_3904356_mat,train_target,type.measure='mse', trace.it = 1)
predicted_3904356=predict(fit_glmnet_3904356,s='lambda.min',full_data_test_3904356_mat)
part_d_matrix$sold_count_3904356=data.table(full_data_test_3904356[,sold_count])
part_d_matrix$predicted_3904356=data.table(predicted_3904356)
part_d_matrix$predicted_3904356_MAPE=with(full_data_test_3904356,abs((sold_count-predicted_3904356)/sold_count)*100)
is.na(part_d_matrix$predicted_3904356_MAPE)<-sapply(part_d_matrix$predicted_3904356_MAPE, is.infinite)
part_d_matrix$predicted_3904356_MAPE[is.na(part_d_matrix$predicted_3904356_MAPE)]<-mean(complete.cases(part_d_matrix$predicted_3904356_MAPE))
part_d_matrix$predicted_3904356_overallMAPE=with(part_d_matrix,sum(part_d_matrix$predicted_3904356_MAPE)/nrow(part_d_matrix))
part_d_matrix$date=full_data_test_3904356$date
sse=sum((full_data_test_3904356$sold_count-predicted_3904356)^2)
mse=sse/nrow(part_d_matrix)
#4066298
target_4066298=data.table(trendyol[product_content_id==4066298])
target_4066298_1= target_4066298 %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))          
target_4066298_1$date=target_4066298$date
full_data_train_4066298 <- target_4066298_1 %>%filter(date >= "2019-05-13" & date < "2020-03-01")
full_data_test_4066298 <- target_4066298_1 %>%filter(date >= "2020-03-01" & date <= "2020-07-04")
full_data_train_4066298_mat <- model.matrix( ~ ., full_data_train_4066298[,-3])
full_data_test_4066298_mat <- model.matrix( ~ ., full_data_test_4066298[,-3])
train_target=as.matrix(full_data_train_4066298[,3])
fit_glmnet_4066298=cv.glmnet(full_data_train_4066298_mat,train_target,type.measure='mse', trace.it = 1)
predicted_4066298=predict(fit_glmnet_4066298,s='lambda.min',full_data_test_4066298_mat)
part_d_matrix$sold_count_4066298=data.table(full_data_test_4066298[,sold_count])
part_d_matrix$predicted_4066298=data.table(predicted_4066298)
part_d_matrix$predicted_4066298_MAPE=with(full_data_test_4066298,abs((sold_count-predicted_4066298)/sold_count)*100)
is.na(part_d_matrix$predicted_4066298_MAPE)<-sapply(part_d_matrix$predicted_4066298_MAPE, is.infinite)
part_d_matrix$predicted_4066298_MAPE[is.na(part_d_matrix$predicted_4066298_MAPE)]<-mean(complete.cases(part_d_matrix$predicted_4066298_MAPE))
part_d_matrix$predicted_4066298_overallMAPE=with(part_d_matrix,sum(part_d_matrix$predicted_4066298_MAPE)/nrow(part_d_matrix))
part_d_matrix$date=full_data_test_4066298$date
sse=sum((full_data_test_4066298$sold_count-predicted_4066298)^2)
mse=sse/nrow(part_d_matrix)
#32939029
target_32939029=data.table(trendyol[product_content_id==32939029])
target_32939029_1= target_32939029 %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))          
target_32939029_1$date=target_32939029$date
full_data_train_32939029 <- target_32939029_1 %>%filter(date >= "2019-05-13" & date < "2020-03-01")
full_data_test_32939029 <- target_32939029_1 %>%filter(date >= "2020-03-01" & date <= "2020-07-04")
full_data_train_32939029_mat <- model.matrix( ~ ., full_data_train_32939029[,-3])
full_data_test_32939029_mat <- model.matrix( ~ ., full_data_test_32939029[,-3])
train_target=as.matrix(full_data_train_32939029[,3])
fit_glmnet_32939029=cv.glmnet(full_data_train_32939029_mat,train_target,type.measure='mse', trace.it = 1)
predicted_32939029=predict(fit_glmnet_32939029,s='lambda.min',full_data_test_32939029_mat)
part_d_matrix$sold_count_32939029=data.table(full_data_test_32939029[,sold_count])
part_d_matrix$predicted_32939029=data.table(predicted_32939029)
part_d_matrix$predicted_32939029_MAPE=with(full_data_test_32939029,abs((sold_count-predicted_32939029)/sold_count)*100)
is.na(part_d_matrix$predicted_32939029_MAPE)<-sapply(part_d_matrix$predicted_32939029_MAPE, is.infinite)
part_d_matrix$predicted_32939029_MAPE[is.na(part_d_matrix$predicted_32939029_MAPE)]<-mean(complete.cases(part_d_matrix$predicted_32939029_MAPE))
part_d_matrix$predicted_32939029_overallMAPE=with(part_d_matrix,sum(part_d_matrix$predicted_32939029_MAPE)/nrow(part_d_matrix))
part_d_matrix$date=full_data_test_32939029$date
sse=sum((full_data_test_32939029$sold_count-predicted_32939029)^2)
mse=sse/nrow(part_d_matrix)

sonuc=select(part_d_matrix,date,predicted_31515569,predicted_32939029,predicted_3904356,predicted_4066298,predicted_5926527,predicted_6676673,predicted_7061886,predicted_85004)
sonuc_6=select(part_d_matrix,predicted_85004_overallMAPE,predicted_31515569_overallMAPE,predicted_7061886_overallMAPE,predicted_6676673_overallMAPE,predicted_5926527_overallMAPE,predicted_3904356_overallMAPE,predicted_4066298_overallMAPE,predicted_32939029_overallMAPE)
mape_plot_4 = data.table(id= 85004, 31515569, 7061886, 6676673, 5926527, 3904356, 4066298, 32939029)
mape_plot_4 =transpose(mape_plot_4)
mape_plot_4$V1=as.character(mape_plot_4$V1)
CV_Penalized_Regression_MAPE=data.table(c("CV_Penalized_Regression_MAPE"))
mape_plot_4$V2=CV_Penalized_Regression_MAPE[1,]
mape_plot_4$V3=transpose(sonuc_6[1,])
mape_plot_sonnn=rbind(mape_plot_sonn,mape_plot_4)

ggplot(mape_plot_4, aes(V1, V3, colour=V2)) +
  geom_point(alpha = 0.5, size=4)+
  theme(axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
        panel.grid.major = element_line(size=0.5),
        panel.grid.minor = element_line(size=0.5),
        text = element_text(size = 15)) +
  labs(title= "MAPE Values for CV Penalized Regression",
       x= "Product ID",
       y= "MAPE") +
  geom_text(aes(label=sprintf("%1.1f", ..y..)),  position=position_nudge(y=4))