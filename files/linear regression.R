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
target_85004=data.table(trendyol[product_content_id==85004])
target_85004_1= target_85004 %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))          
target_85004_1$date=target_85004$date
full_data_train_85004 <- target_85004_1 %>%filter(date >= "2019-05-13" & date < "2020-03-01")
full_data_test_85004 <- target_85004_1 %>%filter(date >= "2020-03-01" & date <= "2020-07-04")
fit_lm_85004=glm(sold_count~.,data=full_data_train_85004)
str(fit_lm_85004)
summary(fit_lm_85004)
fit_lm_85004=glm(sold_count~price+favored_count+category_sold+category_brand_sold,data=full_data_train_85004,family = "gaussian")
predicted_85004=predict(fit_lm_85004,full_data_test_85004)
part_d_matrix=data.table(full_data_test_85004[,sold_count])
names(part_d_matrix)[names(part_d_matrix) == "V1"] <- "sold_count_85004"
part_d_matrix$sold_count_85004=data.table(full_data_test_85004[,sold_count])
part_d_matrix$predicted_85004=data.table(predicted_85004)
part_d_matrix$predicted_85004_MAPE=with(full_data_test_85004,abs((sold_count-predicted_85004)/sold_count)*100)
is.na(part_d_matrix$predicted_85004_MAPE)<-sapply(part_d_matrix$predicted_85004_MAPE, is.infinite)
part_d_matrix$predicted_85004_MAPE[is.na(part_d_matrix$predicted_85004_MAPE)]<-mean(complete.cases(part_d_matrix$predicted_85004_MAPE))
part_d_matrix$predicted_85004_overallMAPE=with(part_d_matrix,sum(part_d_matrix$predicted_85004_MAPE)/nrow(part_d_matrix))
sse=sum((full_data_test_85004$sold_count-predicted_85004)^2)
mse=sse/nrow(part_d_matrix)
part_d_matrix$date=full_data_test_85004$date

#31515569
target_31515569=data.table(trendyol[product_content_id==31515569])
target_31515569_1= target_31515569 %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))          
target_31515569_1$date=target_31515569$date
full_data_train_31515569 <- target_31515569_1 %>%filter(date >= "2019-05-13" & date < "2020-03-01")
full_data_test_31515569 <- target_31515569_1 %>%filter(date >= "2020-03-01" & date <= "2020-07-04")
fit_lm_31515569=glm(sold_count~.,data=full_data_train_31515569)
str(fit_lm_31515569)
summary(fit_lm_31515569)
fit_lm_31515569=glm(sold_count~-1+price+visit_count+category_sold+category_brand_sold,data=full_data_train_31515569,family = "gaussian")
predicted_31515569=predict(fit_lm_31515569,full_data_test_31515569)
part_d_matrix$sold_count_31515569=data.table(full_data_test_31515569[,sold_count])
part_d_matrix$predicted_31515569=data.table(predicted_31515569)
part_d_matrix$predicted_31515569_MAPE=with(full_data_test_31515569,abs((sold_count-predicted_31515569)/sold_count)*100)
is.na(part_d_matrix$predicted_31515569_MAPE)<-sapply(part_d_matrix$predicted_31515569_MAPE, is.infinite)
part_d_matrix$predicted_31515569_MAPE[is.na(part_d_matrix$predicted_31515569_MAPE)]<-mean(complete.cases(part_d_matrix$predicted_31515569_MAPE))
part_d_matrix$predicted_31515569_overallMAPE=with(part_d_matrix,sum(part_d_matrix$predicted_31515569_MAPE)/nrow(part_d_matrix))
sse=sum((full_data_test_31515569$sold_count-predicted_31515569)^2)
mse=sse/nrow(part_d_matrix)
part_d_matrix$date=full_data_test_31515569$date

#7061886
target_7061886=data.table(trendyol[product_content_id==7061886])
target_7061886_1= target_7061886 %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))          
target_7061886_1$date=target_7061886$date
full_data_train_7061886 <- target_7061886_1 %>%filter(date >= "2019-05-13" & date < "2020-03-01")
full_data_test_7061886 <- target_7061886_1 %>%filter(date >= "2020-03-01" & date <= "2020-07-04")
fit_lm_7061886=glm(sold_count~.,data=full_data_train_7061886)
str(fit_lm_7061886)
summary(fit_lm_7061886)
fit_lm_7061886=glm(sold_count~-1+visit_count+category_sold+category_brand_sold+category_visits+ty_visits,data=full_data_train_7061886,family = "gaussian")
predicted_7061886=predict(fit_lm_7061886,full_data_test_7061886)
part_d_matrix$sold_count_7061886=data.table(full_data_test_7061886[,sold_count])
part_d_matrix$predicted_7061886=data.table(predicted_7061886)
part_d_matrix$predicted_7061886_MAPE=with(full_data_test_7061886,abs((sold_count-predicted_7061886)/sold_count)*100)
is.na(part_d_matrix$predicted_7061886_MAPE)<-sapply(part_d_matrix$predicted_7061886_MAPE, is.infinite)
part_d_matrix$predicted_7061886_MAPE[is.na(part_d_matrix$predicted_7061886_MAPE)]<-mean(complete.cases(part_d_matrix$predicted_7061886_MAPE))
part_d_matrix$predicted_7061886_overallMAPE=with(part_d_matrix,sum(part_d_matrix$predicted_7061886_MAPE)/nrow(part_d_matrix))
sse=sum((full_data_test_7061886$sold_count-predicted_7061886)^2)
mse=sse/nrow(part_d_matrix)
part_d_matrix$date=full_data_test_7061886$date

#6676673
target_6676673=data.table(trendyol[product_content_id==6676673])
target_6676673_1= target_6676673 %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))          
target_6676673_1$date=target_6676673$date
full_data_train_6676673 <- target_6676673_1 %>%filter(date >= "2019-05-13" & date < "2020-03-01")
full_data_test_6676673 <- target_6676673_1 %>%filter(date >= "2020-03-01" & date <= "2020-07-04")
fit_lm_6676673=glm(sold_count~.,data=full_data_train_6676673)
str(fit_lm_6676673)
summary(fit_lm_6676673)
fit_lm_6676673=glm(sold_count~+price+basket_count+favored_count+category_sold+category_brand_sold+ty_visits+date,data=full_data_train_6676673,family = "gaussian")
predicted_6676673=predict(fit_lm_6676673,full_data_test_6676673)
part_d_matrix$sold_count_6676673=data.table(full_data_test_6676673[,sold_count])
part_d_matrix$predicted_6676673=data.table(predicted_6676673)
part_d_matrix$predicted_6676673_MAPE=with(full_data_test_6676673,abs((sold_count-predicted_6676673)/sold_count)*100)
is.na(part_d_matrix$predicted_6676673_MAPE)<-sapply(part_d_matrix$predicted_6676673_MAPE, is.infinite)
part_d_matrix$predicted_6676673_MAPE[is.na(part_d_matrix$predicted_6676673_MAPE)]<-mean(complete.cases(part_d_matrix$predicted_6676673_MAPE))
part_d_matrix$predicted_6676673_overallMAPE=with(part_d_matrix,sum(part_d_matrix$predicted_6676673_MAPE)/nrow(part_d_matrix))
sse=sum((full_data_test_6676673$sold_count-predicted_6676673)^2)
mse=sse/nrow(part_d_matrix)
part_d_matrix$date=full_data_test_6676673$date

#5926527
target_5926527=data.table(trendyol[product_content_id==5926527])
target_5926527_1= target_5926527 %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))          
target_5926527_1$date=target_5926527$date
full_data_train_5926527 <- target_5926527_1 %>%filter(date >= "2019-05-13" & date < "2020-03-01")
full_data_test_5926527 <- target_5926527_1 %>%filter(date >= "2020-03-01" & date <= "2020-07-04")
fit_lm_5926527=glm(sold_count~.,data=full_data_train_5926527)
str(fit_lm_5926527)
summary(fit_lm_5926527)
fit_lm_5926527=glm(sold_count~-1+price+favored_count+category_brand_sold,data=full_data_train_5926527,family = "gaussian")
predicted_5926527=predict(fit_lm_5926527,full_data_test_5926527)
part_d_matrix$sold_count_5926527=data.table(full_data_test_5926527[,sold_count])
part_d_matrix$predicted_5926527=data.table(predicted_5926527)
part_d_matrix$predicted_5926527_MAPE=with(full_data_test_5926527,abs((sold_count-predicted_5926527)/sold_count)*100)
is.na(part_d_matrix$predicted_5926527_MAPE)<-sapply(part_d_matrix$predicted_5926527_MAPE, is.infinite)
part_d_matrix$predicted_5926527_MAPE[is.na(part_d_matrix$predicted_5926527_MAPE)]<-mean(complete.cases(part_d_matrix$predicted_5926527_MAPE))
part_d_matrix$predicted_5926527_overallMAPE=with(part_d_matrix,sum(part_d_matrix$predicted_5926527_MAPE)/nrow(part_d_matrix))
sse=sum((full_data_test_5926527$sold_count-predicted_5926527)^2)
mse=sse/nrow(part_d_matrix)
part_d_matrix$date=full_data_test_5926527$date

#3904356
target_3904356=data.table(trendyol[product_content_id==3904356])
target_3904356_1= target_3904356 %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))          
target_3904356_1$date=target_3904356$date
full_data_train_3904356 <- target_3904356_1 %>%filter(date >= "2019-05-13" & date < "2020-03-01")
full_data_test_3904356 <- target_3904356_1 %>%filter(date >= "2020-03-01" & date <= "2020-07-04")
fit_lm_3904356=glm(sold_count~.,data=full_data_train_3904356)
str(fit_lm_3904356)
summary(fit_lm_3904356)
fit_lm_3904356=glm(sold_count~-1+visit_count+favored_count,data=full_data_train_3904356,family = "gaussian")
predicted_3904356=predict(fit_lm_3904356,full_data_test_3904356)
part_d_matrix$sold_count_3904356=data.table(full_data_test_3904356[,sold_count])
part_d_matrix$predicted_3904356=data.table(predicted_3904356)
part_d_matrix$predicted_3904356_MAPE=with(full_data_test_3904356,abs((sold_count-predicted_3904356)/sold_count)*100)
is.na(part_d_matrix$predicted_3904356_MAPE)<-sapply(part_d_matrix$predicted_3904356_MAPE, is.infinite)
part_d_matrix$predicted_3904356_MAPE[is.na(part_d_matrix$predicted_3904356_MAPE)]<-mean(complete.cases(part_d_matrix$predicted_3904356_MAPE))
part_d_matrix$predicted_3904356_overallMAPE=with(part_d_matrix,sum(part_d_matrix$predicted_3904356_MAPE)/nrow(part_d_matrix))
sse=sum((full_data_test_3904356$sold_count-predicted_3904356)^2)
mse=sse/nrow(part_d_matrix)
part_d_matrix$date=full_data_test_3904356$date

#4066298
target_4066298=data.table(trendyol[product_content_id==4066298])
target_4066298_1= target_4066298 %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))          
target_4066298_1$date=target_4066298$date
full_data_train_4066298 <- target_4066298_1 %>%filter(date >= "2019-05-13" & date < "2020-03-01")
full_data_test_4066298 <- target_4066298_1 %>%filter(date >= "2020-03-01" & date <= "2020-07-04")
fit_lm_4066298=glm(sold_count~.,data=full_data_train_4066298)
str(fit_lm_4066298)
summary(fit_lm_4066298)
fit_lm_4066298=glm(sold_count~price+favored_count+category_sold+category_brand_sold+date,data=full_data_train_4066298,family = "gaussian")
predicted_4066298=predict(fit_lm_4066298,full_data_test_4066298)
part_d_matrix$sold_count_4066298=data.table(full_data_test_4066298[,sold_count])
part_d_matrix$predicted_4066298=data.table(predicted_4066298)
part_d_matrix$predicted_4066298_MAPE=with(full_data_test_4066298,abs((sold_count-predicted_4066298)/sold_count)*100)
is.na(part_d_matrix$predicted_4066298_MAPE)<-sapply(part_d_matrix$predicted_4066298_MAPE, is.infinite)
part_d_matrix$predicted_4066298_MAPE[is.na(part_d_matrix$predicted_4066298_MAPE)]<-mean(complete.cases(part_d_matrix$predicted_4066298_MAPE))
part_d_matrix$predicted_4066298_overallMAPE=with(part_d_matrix,sum(part_d_matrix$predicted_4066298_MAPE)/nrow(part_d_matrix))
sse=sum((full_data_test_4066298$sold_count-predicted_4066298)^2)
mse=sse/nrow(part_d_matrix)
part_d_matrix$date=full_data_test_4066298$date

#32939029
target_32939029=data.table(trendyol[product_content_id==32939029])
target_32939029_1= target_32939029 %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))          
target_32939029_1$date=target_32939029$date
full_data_train_32939029 <- target_32939029_1 %>%filter(date >= "2019-05-13" & date < "2020-03-01")
full_data_test_32939029 <- target_32939029_1 %>%filter(date >= "2020-03-01" & date <= "2020-07-04")
fit_lm_32939029=glm(sold_count~.,data=full_data_train_32939029)
str(fit_lm_32939029)
summary(fit_lm_32939029)
fit_lm_32939029=glm(sold_count~price+visit_count+favored_count+category_visits+date,data=full_data_train_32939029,family = "gaussian")
predicted_32939029=predict(fit_lm_32939029,full_data_test_32939029)
part_d_matrix$sold_count_32939029=data.table(full_data_test_32939029[,sold_count])
part_d_matrix$predicted_32939029=data.table(predicted_32939029)
part_d_matrix$predicted_32939029_MAPE=with(full_data_test_32939029,abs((sold_count-predicted_32939029)/sold_count)*100)
is.na(part_d_matrix$predicted_32939029_MAPE)<-sapply(part_d_matrix$predicted_32939029_MAPE, is.infinite)
part_d_matrix$predicted_32939029_MAPE[is.na(part_d_matrix$predicted_32939029_MAPE)]<-mean(complete.cases(part_d_matrix$predicted_32939029_MAPE))
part_d_matrix$predicted_32939029_overallMAPE=with(part_d_matrix,sum(part_d_matrix$predicted_32939029_MAPE)/nrow(part_d_matrix))
sse=sum((full_data_test_32939029$sold_count-predicted_32939029)^2)
mse=sse/nrow(part_d_matrix)
part_d_matrix$date=full_data_test_32939029$date

sonuc=select(part_d_matrix,date,predicted_31515569,predicted_32939029,predicted_3904356,predicted_4066298,predicted_5926527,predicted_6676673,predicted_7061886,predicted_85004)
sonuc_2=select(part_d_matrix,predicted_85004_overallMAPE,predicted_31515569_overallMAPE,predicted_7061886_overallMAPE,predicted_6676673_overallMAPE,predicted_5926527_overallMAPE,predicted_3904356_overallMAPE,predicted_4066298_overallMAPE,predicted_32939029_overallMAPE)
mape_plot = data.table(id= 85004, 31515569, 7061886, 6676673, 5926527, 3904356, 4066298, 32939029)
mape_plot=transpose(mape_plot)
mape_plot$V1=as.character(mape_plot$V1)
Linear_Regression_MAPE=data.table(c("Linear_Regression_MAPE"))
mape_plot$V2=Linear_Regression_MAPE[1,]
mape_plot$V3=transpose(sonuc_2[1,])

ggplot(mape_plot, aes(V1, V3, colour=V2)) +
  geom_point(alpha = 0.5, size=4)+
  theme(axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
        panel.grid.major = element_line(size=0.5),
        panel.grid.minor = element_line(size=0.5),
        text = element_text(size = 15)) +
  labs(title= "MAPE Values for Linear Regression",
       x= "Product ID",
       y= "MAPE") +
  geom_text(aes(label=sprintf("%1.1f", ..y..)),  position=position_nudge(y=4))