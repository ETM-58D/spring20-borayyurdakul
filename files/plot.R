library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readxl)

plot <- read_excel("/Users/ayhanertuglu/Desktop/Okul/II.Dönem/Business Analytics/Project/Data/Boun Content Challenge-2.xlsx", 
                                       sheet = "Daily Actions")

plot=data.table(plot)
plot$price=as.numeric(plot$price)
plot[,date:=as.Date(event_date,'%Y-%m-%d')]
plot <- plot[,-3]
str(plot)
#To change weekly and monthly graphs, you can change the filter dates
#85004
target_85004=data.table(plot[product_content_id==85004])
target_85004_1= target_85004 %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))
target_85004_1$date=target_85004$date
target_85004_1 <- target_85004_1 %>%filter(date >= "2020-06-01" & date <= "2020-06-27")

ggplot(target_85004_1, aes(day, sold_count)) +
  geom_boxplot(alpha = 0.5, size=1) +
  stat_summary(geom="text", fun=quantile, colour = "dark grey",
               aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(y=0.5), size=3.5) +
  stat_summary(geom="text", fun=mean, colour = "black",
               aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(y=0.5), size=3.5)+
  labs(title= "Product ID:85004",
       x= "Day",
       y= "Sold Count")

#31515569
target_31515569=data.table(plot[product_content_id==31515569])
target_31515569_1= target_31515569 %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))
target_31515569_1$date=target_31515569$date
target_31515569_1 <- target_31515569_1 %>%filter(date >= "2020-06-01" & date <= "2020-06-27")

ggplot(target_31515569_1, aes(day, sold_count)) +
  geom_boxplot(alpha = 0.5, size=1) +
  stat_summary(geom="text", fun=quantile, colour = "dark grey",
               aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(y=0.5), size=3.5) +
  stat_summary(geom="text", fun=mean, colour = "black",
               aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(y=0.5), size=3.5) +
  labs(title= "Product ID: 31515569",
       x= "Day",
       y= "Sold Count")

#32939029
target_32939029=data.table(plot[product_content_id==32939029])
target_32939029_1= target_32939029 %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))
target_32939029_1$date=target_32939029$date
target_32939029_1 <- target_32939029_1 %>%filter(date >= "2020-06-01" & date <= "2020-06-27")

ggplot(target_32939029_1, aes(day, sold_count)) +
  geom_boxplot(alpha = 0.5, size=1) +
  stat_summary(geom="text", fun=quantile, colour = "dark grey",
               aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(y=0.5), size=3.5) +
  stat_summary(geom="text", fun=mean, colour = "black",
               aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(y=0.5), size=3.5) +
  labs(title= "Product ID: 32939029",
       x= "Day",
       y= "Sold Count")

#3904356
target_3904356=data.table(plot[product_content_id==3904356])
target_3904356_1= target_3904356 %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))
target_3904356_1$date=target_3904356$date
target_3904356_1 <- target_3904356_1 %>%filter(date >= "2020-06-01" & date <= "2020-06-27")

ggplot(target_3904356_1, aes(day, sold_count)) +
  geom_boxplot(alpha = 0.5, size=1) +
  stat_summary(geom="text", fun=quantile, colour = "dark grey",
               aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(y=0.5), size=3.5) +
  stat_summary(geom="text", fun=mean, colour = "black",
               aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(y=0.5), size=3.5) +
  labs(title= "Product ID: 3904356",
       x= "Day",
       y= "Sold Count")

#4066298
target_4066298=data.table(plot[product_content_id==4066298])
target_4066298_1= target_4066298 %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))
target_4066298_1$date=target_4066298$date
target_4066298_1 <- target_4066298_1 %>%filter(date >= "2020-06-01" & date <= "2020-06-27")

ggplot(target_4066298_1, aes(day, sold_count)) +
  geom_boxplot(alpha = 0.5, size=1) +
  stat_summary(geom="text", fun=quantile, colour = "dark grey",
               aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(y=0.5), size=3.5) +
  stat_summary(geom="text", fun=mean, colour = "black",
               aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(y=0.5), size=3.5) +
  labs(title= "Product ID: 4066298",
       x= "Day",
       y= "Sold Count")

#5926527
target_5926527=data.table(plot[product_content_id==5926527])
target_5926527_1= target_5926527 %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))
target_5926527_1$date=target_5926527$date
target_5926527_1 <- target_5926527_1 %>%filter(date >= "2020-06-01" & date <= "2020-06-27")

ggplot(target_5926527_1, aes(day, sold_count)) +
  geom_boxplot(alpha = 0.5, size=1) +
  stat_summary(geom="text", fun=quantile, colour = "dark grey",
               aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(y=0.5), size=3.5) +
  stat_summary(geom="text", fun=mean, colour = "black",
               aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(y=0.5), size=3.5) +
  labs(title= "Product ID: 5926527",
       x= "Day",
       y= "Sold Count")

#6676673
target_6676673=data.table(plot[product_content_id==6676673])
target_6676673_1= target_6676673 %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))
target_6676673_1$date=target_6676673$date
target_6676673_1 <- target_6676673_1 %>%filter(date >= "2020-06-01" & date <= "2020-06-27")

ggplot(target_6676673_1, aes(day, sold_count)) +
  geom_boxplot(alpha = 0.5, size=1) +
  stat_summary(geom="text", fun=quantile, colour = "dark grey",
               aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(y=0.5), size=3.5) +
  stat_summary(geom="text", fun=mean, colour = "black",
               aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(y=0.5), size=3.5) +
  labs(title= "Product ID: 6676673",
       x= "Day",
       y= "Sold Count")

#7061886
target_7061886=data.table(plot[product_content_id==7061886])
target_7061886_1= target_7061886 %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))
target_7061886_1$date=target_7061886$date
target_7061886_1 <- target_7061886_1 %>%filter(date >= "2020-06-01" & date <= "2020-06-27")

ggplot(target_7061886_1, aes(day, sold_count)) +
  geom_boxplot(alpha = 0.5, size=1) +
  stat_summary(geom="text", fun=quantile, colour = "dark grey",
               aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(y=0.5), size=3.5) +
  stat_summary(geom="text", fun=mean, colour = "black",
               aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(y=0.5), size=3.5) +
  labs(title= "Product ID: 7061886",
       x= "Day",
       y= "Sold Count")