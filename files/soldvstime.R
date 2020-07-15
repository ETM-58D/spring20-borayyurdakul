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
#31515569
target_31515569=data.table(trendyol[product_content_id==31515569])
#7061886
target_7061886=data.table(trendyol[product_content_id==7061886])
#6676673
target_6676673=data.table(trendyol[product_content_id==6676673])
#5926527
target_5926527=data.table(trendyol[product_content_id==5926527])
#3904356
target_3904356=data.table(trendyol[product_content_id==3904356])
#4066298
target_4066298=data.table(trendyol[product_content_id==4066298])
#32939029
target_32939029=data.table(trendyol[product_content_id==32939029])


ggplot(target_31515569, aes(date, sold_count)) +
  geom_line(size=1.5) +
  theme(axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
        panel.grid.major = element_line(size=0.5),
        panel.grid.minor = element_line(size=0.5),
        text = element_text(size = 15)) +
  labs(title= "Sold Count of 31515569 over Time",
       x= "Date",
       y= "Sold Count")

ggplot(target_32939029, aes(date, sold_count)) +
  geom_line(size=1.5) +
  theme(axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
        panel.grid.major = element_line(size=0.5),
        panel.grid.minor = element_line(size=0.5),
        text = element_text(size = 15)) +
  labs(title= "Sold Count of 32939023 over Time",
       x= "Date",
       y= "Sold Count")

ggplot(target_3904356, aes(date, sold_count)) +
  geom_line(size=1.5) +
  theme(axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
        panel.grid.major = element_line(size=0.5),
        panel.grid.minor = element_line(size=0.5),
        text = element_text(size = 15)) +
  labs(title= "Sold Count of 3904356 over Time",
       x= "Date",
       y= "Sold Count")

ggplot(target_4066298, aes(date, sold_count)) +
  geom_line(size=1.5) +
  theme(axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
        panel.grid.major = element_line(size=0.5),
        panel.grid.minor = element_line(size=0.5),
        text = element_text(size = 15)) +
  labs(title= "Sold Count of 4066298 over Time",
       x= "Date",
       y= "Sold Count")

ggplot(target_5926527, aes(date, sold_count)) +
  geom_line(size=1.5) +
  theme(axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
        panel.grid.major = element_line(size=0.5),
        panel.grid.minor = element_line(size=0.5),
        text = element_text(size = 15)) +
  labs(title= "Sold Count of 5926527 over Time",
       x= "Date",
       y= "Sold Count")

ggplot(target_6676673, aes(date, sold_count)) +
  geom_line(size=1.5) +
  theme(axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
        panel.grid.major = element_line(size=0.5),
        panel.grid.minor = element_line(size=0.5),
        text = element_text(size = 15)) +
  labs(title= "Sold Count of 6676673 over Time",
       x= "Date",
       y= "Sold Count")

ggplot(target_7061886, aes(date, sold_count)) +
  geom_line(size=1.5) +
  theme(axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
        panel.grid.major = element_line(size=0.5),
        panel.grid.minor = element_line(size=0.5),
        text = element_text(size = 15)) +
  labs(title= "Sold Count of 7061886 over Time",
       x= "Date",
       y= "Sold Count")

ggplot(target_85004, aes(date, sold_count)) +
  geom_line(size=1.5) +
  theme(axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
        panel.grid.major = element_line(size=0.5),
        panel.grid.minor = element_line(size=0.5),
        text = element_text(size = 15)) +
  labs(title= "Sold Count of 85004 over Time",
       x= "Date",
       y= "Sold Count")