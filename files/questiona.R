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