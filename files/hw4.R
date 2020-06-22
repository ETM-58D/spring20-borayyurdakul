library(shiny)
library(shinydashboard)
library(DT)
library(shiny)
library(shinydashboard)
library(data.table)
library(dplyr)
library(ggplot2)
library(quantmod)
library(reshape2)

setwd("~/Documents/school/bogazici/ETM58d/hw4/app")

data=fread("ETM58D_Spring20 HW 4_electricity_load_Turkey.csv")
data=data.table(data)
data$Date =  as.Date(data$Date, format = "%Y-%m-%d")
data$Average_Temperature=with(data,(T_1+T_2+T_3+T_4+T_5+T_6+T_7)/7)
data$Average_Temperature=sprintf("%1.2f°C", data$Average_Temperature)
data = subset(data, select = -c(4:10) )
str(data)

#codes for question 2
data_2=fread("ETM58D_Spring20 HW 4_electricity_load_Turkey.csv")

#codes for question 3
full_data=fread("ETM58D_Spring20 HW 4_electricity_load_Turkey.csv")
full_data_1=fread("ETM58D_Spring20 HW 4_electricity_load_Turkey.csv")
full_data=data.table(full_data)
full_data[,Date:=as.Date(Date,'%Y-%m-%d')]
full_data=full_data[,list(Date,Hour,Consumption)]
head(full_data)
full_data[,lag_168:=shift(Consumption,168)]
full_data[,lag_48:=shift(Consumption,48)]
full_data= full_data %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))
full_data$Date=full_data_1$Date
full_data_train <- full_data %>%filter(Date >= "2017-01-01" & Date < "2019-03-01")
full_data_test <- full_data %>%filter(Date >= "2019-03-01" & Date <= "2019-12-29")
str(full_data)
fit_lr=lm(Consumption~+lag_168+lag_48,full_data_train)
summary(fit_lr)
pred.full_data_test <- predict(fit_lr,full_data_test)
pred.full_data_test
pred.full_data_test <- as.data.table(pred.full_data_test)
full_data_test$predicted <- pred.full_data_test
full_data_test$predicted_APE=with(full_data_test,abs((Consumption-predicted)/Consumption)*100)
full_data_test$MAPE_b=with(full_data_test,sum(full_data_test$predicted_APE)/nrow(full_data_test))
mse_b=sum(full_data_test$predicted_MAPE)/nrow(full_data_test)
pred.full_data <- predict(fit_lr,full_data)
tahmin=data.table(pred.full_data)
tahmin$Hour=full_data$Hour
tahmin[,forecast:=lead(pred.full_data,48)]
full_data$Forecast=tahmin$forecast
full_data=full_data[,-3]
full_data=full_data[,-3]
full_data=full_data[,-3]
filtered_full_data=full_data

ui <- navbarPage("ETM 58d / Homework 4 / Group 4",
                  tabPanel("Question 1",
                           sidebarLayout(
                             sidebarPanel(
                               dateRangeInput('dateRange',min=as.Date('2017-01-01'),max = as.Date('2019-12-29'),
                                              label = 'Filter consumption by date',
                                              start = as.Date('2017-01-01'), 
                                              end = as.Date('2019-12-29')
                             )),
                             mainPanel(
                               DT::dataTableOutput('table')
                             ))),
                  tabPanel("Question 2",
                           sidebarLayout(
                             sidebarPanel(
                               dateRangeInput('dateRange_2',min=as.Date('2017-01-01'),max = as.Date('2019-12-29'),
                                              label = 'Filter consumption by date',
                                              start = as.Date('2017-01-01'), 
                                              end = as.Date('2019-12-29')
                               )),
                             mainPanel(
                               plotOutput('my_plot_1'),
                               plotOutput('my_plot_2')
                             ))),
                  tabPanel("Question 3",
                          sidebarLayout(
                            sidebarPanel(
                              dateRangeInput('dateRange_3',min=as.Date('2017-01-01'),max = as.Date('2019-12-29'),
                                             label = 'Filter consumption by date',
                                             start = as.Date('2017-01-01'), 
                                             end = as.Date('2019-12-27')
                              )),
                            mainPanel(
                              plotOutput('my_plot_3')
                            )))
                  )
                  
server <- function(input, output, session) {
    filtered_data <- reactive({data %>% filter(Date >= input$dateRange[1] & Date <= input$dateRange[2])})
    
    output$table  <- DT::renderDataTable({DT::datatable(filtered_data())
      })

    filtered_data_2 <- reactive({data_2 %>% filter(Date >= input$dateRange_2[1] & Date <= input$dateRange_2[2])})
    Hourly <- reactive({aggregate(Consumption ~ Hour, filtered_data_2(), mean)})
    
    output$my_plot_1 <-  renderPlot({ggplot(Hourly(), aes(Hour, Consumption)) +
        geom_line() +
        theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
        theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
        scale_x_continuous(breaks=seq(0, 24, by = 1)) + 
        labs(title= "Hourly Average Consumption Values",
             x= "Hour",
             y= "Average Consumption")
      
    })
    
    T_1_Average <- reactive({aggregate(T_1 ~ Hour, filtered_data_2(), mean)})
    T_2_Average <- reactive({aggregate(T_2 ~ Hour, filtered_data_2(), mean)})
    T_3_Average <- reactive({aggregate(T_3 ~ Hour, filtered_data_2(), mean)})
    T_4_Average <- reactive({aggregate(T_4 ~ Hour, filtered_data_2(), mean)})
    T_5_Average <- reactive({aggregate(T_5 ~ Hour, filtered_data_2(), mean)})
    T_6_Average <- reactive({aggregate(T_6 ~ Hour, filtered_data_2(), mean)})
    T_7_Average <- reactive({aggregate(T_7 ~ Hour, filtered_data_2(), mean)})
    
    joined=reactive({merge(T_1_Average(),T_2_Average(),by='Hour')})
    joined_1=reactive({merge(joined(),T_3_Average(),by='Hour')})
    joined_2=reactive({merge(joined_1(),T_4_Average(),by='Hour')})
    joined_3=reactive({merge(joined_2(),T_5_Average(),by='Hour')})
    joined_4=reactive({merge(joined_3(),T_6_Average(),by='Hour')})
    joined_5=reactive({merge(joined_4(),T_7_Average(),by='Hour')})
    
    plot_long <- reactive({reshape2::melt(joined_5(), id = "Hour", measure = c("T_1", "T_2", "T_3", "T_4", "T_5", "T_6", "T_7"))})
    
    output$my_plot_2 <-  renderPlot({ggplot(plot_long(), aes(Hour, value, colour=variable)) +
        geom_point(size=3)+
        theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
        theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
        scale_x_continuous(breaks=seq(0, 24, by = 1)) + 
        labs(title= "Hourly Average Temperature Values by Cities",
             x= "Hour",
             y= "Average temperature")
      })
    
    Forecast_data <- reactive({full_data %>% filter(Date==input$dateRange_3[2])})
    
    output$my_plot_3 <-  renderPlot({ggplot(Forecast_data(), aes(Hour, Forecast)) +
        geom_line() +
        theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
        theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
        scale_x_continuous(breaks=seq(0, 24, by = 1)) + 
        labs(title= "Hourly Forecast Values",
             x= "Hour",
             y= "Forecast")
    })
}

shinyApp(ui = ui, server = server)